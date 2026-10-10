;;; pos-salvage.el --- Keep the open tasks of what is retired  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chris Gough

;; Author: Chris Gough
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A document that leaves canon may hold tasks that are still open.
;; Once it is sealed nothing reads them.  Salvage is run on the
;; material before its seal is planned: each open task is copied to
;; the intray, and the original is closed where it stands, with a note
;; that cites the copy.
;;
;; The copy cites the original by a file link.  The relink plan of
;; the seal, pos-relink.el, rewrites that link to the sealed item's
;; ipfs:// link, as it does every link into the item.  The original
;; cites the copy by an id link, which sealing resolves as it does
;; any id link to canon.
;;
;; A task's ID passes to the copy, so that a link to the task by its
;; ID still leads to the task.
;;
;; - `pos-salvage-open-tasks': the open tasks of an item.
;; - `pos-salvage': command.
;; - `pos-salvage-report': what a salvage did, as text.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'seq)
(require 'pos)
(require 'pos-corpus)

(defcustom pos-salvage-closed-state "CANCELLED"
  "The state a salvaged task is left in where it stood."
  :type 'string
  :group 'pos)

(defun pos-salvage--files (source)
  "Return the Org files of SOURCE, a file or a directory, sorted.
Those a corpus would read by name: not a hidden file, a lock file or
a symbolic link, nor a file in a hidden directory."
  (if (file-directory-p source)
      (sort (seq-remove
             #'file-symlink-p
             (directory-files-recursively
              source pos-corpus--org-regexp nil
              (lambda (dir)
                (not (string-prefix-p "." (file-name-nondirectory dir))))))
            #'string<)
    (and (pos-corpus--org-file-p (file-name-nondirectory source))
         (not (file-symlink-p source))
         (list source))))

(defun pos-salvage--open-p ()
  "Return non-nil if the heading at point is an open task."
  (and (org-get-todo-state) (not (org-entry-is-done-p))))

(defun pos-salvage--map-open (file function)
  "Call FUNCTION at each topmost open task of FILE and collect the results.
An open task beneath an open task is its parent's."
  (let (results)
    (with-current-buffer (pos-visit file)
      (org-with-wide-buffer
       (let (markers)
         (org-map-entries
          (lambda ()
            (when (pos-salvage--open-p)
              ;; What FUNCTION inserts at the end of one task is
              ;; before the next, not within it.
              (push (copy-marker (point) t) markers)
              (setq org-map-continue-from
                    (save-excursion (org-end-of-subtree t) (point)))))
          nil 'file)
         (dolist (marker (nreverse markers))
           (goto-char marker)
           (push (funcall function) results)
           (set-marker marker nil)))))
    (nreverse results)))

(defun pos-salvage-open-tasks (source)
  "Return (FILE LINE STATE TITLE) for each open task of SOURCE.
SOURCE is a file or a directory.  The topmost of a nest only."
  (mapcan (lambda (file)
            (pos-salvage--map-open
             file
             (lambda ()
               (list file (line-number-at-pos) (org-get-todo-state)
                     (substring-no-properties (org-get-heading t t t t))))))
          (pos-salvage--files source)))

(defun pos-salvage--close-subtree ()
  "Close each open task of the subtree at point and take its ID away.
A task is closed by writing `pos-salvage-closed-state' for its
keyword; nothing is logged and no repeater is followed."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t) (point-marker)))
          (case-fold-search nil))
      (while (progn
               (when (pos-salvage--open-p)
                 (save-excursion
                   (beginning-of-line)
                   (when (looking-at org-complex-heading-regexp)
                     (replace-match pos-salvage-closed-state t t nil 2))))
               (when (org-entry-get nil "ID")
                 (org-entry-delete nil "ID"))
               (and (outline-next-heading) (< (point) end))))
      (set-marker end nil))))

(defun pos-salvage--note (text)
  "Insert TEXT as a line after the planning and drawers of the entry at point."
  (save-excursion
    (org-end-of-meta-data t)
    (unless (bolp) (insert "\n"))
    (insert text "\n")))

(defun pos-salvage--search (title)
  "Return the search option for the heading at point, titled TITLE.
Its CUSTOM_ID if it has one, else its title.  A title that holds a
link cannot be written inside a link, and has no search option: the
file alone is cited."
  (let ((custom (org-entry-get nil "CUSTOM_ID")))
    (cond (custom (concat "::#" custom))
          ((string-match-p "\\[\\[\\|\\]" title) "")
          (t (concat "::*" title)))))

(defun pos-salvage--one (root file intray)
  "Salvage the open task at point in FILE into the buffer INTRAY.
ROOT is the directory of the intray."
  (let* ((title (substring-no-properties (org-get-heading t t t t)))
         (id (or (org-entry-get nil "ID") (org-id-new)))
         (from (file-relative-name file root))
         (search (pos-salvage--search title))
         (subtree (buffer-substring-no-properties
                   (point) (save-excursion (org-end-of-subtree t t) (point)))))
    (with-current-buffer intray
      (org-with-wide-buffer
       (pos-goto-unsorted-end)
       (let ((start (point)))
         (org-paste-subtree 2 subtree)
         (goto-char start)
         (org-entry-put (point) "ID" id)
         (pos-salvage--note
          (format "Salvaged from %s; nobody knew where it belonged."
                  (org-link-make-string (concat "file:" from search) from))))))
    (pos-salvage--close-subtree)
    (pos-salvage--note
     (format "Salvaged to the intray: %s"
             (org-link-make-string (concat "id:" id)
                                   (org-link-display-format title))))))

(defun pos-salvage (root source &optional dry-run)
  "Copy each open task of SOURCE to ROOT's intray and close the original.
SOURCE is a file or a directory beneath ROOT, about to be sealed.
Each topmost open task of its Org files is copied, with what is
beneath it, to the end of Unsorted in ROOT's intray.org.  The copy
keeps the task's state and takes its ID, or a new one if it had
none, and says where it was salvaged from, by a file link.  The
original is left in `pos-salvage-closed-state', as is each open task
beneath it, without its ID, and says where the copy is, by an id
link.  With DRY-RUN nothing is written.  Return (FILE LINE STATE
TITLE) for each task, as it stood.  Refuse a SOURCE that is not
beneath ROOT or holds the intray, an intray that is not a regular
file or that an editor holds, and a file whose buffer has unsaved
changes or is older than the file."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (source (expand-file-name source))
         (intray-file (expand-file-name "intray.org" root))
         (files (pos-salvage--files source))
         (make-backup-files nil)
         (auto-save-default nil)
         (enable-local-variables nil)
         (enable-local-eval nil))
    (unless (and (file-exists-p source)
                 (string-prefix-p root (file-name-as-directory source))
                 (not (equal (file-name-as-directory source) root)))
      (user-error "Salvage needs a file or directory beneath the root: %s" source))
    (when (member intray-file files)
      (user-error "The item holds the intray: %s" source))
    (let ((tasks (pos-salvage-open-tasks source)))
      (unless (or dry-run (not tasks))
	(unless (and (file-regular-p intray-file) (not (file-symlink-p intray-file)))
          (user-error "Intray must be an existing regular file, not a symlink"))
	(dolist (file (cons intray-file files))
          (when (stringp (file-locked-p file))
            (user-error "%s is locked by another editor; save it there first" file))
          (let ((buffer (get-file-buffer file)))
            (when (and buffer (buffer-modified-p buffer))
              (user-error "Save the modified buffer of %s first" file))
            (when (and buffer (not (verify-visited-file-modtime buffer)))
              (user-error "%s changed on disk; revert its buffer first" file))))
	(let ((intray (pos-visit intray-file)))
          (dolist (file (delete-dups (mapcar #'car tasks)))
            (pos-salvage--map-open
             file (lambda () (pos-salvage--one root file intray)))
            (with-current-buffer (pos-visit file) (save-buffer)))
          (with-current-buffer intray (save-buffer))))
      tasks)))

(defun pos-salvage-report (root tasks dry-run)
  "Return a report of TASKS, as `pos-salvage' gives them, relative to ROOT.
DRY-RUN words it as what would be done."
  (concat
   (format "%s %d open task%s to intray.org\n"
           (if dry-run "Would salvage" "Salvaged")
           (length tasks) (if (= (length tasks) 1) "" "s"))
   (mapconcat (pcase-lambda (`(,file ,line ,state ,title))
                (format "%s:%d: %s %s\n"
                        (file-relative-name file root) line state title))
              tasks "")))

(provide 'pos-salvage)
;;; pos-salvage.el ends here
