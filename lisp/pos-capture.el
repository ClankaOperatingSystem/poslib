;;; pos-capture.el --- Capture a task into an intray -*- lexical-binding: t; -*-

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

;; - `pos-capture': command.
;; - `pos-capture-like': the open items whose titles are like a title.
;; - `pos-capture-batch': shell entry; reads POS_CAPTURE_ROOT,
;;   POS_CAPTURE_TITLE, POS_CAPTURE_BODY and POS_CAPTURE_CHECK.

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'pos)

(defcustom pos-capture-like-share 0.6
  "The share of words two titles have in common that makes them like.
The words both have, over the words either has; 1.0 is the same words."
  :type 'float
  :group 'pos)

(defun pos-capture--words (title)
  "Return the words of TITLE that tell it from another, lower-cased.
A word is a run of letters and digits of more than three characters."
  (seq-uniq
   (seq-filter (lambda (word) (> (length word) 3))
               (split-string (downcase title) "[^[:alnum:]]+" t))))

(defun pos-capture-like (root title)
  "Return (FILE LINE STATE TITLE) for each open item of ROOT like TITLE.
The items are those of the corpus of ROOT in a state that is not
done.  Two titles are like when the words they have in common are at
least `pos-capture-like-share' of the words either has, by
`pos-capture--words'.  A title with no such word is like none."
  (let ((words (pos-capture--words title))
        like)
    (when words
      (dolist (file (pos-files root))
        (with-current-buffer (pos-visit file)
          (org-map-entries
           (lambda ()
             (when (and (org-get-todo-state) (not (org-entry-is-done-p)))
               (let* ((other (substring-no-properties
                              (org-get-heading t t t t)))
                      (theirs (pos-capture--words other))
                      (both (seq-intersection words theirs)))
                 (when (and both
                            (>= (/ (float (length both))
                                   (length (seq-union words theirs)))
                                pos-capture-like-share))
                   (push (list file (line-number-at-pos)
                               (substring-no-properties (org-get-todo-state))
                               other)
                         like)))))
           nil 'file))))
    (nreverse like)))

(defun pos-capture (root title &optional body)
  "File TITLE as a TODO under Unsorted in ROOT's intray; return its line.
The item has an ID property, a new Org ID, and a CREATED property, the
time of capture as an inactive timestamp.  BODY, if it is not nil or
blank, is the item's text, written after its properties.  Refuse bad
titles, a BODY with a line that begins with a star, which Org would
read as a heading, symlinks, unsaved or stale buffers and foreign
locks."
  (interactive "DRoot: \nsTask: ")
  (unless (and (stringp title)
               (not (string-empty-p (string-trim title)))
               (not (string-match-p "[[:cntrl:]]" title)))
    (user-error "Capture needs a nonempty, single-line title"))
  (when (and body (string-empty-p (string-trim body)))
    (setq body nil))
  (when body
    (when (string-match-p "^\\*" body)
      (user-error "A line of the body begins with a star"))
    (when (string-match-p "[^[:print:]\n\t]" body)
      (user-error "The body has a control character")))
  (let ((file (expand-file-name "intray.org" root))
        (enable-local-variables nil)
        (enable-local-eval nil)
        (make-backup-files nil)
        (auto-save-default nil)
        (create-lockfiles t))
    (unless (and (file-regular-p file) (not (file-symlink-p file)))
      (user-error "Intray must be an existing regular file, not a symlink"))
    (when (stringp (file-locked-p file))
      (user-error "Intray is locked by another editor; save it there first"))
    (with-current-buffer (pos-visit file)
      (when (buffer-modified-p)
        (user-error "Save the intray's modified buffer before capturing"))
      (unless (verify-visited-file-modtime (current-buffer))
        (user-error "Intray changed on disk; revert its buffer first"))
      (save-restriction
        (widen)
        (save-excursion
          (unwind-protect
              (progn
                (lock-buffer)
                (atomic-change-group
                  (pos-goto-unsorted-end)
                  (let ((line (line-number-at-pos)))
                    (insert "** TODO " (string-trim title) "\n")
                    (forward-line -1)
                    ;; Not `org-id-get-create', which also writes the
                    ;; ID to `org-id-locations-file'.
                    (org-entry-put (point) "ID" (pos-new-id))
                    (org-entry-put (point) "CREATED"
                                   (format-time-string
                                    (org-time-stamp-format t t)))
                    (when body
                      (org-end-of-meta-data t)
                      (unless (bolp) (insert "\n"))
                      (insert (string-trim body) "\n"))
                    (save-buffer)
                    line)))
            (unlock-buffer)))))))

(defun pos-capture-batch ()
  "Capture POS_CAPTURE_TITLE into POS_CAPTURE_ROOT's intray.
POS_CAPTURE_BODY, if set, is the item's text.  Print the item's line,
then \"Like: FILE:LINE: STATE TITLE\" for each of `pos-capture-like'
found before the item was written.  With POS_CAPTURE_CHECK set and
not empty, write nothing and print those lines alone."
  (let* ((root (getenv "POS_CAPTURE_ROOT"))
         (title (getenv "POS_CAPTURE_TITLE"))
         (check (not (member (getenv "POS_CAPTURE_CHECK") '(nil ""))))
         (like (pos-capture-like root (or title ""))))
    (unless check
      (princ (format "%s:%d: TODO %s\n"
                     (expand-file-name "intray.org" root)
                     (pos-capture root title (getenv "POS_CAPTURE_BODY"))
                     (string-trim title))))
    (pcase-dolist (`(,file ,line ,state ,other) like)
      (princ (format "Like: %s:%d: %s %s\n" file line state other)))))

(provide 'pos-capture)
;;; pos-capture.el ends here
