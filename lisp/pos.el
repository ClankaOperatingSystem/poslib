;;; pos.el --- Personal OS tools for an Org repository  -*- lexical-binding: t -*-
;; Copyright (C) 2026 Chris Gough
;; Author: Chris Gough
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.6") (yaml "1.2.4") (org-roam "2.3.1"))
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
;; What the commands over a tree's Org files share: the root and its
;; settings, the one TODO sequence, and the files, which are the
;; corpus of pos-corpus.el, every Org file the tree's configurations
;; allow.  A command writes only the files of the root's own
;; repository.  The commands are in files of their own: pos-lint.el,
;; pos-dedupe.el, pos-refile.el, pos-sweep.el and the others.
;;
;; - `pos-directory', `pos-load-config': the root and its settings.
;; - `pos-files': the Org files of a tree.
;; - `pos-todo-keywords', `pos-visit': the TODO sequence, and a buffer
;;   visiting a file with it in force.
;; - `pos-normalise-keywords': remove #+TODO lines from the files.
;; - `pos-map-headings', `pos-marker-at', `pos-open-descendant-p': the
;;   headings of a file, one of them by its line, and whether one has
;;   an open task below it.
;; - `pos-goto-unsorted-end': the place in the intray for a new item.
;;; Code:
(require 'org)
(require 'org-archive)
(require 'org-id)
(require 'seq)
(require 'cl-lib)
(require 'pos-roam)
(require 'pos-corpus)

(defgroup pos nil
  "A personal operating system in Org files."
  :group 'org
  :prefix "pos-")

;;;; Files and paths

(defun pos-new-id ()
  "Return a new ID for an item or a file, in lower case.
Org takes a UUID from the system's uuidgen, which prints upper case on
some systems and lower case on others."
  (downcase (org-id-new)))

(defun pos-files (root &optional writable)
  "Return the Org files of the tree at ROOT: its corpus, sorted.
With WRITABLE, only those a command of ROOT may write: the files of
the root's own repository, not those of a repository mounted within
it, which that repository's own configuration governs."
  (let ((corpus (pos-corpus root)))
    (if writable
        (seq-filter (lambda (file) (pos-corpus-writable-p corpus file))
                    (pos-corpus-files corpus))
      (pos-corpus-files corpus))))

;;;; Keywords

;; One sequence for every file; per-file #+TODO lines drift.
(defconst pos-todo-keywords
  '((sequence "TODO" "NEXT" "WAITING" "SOMEDAY"
              "|" "DONE" "CANCELLED"))
  "The TODO sequence, shaped as `org-todo-keywords'.")

(defun pos-done-keywords ()
  "Return the done keywords of `pos-todo-keywords'."
  (cdr (member "|" (cdr (car pos-todo-keywords)))))

(defvar pos-visit-corpus nil
  "The corpus whose files are being visited, or nil.
A command that reads a corpus binds it, so that `pos-visit' knows
which files the root may not write.")

(defun pos-visit-own-keywords-p (file)
  "Return non-nil if the keywords FILE was written with stand.
A file of `pos-visit-corpus' that the root may not write: one of
another repository of the tree, or of a product."
  (and pos-visit-corpus
       (pos-corpus-owner pos-visit-corpus file)
       (not (pos-corpus-writable-p pos-visit-corpus file))))

(defun pos-visit (file)
  "Return a buffer visiting FILE, with the one TODO sequence in force.
Org reads `org-todo-keywords' when a buffer enters Org mode, so the
sequence is bound for that moment and the user's own setting is left
as it is.  A buffer already visiting FILE is returned as it is.  A
file of `pos-visit-corpus' that the root may not write is visited
with no sequence bound: its own #+TODO line stands, and with none,
the user's setting."
  (if (pos-visit-own-keywords-p file)
      (find-file-noselect file)
    (let ((org-todo-keywords pos-todo-keywords))
      (find-file-noselect file))))

;;;; The root and its settings

;; Default: parent of lisp/.
(defcustom pos-directory
  (expand-file-name ".." (file-name-directory (or load-file-name
                                                  buffer-file-name
                                                  default-directory)))
  "Root of the repository the tools operate on."
  :type 'directory
  :group 'pos)

(defcustom pos-config-file "pos-config.el"
  "Per-repository configuration, relative to `pos-directory'."
  :type 'string
  :group 'pos)

(defconst pos-retired-settings
  '((pos-pillars . "the files are every Org file the configuration allows")
    (pos-startup-excluded-directories
     . "the directories not read are declared under exclude in the configuration")
    (pos-archive-directory
     . "where a scope's done items go is declared by its archive entry's sweep and path")
    (pos-roam-excluded-directories
     . "the index covers the corpus, whose exclusions are declared under exclude in the configuration")
    (pos-prose-directories
     . "no scan reads outside the corpus; the open tasks of what is retired are salvaged before it is sealed"))
  "Settings pos-config.el once set, each with what replaced it.")

(defun pos-load-config (&optional root)
  "Load the `pos-config-file' of ROOT, default `pos-directory', if present.
Return non-nil if loaded.  A retired setting the file still sets is
reported, and ignored."
  (prog1 (load (expand-file-name pos-config-file (or root pos-directory)) t t t)
    (pcase-dolist (`(,setting . ,replacement) pos-retired-settings)
      (when (boundp setting)
        (message "%s sets %s, which is no longer read: %s"
                 pos-config-file setting replacement)))))

;;;; Normalising keywords

(defun pos-next-todo-line ()
  "Move past the next #+TODO line in force; return non-nil if there is one.
A line is in force where Org reads it as a keyword.  One inside an
example or a source block is text the document shows, and is passed
over."
  (let ((case-fold-search t) found)
    (while (and (not found)
                (re-search-forward "^#\\+\\(SEQ_\\|TYP_\\)?TODO:" nil t))
      (setq found (save-excursion
                    (beginning-of-line)
                    (eq 'keyword (org-element-type (org-element-at-point))))))
    found))

(defun pos-normalise-keywords-in-file (file)
  "Remove #+TODO lines from FILE; respell CANCELED as CANCELLED.
A #+TODO line is one `pos-next-todo-line' finds.  Return
\(:lines-removed N :respelled N)."
  (let ((lines-removed 0)
        (respelled 0)
        (make-backup-files nil))
    (with-current-buffer (pos-visit file)
      (save-excursion
        (goto-char (point-min))
        (while (pos-next-todo-line)
          (delete-region (line-beginning-position) (line-beginning-position 2))
          (setq lines-removed (1+ lines-removed)))
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward "^\\*+ \\(CANCELED\\)\\b" nil t)
            (replace-match "CANCELLED" t t nil 1)
            (setq respelled (1+ respelled)))))
      (when (buffer-modified-p)
        (save-buffer)))
    (list :lines-removed lines-removed :respelled respelled)))

(defun pos-normalise-keywords ()
  "Run `pos-normalise-keywords-in-file' on the tree's files; return totals.
The files are those of the corpus a command may write."
  (interactive)
  (let ((lines-removed 0)
        (respelled 0))
    (dolist (file (pos-files (file-name-as-directory pos-directory) t))
      (let ((result (pos-normalise-keywords-in-file file)))
        (setq lines-removed (+ lines-removed (plist-get result :lines-removed))
              respelled (+ respelled (plist-get result :respelled)))))
    (message "Normalised keywords: %d #+TODO lines removed, %d CANCELED respelled"
             lines-removed respelled)
    (list :lines-removed lines-removed :respelled respelled)))

;;;; Headings

(defun pos-open-descendant-p ()
  "Return non-nil if the entry at point has an open TODO below it."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil))
      (re-search-forward org-not-done-heading-regexp end t))))

(defun pos-map-headings (file function)
  "Call FUNCTION at each heading of FILE; collect non-nil results."
  (with-current-buffer (pos-visit file)
    (delq nil (org-map-entries function nil 'file))))

(defun pos-marker-still-at-p (marker key)
  "Return non-nil if MARKER is at a heading matching KEY."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (and (org-at-heading-p)
           (equal key (downcase (string-trim (org-get-heading t t t t))))))))

(defun pos-marker-at (root file line key)
  "Return a marker at LINE of FILE under ROOT if KEY's heading is there."
  (with-current-buffer (pos-visit (expand-file-name file root))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (let ((marker (point-marker)))
        (and (pos-marker-still-at-p marker key) marker)))))

(defconst pos-waiting-on-property "WAITING_ON"
  "The property of a WAITING item that links to the person it waits on.
Its value is a link by ID to a person-identity of the item's scope.")

;;;; The intray's Unsorted

(defun pos-goto-unsorted-end ()
  "Move point to the end of the Unsorted subtree; create it if missing."
  (goto-char (point-min))
  (if (re-search-forward "^\\* Unsorted\\b" nil t)
      (org-end-of-subtree t t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* Unsorted\n"))
  (unless (bolp) (insert "\n")))

;;;; Commands in files of their own

(autoload 'pos-lint "pos-lint" nil t)
(autoload 'pos-lint-batch "pos-lint")
(autoload 'pos-dedupe-plan-batch "pos-dedupe")
(autoload 'pos-dedupe-apply-batch "pos-dedupe")
(autoload 'pos-refile-plan-batch "pos-refile")
(autoload 'pos-refile-apply-batch "pos-refile")

(provide 'pos)
;;; pos.el ends here
