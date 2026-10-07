;;; pos-roam.el --- The org-roam index of a tree -*- lexical-binding: t; -*-

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

;; org-roam keeps an index of a tree's Org files in SQLite: the nodes,
;; headings and files that carry an `:ID:', and the links between them.
;; The commands that rewrite links consult it rather than reading every
;; file, and bring it up to date first, so that what they read is what is
;; on disk.
;;
;; Where org-roam is already set up around the tree, so that
;; `org-roam-directory' holds the root, its index is the one used.
;; Elsewhere, as in the ClankOS container, the tree has an index of its
;; own under `pos-roam-cache-directory', named by the root's path.  Both
;; are caches: a rewrite checks the file before it writes.
;;
;; The index records a link only where a node holds it: a link in a file
;; with no `:ID:' at file level or on an enclosing heading is not in it.
;;
;; The files indexed are the corpus, pos-corpus.el: every Org file the
;; tree's configurations allow, so the index covers what the commands
;; read, no more and no less.

;;; Code:

(require 'cl-lib)
(require 'org-roam)
(require 'xdg)
(require 'pos-corpus)

(defcustom pos-roam-cache-directory
  (expand-file-name "pos/org-roam" (xdg-cache-home))
  "Where a tree's own org-roam index is kept, one database per root."
  :type 'directory
  :group 'pos)

(defun pos-roam-files (root)
  "Return the Org files under ROOT that the index covers: its corpus."
  (pos-corpus-files (pos-corpus root)))

(defun pos-roam-own-index-p (root)
  "Return non-nil if ROOT lies in `org-roam-directory', whose index serves."
  (and (stringp org-roam-directory)
       (file-directory-p org-roam-directory)
       (file-in-directory-p root org-roam-directory)))

(defun pos-roam-db-location (root)
  "Return where the index of ROOT is kept."
  (if (pos-roam-own-index-p root)
      org-roam-db-location
    (expand-file-name (concat (sha1 (file-truename root)) ".db")
                      pos-roam-cache-directory)))

(defmacro pos-roam-with-index (root &rest body)
  "Run BODY with the index of ROOT bound and up to date.
Within BODY, org-roam's directory, database and exclusions are the
tree's, and `org-roam-db-query' answers for it."
  (declare (indent 1))
  `(let* ((pos-roam--root (file-name-as-directory (file-truename ,root)))
          (pos-roam--own (pos-roam-own-index-p pos-roam--root))
          ;; Decided before org-roam's directory is rebound below: the
          ;; user's database serves only where ROOT lies in their directory.
          (pos-roam--location (pos-roam-db-location pos-roam--root))
          (org-roam-directory (if pos-roam--own org-roam-directory pos-roam--root))
          (org-roam-db-location pos-roam--location)
          ;; The tree's own index is given its files outright, below.
          (org-roam-file-exclude-regexp
           (if pos-roam--own org-roam-file-exclude-regexp nil))
          (org-roam-db-update-on-save nil)
          (org-roam-verbose nil))
     (make-directory (file-name-directory org-roam-db-location) t)
     ;; org-roam lists files by walking the whole directory and sifting
     ;; afterwards, which reads every repository and dependency below
     ;; the root.  The tree's own index lists only what it covers.
     (cl-letf (((symbol-function 'org-roam-list-files)
                (if pos-roam--own
                    (symbol-function 'org-roam-list-files)
                  (lambda () (pos-roam-files pos-roam--root)))))
       (org-roam-db-sync))
     ,@body))

(defun pos-roam-sync (root)
  "Bring the index of ROOT up to date; return the database's location."
  (pos-roam-with-index root
    org-roam-db-location))

(defun pos-roam-id-at (file line)
  "Return the `:ID:' of the heading at LINE of FILE, or nil."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (and (org-at-heading-p) (org-entry-get nil "ID")))))

(defun pos-roam-referrers (id)
  "Return (FILE . POS) for each `id:' link to ID the index records.
Call within `pos-roam-with-index'.  FILE is absolute; POS is where the
link begins in it."
  (mapcar (lambda (row) (cons (nth 0 row) (nth 1 row)))
          (org-roam-db-query
           [:select [nodes:file links:pos] :from links
            :inner-join nodes :on (= links:source nodes:id)
            :where (and (= links:dest $s1) (= links:type "id"))
            :order-by [nodes:file links:pos]]
           id)))

(defun pos-roam-buffer (file)
  "Return the buffer visiting FILE, found or made.
FILE is as the index names it, by its true path; a buffer visiting it
by another name is the same buffer."
  (or (find-buffer-visiting file) (find-file-noselect file)))

(defun pos-roam-rewrite-link (file pos from to &optional dry-run)
  "Point the `id:' link at POS of FILE from FROM to TO.
Return the buffer if the link is there, rewritten unless DRY-RUN, and
left to save; nil if FILE no longer holds a link to FROM at POS."
  (with-current-buffer (pos-roam-buffer file)
    (save-excursion
      (goto-char (min pos (point-max)))
      (when (looking-at (regexp-quote (concat "[[id:" from)))
        (unless dry-run (replace-match (concat "[[id:" to) t t))
        (current-buffer)))))

(provide 'pos-roam)
;;; pos-roam.el ends here
