;;; pos-referrers.el --- The links that lead into a path -*- lexical-binding: t; -*-

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

;; What links to a file or a directory of a tree: the links in the
;; tree's Org files that lead to it or into it.  A link by file is
;; read as `pos-links-in-file' reads it.  A link by ID is followed to
;; each file of the tree that holds the ID, found by reading the files;
;; no index is asked.  A path that is to be moved or sealed is asked
;; for first, so that what links to it is known.
;;
;; - `pos-referrers': the links that lead to a path or into it.
;; - `pos-referrers-ids': each Org ID of a tree and the files that hold it.

;;; Code:

(require 'cl-lib)
(require 'pos-ledger)
(require 'pos-links)
(require 'pos-corpus)

(defun pos-referrers--line (bytes offset)
  "Return the line, from 1, that byte OFFSET of BYTES is on."
  (let ((line 1) (start 0))
    (while (and (setq start (string-search "\n" bytes start))
                (< start offset))
      (setq line (1+ line) start (1+ start)))
    line))

(defun pos-referrers-ids (corpus)
  "Return a hash table from each Org ID in CORPUS to the files that hold it.
An ID is the value of an ID property, of a file or of a heading."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (file (pos-corpus-files corpus))
      (let ((text (decode-coding-string (pos-ledger-read file) 'utf-8))
            (start 0))
        (while (string-match "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$" text start)
          (setq start (match-end 0))
          (cl-pushnew file (gethash (match-string 1 text) ids) :test #'equal))))
    ids))

(defun pos-referrers (root path)
  "Return the links in the tree at ROOT that lead to PATH or into it.
Each is (FILE LINE TEXT TARGET): the Org file of the corpus that holds
the link, the line it is on, the link as written and the file it
leads to, absolute.  A file link leads where its path does, read from
its file's directory.  An id link leads to each file of the corpus
that holds the ID.  A link in a file that is itself PATH or within it
is not one.  Sorted by file, then line."
  (let* ((corpus (pos-corpus root))
         (path (directory-file-name (expand-file-name path root)))
         (base (file-name-as-directory path))
         (inside (lambda (file) (or (equal file path) (string-prefix-p base file))))
         (ids (pos-referrers-ids corpus))
         links)
    (dolist (file (pos-corpus-files corpus))
      (unless (funcall inside file)
        (let ((bytes (pos-ledger-read file))
              (here (file-name-directory file)))
          (pcase-dolist (`(,offset ,text ,target) (pos-links-in-file file))
            (let ((target (directory-file-name (expand-file-name target here))))
              (when (funcall inside target)
                (push (list file (pos-referrers--line bytes offset) text target)
                      links))))
          (let ((start 0))
            (while (string-match "\\[\\[\\(id:\\([^]\n]+\\)\\)\\]" bytes start)
              (setq start (match-end 0))
              (let ((text (decode-coding-string (match-string 1 bytes) 'utf-8))
                    (line (pos-referrers--line bytes (match-beginning 0))))
                (dolist (target (gethash (decode-coding-string
                                          (match-string 2 bytes) 'utf-8)
                                         ids))
                  (when (funcall inside target)
                    (push (list file line text target) links)))))))))
    (sort links (lambda (a b)
                  (or (string< (car a) (car b))
                      (and (equal (car a) (car b))
                           (or (< (nth 1 a) (nth 1 b))
                               (and (= (nth 1 a) (nth 1 b))
                                    (string< (nth 3 a) (nth 3 b))))))))))

(provide 'pos-referrers)
;;; pos-referrers.el ends here
