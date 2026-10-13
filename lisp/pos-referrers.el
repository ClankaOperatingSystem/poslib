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
;; - `pos-referrers': the links that lead to a path or into it, each a
;;   `pos-referrer'.
;; - `pos-referrers-print': the same, printed, a line for each.
;; - `pos-referrers-ids': each Org ID of a tree and where it is held.

;;; Code:

(require 'cl-lib)
(require 'pos-bytes)
(require 'pos-links)
(require 'pos-corpus)

(cl-defstruct (pos-referrer (:constructor pos-referrer--make) (:copier nil))
  "A link in a tree's Org files that leads to a path or into it.
FILE is the Org file that holds the link, and TEXT the link as
written, at byte OFFSET, on LINE.  TARGET is the file it leads to,
absolute.  SUFFIX is the search option that goes with it: a file
link's own, or for an id link the anchor of the ID."
  file offset line text target suffix)

(defun pos-referrers-line (bytes offset)
  "Return the line, from 1, that byte OFFSET of BYTES is on."
  (let ((line 1) (start 0))
    (while (and (setq start (string-search "\n" bytes start))
                (< start offset))
      (setq line (1+ line) start (1+ start)))
    line))

(defun pos-referrers-ids (corpus)
  "Return a hash table from each Org ID in CORPUS to where it is held.
As `pos-links-ids' gives it for the corpus's files."
  (pos-links-ids (pos-corpus-files corpus)))

(defun pos-referrers-links (corpus path)
  "Return the links in CORPUS that lead to PATH, absolute, or into it.
Each is a `pos-referrer'.  Unsorted."
  (let* ((base (file-name-as-directory path))
         (inside (lambda (file) (or (equal file path) (string-prefix-p base file))))
         (ids (pos-referrers-ids corpus))
         links)
    (dolist (file (pos-corpus-files corpus))
      (unless (funcall inside file)
        (let ((bytes (pos-bytes-read file))
              (here (file-name-directory file)))
          (pcase-dolist ((cl-struct pos-link offset text (path target) suffix)
                         (pos-links-in-file file))
            (let ((target (directory-file-name (expand-file-name target here))))
              (when (funcall inside target)
                (push (pos-referrer--make
                       :file file :offset offset
                       :line (pos-referrers-line bytes offset)
                       :text text :target target :suffix suffix)
                      links))))
          (let ((start 0))
            (while (string-match "\\[\\[\\(id:\\([^]\n]+\\)\\)\\]" bytes start)
              (setq start (match-end 0))
              (let ((offset (match-beginning 1))
                    (text (decode-coding-string (match-string 1 bytes) 'utf-8)))
                (pcase-dolist (`(,target . ,anchor)
                               (gethash (decode-coding-string
                                         (match-string 2 bytes) 'utf-8)
                                        ids))
                  (when (funcall inside target)
                    (push (pos-referrer--make
                           :file file :offset offset
                           :line (pos-referrers-line bytes offset)
                           :text text :target target :suffix anchor)
                          links)))))))))
    links))

(defun pos-referrers (root path)
  "Return the links in the tree at ROOT that lead to PATH or into it.
Each is a `pos-referrer'.  A file link leads where its path does, read
from its file's directory.  An id link leads to each file of the
corpus that holds the ID.  A link in a file that is itself PATH or
within it is not one.  Sorted by file, then line, then target, then
text."
  (sort (pos-referrers-links
         (pos-corpus root)
         (directory-file-name (expand-file-name path root)))
        (lambda (a b)
          (pcase-let (((cl-struct pos-referrer (file file-a) (line line-a)
                                  (target target-a) (text text-a))
                       a)
                      ((cl-struct pos-referrer (file file-b) (line line-b)
                                  (target target-b) (text text-b))
                       b))
            (or (string< file-a file-b)
                (and (equal file-a file-b)
                     (or (< line-a line-b)
                         (and (= line-a line-b)
                              (or (string< target-a target-b)
                                  (and (equal target-a target-b)
                                       (string< text-a text-b)))))))))))

(defun pos-referrers-print (root path)
  "Print each of `pos-referrers' of PATH in ROOT, a directory name.
A line for each, as FILE:LINE: LINK -> FILE, the files relative to
ROOT."
  (pcase-dolist ((cl-struct pos-referrer file line text target)
                 (pos-referrers root path))
    (princ (format "%s:%d: %s -> %s\n"
                   (file-relative-name file root) line text
                   (file-relative-name target root)))))

(provide 'pos-referrers)
;;; pos-referrers.el ends here
