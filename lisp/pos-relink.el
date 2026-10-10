;;; pos-relink.el --- Rewrite the links to a sealed item -*- lexical-binding: t; -*-

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

;; Sealing rewrites the links in the item's own files.  The links to
;; the item from the rest of the tree are rewritten by a second plan,
;; the relink plan, as doc/formats.org specifies under Relinking.  It
;; is made from the seal plan while the item still lies at its source,
;; so that both plans are reviewed before anything changes, and it is
;; applied after the seal, when the item's CID is in its ledger.
;;
;; A link by file and a link by ID are both rewritten to the ipfs://
;; form of the item, the path within it and a search option.
;;
;; - `pos-relink-plan': the plan, from a seal plan.
;; - `pos-relink-apply': apply a reviewed plan.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pos-bytes)
(require 'pos-ledger)
(require 'pos-links)
(require 'pos-corpus)
(require 'pos-referrers)

(defun pos-relink--within (target source)
  "Return the path of TARGET within the item at SOURCE, \"\" for SOURCE."
  (if (equal target source) "" (file-relative-name target source)))

(defun pos-relink-plan (root seal-plan)
  "Return the plan to rewrite the links to the item SEAL-PLAN seals.
ROOT is the tree whose Org files are read.  The item must lie at the
seal plan's source.  Each link to it or into it from a file the root
may write is planned: the file, its SHA-256, and for each link its
byte offset, its text, the path within the item it leads to and the
search option to follow it.  A link in a file the root may not write
is listed apart, by file and line, and is not rewritten.  Refuse a
plan that is not a seal plan, a source that is not there, and an id
link whose ID two files hold."
  (let-alist seal-plan
    (unless (and (equal .operation "seal") (stringp .source) (stringp .destination))
      (pos-ledger--refuse 'plan "Not a seal plan"))
    (unless (or (file-exists-p .source) (file-symlink-p .source))
      (pos-ledger--refuse 'source "No such item: %s" .source))
    (let* ((root (file-name-as-directory (file-truename root)))
           (corpus (pos-corpus root))
           (source (directory-file-name .source))
           (ids (pos-referrers-ids corpus))
           files unwritable)
      (maphash (lambda (id places)
                 (when (and (cdr places)
                            (seq-some (lambda (place)
                                        (not (string-prefix-p
                                              ".." (pos-relink--within (car place) source))))
                                      places))
                   (pos-ledger--refuse
                    'unresolved "Two files hold the ID %s: %s" id
                    (mapconcat (lambda (place) (file-relative-name (car place) root))
                               (reverse places) ", "))))
               ids)
      (pcase-dolist (`(,file ,offset ,text ,target ,suffix)
                     (pos-referrers--links corpus source))
        (let ((name (file-relative-name file root)))
          (if (pos-corpus-writable-p corpus file)
              (push `((offset . ,offset) (from . ,text)
                      (path . ,(pos-relink--within target source))
                      (suffix . ,suffix))
                    (alist-get name files nil nil #'equal))
            (push `((file . ,name)
                    (line . ,(pos-referrers--line (pos-bytes-read file) offset))
                    (link . ,text))
                  unwritable))))
      `((schema . 1) (operation . "relink")
        (root . ,(directory-file-name root))
        (source . ,.source) (destination . ,.destination)
        (seal_sha256 . ,(pos-bytes-sha (pos-bytes-json seal-plan)))
        (files . ,(vconcat
                   (mapcar
                    (lambda (entry)
                      `((file . ,(car entry))
                        (sha256 . ,(pos-bytes-sha
                                    (pos-bytes-read (expand-file-name (car entry) root))))
                        (links . ,(vconcat
                                   (sort (cdr entry)
                                         (lambda (a b)
                                           (< (alist-get 'offset a)
                                              (alist-get 'offset b))))))))
                    (sort files (lambda (a b) (string< (car a) (car b)))))))
        (unwritable . ,(vconcat
                        (sort unwritable
                              (lambda (a b)
                                (let ((fa (alist-get 'file a)) (fb (alist-get 'file b)))
                                  (if (equal fa fb)
                                      (< (alist-get 'line a) (alist-get 'line b))
                                    (string< fa fb)))))))))))

(defun pos-relink-apply (plan expected)
  "Apply PLAN, a relink plan whose canonical JSON has the SHA-256 EXPECTED.
The item must be sealed at the plan's destination: each link is
rewritten to the item's ipfs:// link as `pos-links-link' gives it,
then / and the path within the item if it is not the whole, then the
search option.  Every file is checked against its SHA-256 before any
is written.  Return the files rewritten, relative to the plan's root.
Refuse a plan that is not a relink plan, a destination not sealed,
and a file changed since the plan was made."
  (unless (equal (pos-bytes-sha (pos-bytes-json plan)) expected)
    (pos-ledger--refuse 'plan "Reviewed plan hash mismatch"))
  (let-alist plan
    (unless (and (eql .schema 1) (equal .operation "relink"))
      (pos-ledger--refuse 'plan "Not a relink plan"))
    (let ((item (pos-links-link .destination))
          writes)
      (seq-doseq (entry .files)
        (let* ((file (expand-file-name (alist-get 'file entry) .root))
               (bytes (pos-bytes-read file)))
          (unless (equal (pos-bytes-sha bytes) (alist-get 'sha256 entry))
            (pos-ledger--refuse 'plan "File changed since review: %s" file))
          (push (list file
                      (pos-links-rewrite
                       bytes
                       (mapcar (lambda (link)
                                 (let-alist link
                                   (list .offset .from
                                         (concat item
                                                 (if (equal .path "") "" (concat "/" .path))
                                                 .suffix))))
                               (alist-get 'links entry)))
                      (alist-get 'file entry))
                writes)))
      (dolist (write (nreverse writes))
        (pcase-let ((`(,file ,bytes) write))
          (let ((modes (file-modes file))
                (coding-system-for-write 'binary))
            (with-temp-file file
              (set-buffer-multibyte nil)
              (insert bytes))
            (set-file-modes file modes))))
      (mapcar (lambda (entry) (alist-get 'file entry)) .files))))

(provide 'pos-relink)
;;; pos-relink.el ends here
