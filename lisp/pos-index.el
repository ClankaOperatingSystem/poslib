;;; pos-index.el --- Find archived files by CID -*- lexical-binding: t; -*-

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

;; The index from CID to path, for a scope's archives and those within
;; them.  It is ephemera: kept in the scope's _index/, rebuilt from the
;; sealed archives whenever it is missing or cannot answer, and never
;; itself evidence.
;;
;; - `pos-index-build': rebuild and save a scope's index.
;; - `pos-index-resolve': the file an ipfs:// link names.
;; - Org follows ipfs: links through it.

;;; Code:

(require 'ol)
(require 'pos-cid)
(require 'pos-ledger)

(defconst pos-index-file "_index/cids.json"
  "Where a scope keeps its index, relative to the scope.")

(defun pos-index-build (scope)
  "Rebuild SCOPE's index from its archives, save it, and return it.
An alist of CID and the paths, relative to SCOPE, that have it."
  (let* ((scope (file-name-as-directory (file-truename scope)))
         (table (make-hash-table :test #'equal)))
    (dolist (archive (pos-ledger-roots scope))
      (condition-case nil
          (dolist (pair (pos-cid-tree archive))
            (push (file-relative-name
                   (expand-file-name (if (equal (car pair) ".") "" (car pair)) archive)
                   scope)
                  (gethash (cdr pair) table)))
        (pos-cid-sharding-unsupported nil)))
    (let (index)
      (maphash (lambda (cid paths)
                 (push (cons cid (vconcat (sort (mapcar #'directory-file-name paths)
                                                #'string<)))
                       index))
               table)
      (setq index (sort index (lambda (a b) (string< (car a) (car b)))))
      (let ((file (expand-file-name pos-index-file scope)))
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'binary))
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert (pos-ledger-json `((schema . 1) (cids . ,index)))))))
      index)))

(defun pos-index--load (scope)
  "Return SCOPE's saved index, or nil if there is none."
  (let ((file (expand-file-name pos-index-file scope)))
    (when (file-exists-p file)
      (mapcar (lambda (pair) (cons (pos-ledger--key (car pair)) (cdr pair)))
              (alist-get 'cids (pos-ledger--parse (pos-ledger--read file)))))))

(defun pos-index--parse (uri)
  "Return (CID . PATH) from URI, ipfs://CID or ipfs://CID/PATH."
  (unless (string-match "\\`\\(?:ipfs:\\)?//\\([^/?#]+\\)\\(?:/\\([^?#]*\\)\\)?" uri)
    (error "Not an ipfs:// link: %s" uri))
  (cons (match-string 1 uri) (match-string 2 uri)))

(defun pos-index--lookup (scope index cid path)
  "Return the existing file INDEX gives for CID and PATH in SCOPE, or nil."
  (seq-some (lambda (base)
              (let ((file (expand-file-name (concat base (when path (concat "/" path)))
                                            scope)))
                (and (file-exists-p file) file)))
            (cdr (assoc cid index))))

(defun pos-index-resolve (scope uri)
  "Return the file URI names, from SCOPE's index, rebuilding it if need be."
  (pcase-let* ((scope (file-name-as-directory (file-truename scope)))
               (`(,cid . ,path) (pos-index--parse uri)))
    (or (pos-index--lookup scope (pos-index--load scope) cid path)
        (pos-index--lookup scope (pos-index-build scope) cid path)
        (error "No archived file for %s under %s" uri scope))))

(defun pos-index--scopes (directory)
  "Return the scopes above DIRECTORY that have archives, nearest first."
  (let (scopes (dir (file-name-as-directory (expand-file-name directory))))
    (while dir
      (when (and (file-directory-p (expand-file-name "archives" dir))
                 (not (equal (file-name-nondirectory (directory-file-name dir))
                             "archives")))
        (push dir scopes))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (equal parent dir) parent))))
    (nreverse scopes)))

(defun pos-index-follow (path &optional _)
  "Visit the archived file an Org ipfs: link names; PATH is after ipfs:.
Each scope above the current file is tried, nearest first."
  (let ((uri (concat "ipfs:" path)))
    (find-file
     (or (seq-some (lambda (scope)
                     (ignore-errors (pos-index-resolve scope uri)))
                   (pos-index--scopes default-directory))
         (user-error "No archived file for %s" uri)))))

(org-link-set-parameters "ipfs" :follow #'pos-index-follow)

(provide 'pos-index)
;;; pos-index.el ends here
