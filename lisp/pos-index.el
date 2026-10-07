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
;; - `pos-index-resolve': where the file an ipfs:// link names is.
;; - `pos-index-fetch': a buffer of a file fetched from its keeper.
;; - `pos-index-bytes': the bytes of the file a link names, wherever it is.
;; - Org follows ipfs: links through it.

;;; Code:

(require 'ol)
(require 'pos-cid)
(require 'pos-ledger)
(require 'pos-remote)

(defconst pos-index-file "_index/cids.json"
  "Where a scope keeps its index, relative to the scope.")

(defun pos-index-build (scope)
  "Rebuild SCOPE's index from its archives, save it, and return it.
An alist of CID and the paths, relative to SCOPE, that have it."
  (let* ((scope (file-name-as-directory (file-truename scope)))
         (table (make-hash-table :test #'equal)))
    (dolist (archive (pos-ledger-roots scope))
      (dolist (pair (if (pos-ledger-kept archive)
                        (pos-ledger-fold-cids archive)
                      (pos-cid-tree archive)))
        (push (file-relative-name
               (expand-file-name (if (equal (car pair) ".") "" (car pair)) archive)
               scope)
              (gethash (cdr pair) table))))
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
              (alist-get 'cids (pos-ledger-parse (pos-ledger-read file)))))))

(defun pos-index--parse (uri)
  "Return (CID . PATH) from URI, ipfs://CID or ipfs://CID/PATH.
PATH ends where an Org search begins, at ::, as a sealed item's link
is written (doc/formats.org, \"Links\")."
  (unless (string-match "\\`\\(?:ipfs:\\)?//\\([^/?#:]+\\)\\(?:/\\([^?#]*\\)\\)?" uri)
    (error "Not an ipfs:// link: %s" uri))
  (let ((cid (match-string 1 uri))
        (path (match-string 2 uri)))
    (cons cid (and path (substring path 0 (string-search "::" path))))))

(defun pos-index--lookup (scope index cid path)
  "Return the existing file INDEX gives for CID and PATH in SCOPE, or nil."
  (seq-some (lambda (base)
              (let ((file (expand-file-name (concat base (when path (concat "/" path)))
                                            scope)))
                (and (file-exists-p file) file)))
            (cdr (assoc cid index))))

(defun pos-index--kept (scope index cid path)
  "Return where a keeper has the file INDEX gives for CID and PATH in SCOPE.
As (URL FILE-CID NAME): the keeper's base URL, the CID the ledger enrols
the file under, and the file's name; or nil if INDEX gives no file a
keeper keeps.  A directory is not a file: the protocol reads none."
  (seq-some
   (lambda (base)
     (let* ((file (expand-file-name (concat base (when path (concat "/" path))) scope))
            (archive (seq-find
                      (lambda (a) (string-prefix-p (file-name-as-directory a) file))
                      (pos-ledger-roots scope)))
            (url (and archive (pos-ledger-kept archive)))
            (entry (and url (assoc (file-relative-name file archive)
                                   (car (pos-ledger-history archive))))))
       (and entry
            (list url (alist-get 'cid (cdr entry)) (file-name-nondirectory file)))))
   (cdr (assoc cid index))))

(defun pos-index-resolve (scope uri)
  "Return where the file URI names is, from SCOPE's index.
A file on disk, or for a file a keeper keeps, the URL the keeper has it
at.  The index is rebuilt if it does not have it."
  (pcase-let* ((scope (file-name-as-directory (file-truename scope)))
               (`(,cid . ,path) (pos-index--parse uri))
               (kept (lambda (index)
                       (pcase (pos-index--kept scope index cid path)
                         (`(,url ,file-cid ,_)
                          (concat (string-remove-suffix "/" url) "/ipfs/" file-cid))))))
    (or (pos-index--lookup scope (pos-index--load scope) cid path)
        (funcall kept (pos-index--load scope))
        (pos-index--lookup scope (pos-index-build scope) cid path)
        (funcall kept (pos-index--load scope))
        (error "No archived file for %s under %s" uri scope))))

(defun pos-index--kept-read (scope uri)
  "Return what a keeper has of the file URI names, from SCOPE's index.
As (BYTES FILE-CID NAME): the bytes the keeper answers with, the CID
the ledger enrols the file under, and the file's name; or nil if no
keeper keeps such a file."
  (pcase-let* ((scope (file-name-as-directory (file-truename scope)))
               (`(,cid . ,path) (pos-index--parse uri)))
    (pcase (or (pos-index--kept scope (pos-index--load scope) cid path)
               (pos-index--kept scope (pos-index-build scope) cid path))
      (`(,url ,file-cid ,name)
       (list (pos-remote-read (funcall pos-remote-keeper-function url) file-cid)
             file-cid name)))))

(defun pos-index-fetch (scope uri)
  "Return a buffer holding the file URI names, fetched from its keeper.
SCOPE's index says which keeper has it; nil if none does.  The buffer
is read-only, visits no file, and has the coding and the mode the
file's name and bytes give, as visiting the file would."
  (pcase (pos-index--kept-read scope uri)
    (`(,bytes ,_ ,name)
     (let ((buffer (generate-new-buffer name))
           (file (expand-file-name name scope)))
       (with-current-buffer buffer
         ;; The bytes as they are, then decoded as a file of that name is.
         (insert bytes)
         (decode-coding-inserted-region (point-min) (point-max) file)
         (setq buffer-file-coding-system last-coding-system-used)
         (goto-char (point-min))
         (let ((buffer-file-name file))
           (set-auto-mode))
         (set-buffer-modified-p nil)
         (setq buffer-read-only t))
       buffer))))

(defun pos-index--scopes (directory)
  "Return the scopes above DIRECTORY that have archives, nearest first.
On disk, or kept by a keeper and known by their ledgers."
  (let (scopes (dir (file-name-as-directory (expand-file-name directory))))
    (while dir
      (when (and (or (file-directory-p (expand-file-name "archives" dir))
                     (pos-ledger--kept-here (directory-file-name dir)))
                 (not (equal (file-name-nondirectory (directory-file-name dir))
                             "archives")))
        (push dir scopes))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (equal parent dir) parent))))
    (nreverse scopes)))

(defun pos-index-follow (path &optional _)
  "Visit the archived file an Org ipfs: link names; PATH is after ipfs:.
Each scope above the current file is tried, nearest first.  A file a
keeper keeps is fetched from it and shown read-only."
  (let* ((uri (concat "ipfs:" path))
         (scopes (pos-index--scopes default-directory))
         (file (seq-some (lambda (scope)
                           (let ((found (ignore-errors (pos-index-resolve scope uri))))
                             (and found (file-name-absolute-p found) found)))
                         scopes)))
    (cond (file (find-file file))
          ((seq-some (lambda (scope)
                       (when-let* ((buffer (pos-index-fetch scope uri)))
                         (pop-to-buffer-same-window buffer)
                         buffer))
                     scopes))
          (t (user-error "No archived file for %s" uri)))))

(defun pos-index-bytes (uri &optional directory)
  "Return the bytes of the archived file URI names, a unibyte string.
URI is ipfs://CID or ipfs://CID/PATH.  Each scope above DIRECTORY, by
default the current one, is tried, nearest first.  A file on disk is
read as it lies.  A file a keeper keeps is read from the keeper and
must have the CID its ledger enrols it under, else `entry'.  Refuse
`absent' if URI is no such link or no scope has such a file; a
directory is not a file."
  (unless (string-match-p "\\`ipfs://[^/?#:]" uri)
    (pos-ledger--refuse 'absent "Not an ipfs:// link: %s" uri))
  (let* ((scopes (pos-index--scopes (or directory default-directory)))
         (file (seq-some (lambda (scope)
                           (let ((found (ignore-errors (pos-index-resolve scope uri))))
                             (and found (file-name-absolute-p found)
                                  (file-regular-p found) found)))
                         scopes)))
    (or (and file (pos-ledger-read file))
        (seq-some (lambda (scope)
                    (pcase (pos-index--kept-read scope uri)
                      (`(,bytes ,file-cid ,_)
                       (unless (equal file-cid (pos-cid-bytes bytes))
                         (pos-ledger--refuse
                          'entry "The keeper's bytes are not those of %s" file-cid))
                       bytes)))
                  scopes)
        (pos-ledger--refuse 'absent "No archived file for %s" uri))))

(org-link-set-parameters "ipfs" :follow #'pos-index-follow)

(provide 'pos-index)
;;; pos-index.el ends here
