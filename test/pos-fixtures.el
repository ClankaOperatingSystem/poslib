;;; pos-fixtures.el --- The shared fixtures in fixtures/  -*- lexical-binding: t -*-

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

;; Loads the fixtures poslib and pyposlib share, and builds their trees
;; as doc/formats.org describes.

;;; Code:

(defconst pos-fixtures-directory
  (expand-file-name "../fixtures/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The shared fixtures.")

(defun pos-fixtures (kind)
  "Return the fixtures of KIND, a subdirectory, as (NAME . FIXTURE)."
  (mapcar (lambda (file)
            (cons (file-name-base file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'alist :null-object :null
                                       :false-object :false))))
          (directory-files (expand-file-name kind pos-fixtures-directory) t
                           "\\.json\\'")))

(defun pos-fixture (kind name)
  "Return fixture NAME of KIND."
  (or (cdr (assoc name (pos-fixtures kind)))
      (error "No fixture %s/%s" kind name)))

(defun pos-fixture-write (file bytes &optional mode)
  "Write the unibyte string BYTES to FILE, with MODE, default #o644."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'binary))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes)))
  (set-file-modes file (or mode #o644)))

(defun pos-fixture-bytes (entry)
  "Return the bytes a tree ENTRY describes."
  (let-alist entry
    (cond (.repeat (make-string (aref .repeat 1) (aref .repeat 0)))
          (.pattern (apply #'unibyte-string
                           (mapcar (lambda (i) (% i 251))
                                   (number-sequence 0 (1- .pattern)))))
          (t (encode-coding-string .text 'utf-8 t)))))

(defun pos-fixture-build (fixture dir)
  "Build FIXTURE's tree in DIR."
  (seq-doseq (entry (alist-get 'tree fixture))
    (let-alist entry
      (let ((path (expand-file-name .path dir)))
        (cond
         (.directory (make-directory path t))
         (.symlink (make-directory (file-name-directory path) t)
                   (make-symbolic-link .symlink path))
         (.hardlink (add-name-to-file (expand-file-name .hardlink dir) path))
         (.series (dotimes (i .series)
                    (pos-fixture-write (expand-file-name (format .path i) dir)
                                       (pos-fixture-bytes entry) .mode)))
         (t (pos-fixture-write path (pos-fixture-bytes entry) .mode)))))))

(defmacro pos-fixture-with (fixture dir &rest body)
  "Evaluate BODY with DIR bound to a temporary directory holding FIXTURE."
  (declare (indent 2))
  `(let ((,dir (make-temp-file "pos-fixture" t)))
     (unwind-protect
         (progn (pos-fixture-build ,fixture ,dir) ,@body)
       (pos-fixture-writable ,dir)
       (delete-directory ,dir t))))

(defun pos-fixture-writable (dir)
  "Make everything under DIR writable, so it can be deleted."
  (dolist (file (directory-files-recursively dir "" t))
    (unless (file-symlink-p file)
      (set-file-modes file (logior (file-modes file) #o200)))))

(provide 'pos-fixtures)
;;; pos-fixtures.el ends here
