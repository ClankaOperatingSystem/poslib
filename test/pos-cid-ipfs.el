;;; pos-cid-ipfs.el --- Check the CID fixtures against IPFS  -*- lexical-binding: t -*-

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

;; Run: make check-ipfs IPFS=path/to/ipfs.  Offline: a fresh repository
;; with the unixfs-v1-2025 profile, and `ipfs add --only-hash', which
;; stores and announces nothing.  For each fixture in fixtures/cid/,
;; kubo must give its recorded CID and so must we; for one we refuse,
;; kubo must give the CID the fixture records it giving.

;;; Code:

(require 'pos-cid)
(require 'pos-fixtures
         (expand-file-name "pos-fixtures"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defvar pos-cid-ipfs-program
  (let ((program (or (getenv "IPFS") "ipfs")))
    (if (string-match-p "/" program) (expand-file-name program) program))
  "The kubo command.")

(defun pos-cid-ipfs-run (repo &rest args)
  "Run kubo on ARGS with repository REPO; return its trimmed output."
  (with-temp-buffer
    (let* ((process-environment (cons (concat "IPFS_PATH=" repo)
                                      process-environment))
           (status (apply #'call-process pos-cid-ipfs-program nil t nil args)))
      (unless (eql status 0)
        (error "ipfs %s failed: %s" (car args) (buffer-string)))
      (string-trim (buffer-string)))))

(defun pos-cid-ipfs-add (repo fixture path)
  "Return the CID kubo in REPO gives FIXTURE, built at PATH."
  (let-alist fixture
    (apply #'pos-cid-ipfs-run repo "add" "--quieter" "--only-hash"
           (append (when (file-directory-p path) '("--recursive"))
                   (when .params.chunk
                     (list (format "--chunker=size-%d" .params.chunk)))
                   (when .params.links
                     (list (format "--max-file-links=%d" .params.links)))
                   (list path)))))

(defun pos-cid-ipfs-check-inventory (repo)
  "Compare every inventory fixture's root with kubo in REPO; count failures."
  (let ((failed 0))
    (dolist (named (pos-fixtures "inventory"))
      (let-alist (cdr named)
        (pos-fixture-with (cdr named) dir
          (let* ((pos-cid-chunk-size (or .params.chunk pos-cid-chunk-size))
                 (pos-cid-file-max-links (or .params.links pos-cid-file-max-links))
                 (path (expand-file-name .entry dir))
                 (recorded (alist-get (intern ".") .cids))
                 (kubo (pos-cid-ipfs-add repo (cdr named) path))
                 (ours (pos-cid-directory path)))
            (if (and (equal kubo recorded) (equal ours recorded))
                (message "ok    inventory %s" (car named))
              (setq failed (1+ failed))
              (message "FAIL  inventory %s: recorded %s, kubo %s, ours %s"
                       (car named) recorded kubo ours))))))
    failed))

(defun pos-cid-ipfs-check ()
  "Compare every CID fixture with kubo; exit nonzero on any disagreement."
  (let ((repo (make-temp-file "pos-cid-ipfs" t))
        (failed 0))
    (unwind-protect
        (progn
          (delete-directory repo)
          (pos-cid-ipfs-run repo "init" "--profile" "unixfs-v1-2025,test")
          (message "%s" (pos-cid-ipfs-run repo "version"))
          (dolist (named (pos-fixtures "cid"))
            (let-alist (cdr named)
              (pos-fixture-with (cdr named) dir
                (let* ((path (expand-file-name .entry dir))
                       (kubo (pos-cid-ipfs-add repo (cdr named) path))
                       (ours (let ((pos-cid-chunk-size (or .params.chunk pos-cid-chunk-size))
                                   (pos-cid-file-max-links
                                    (or .params.links pos-cid-file-max-links)))
                               (condition-case nil
                                   (if (file-directory-p path)
                                       (pos-cid-directory path)
                                     (pos-cid-file path))
                                 (pos-cid-sharding-unsupported "sharding-unsupported")))))
                  (if (and (equal kubo (or .cid .ipfs)) (equal ours (or .cid .error)))
                      (message "ok    %s" (car named))
                    (setq failed (1+ failed))
                    (message "FAIL  %s: recorded %s, kubo %s, ours %s"
                             (car named) (or .cid .ipfs) kubo ours))))))
          (setq failed (+ failed (pos-cid-ipfs-check-inventory repo))))
      (delete-directory repo t))
    (kill-emacs (if (zerop failed) 0 1))))

(provide 'pos-cid-ipfs)
;;; pos-cid-ipfs.el ends here
