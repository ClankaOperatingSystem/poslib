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
;; stores and announces nothing.  Each fixture's recorded CID, kubo's
;; and ours must agree.

;;; Code:

(require 'pos-cid)
(require 'pos-cid-fixtures
         (expand-file-name "pos-cid-fixtures"
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
  (let ((params (nthcdr 3 fixture)))
    (apply #'pos-cid-ipfs-run repo "add" "--quieter" "--only-hash"
           (append (when (file-directory-p path) '("--recursive"))
                   (when (plist-get params :chunk)
                     (list (format "--chunker=size-%d" (plist-get params :chunk))))
                   (when (plist-get params :links)
                     (list (format "--max-file-links=%d" (plist-get params :links))))
                   (list path)))))

(defun pos-cid-ipfs-check ()
  "Compare every fixture with kubo; exit nonzero on any disagreement."
  (let ((repo (make-temp-file "pos-cid-ipfs" t))
        (failed 0))
    (unwind-protect
        (progn
          (delete-directory repo)
          (pos-cid-ipfs-run repo "init" "--profile" "unixfs-v1-2025,test")
          (message "%s" (pos-cid-ipfs-run repo "version"))
          (dolist (fixture pos-cid-fixtures)
            (pos-cid-fixture-with fixture path
              (let ((kubo (pos-cid-ipfs-add repo fixture path))
                    (ours (if (file-directory-p path)
                              (pos-cid-directory path)
                            (pos-cid-file path))))
                (if (and (equal kubo (nth 1 fixture)) (equal ours kubo))
                    (message "ok    %s" (car fixture))
                  (setq failed (1+ failed))
                  (message "FAIL  %s: recorded %s, kubo %s, ours %s"
                           (car fixture) (nth 1 fixture) kubo ours))))))
      (delete-directory repo t))
    (kill-emacs (if (zerop failed) 0 1))))

(provide 'pos-cid-ipfs)
;;; pos-cid-ipfs.el ends here
