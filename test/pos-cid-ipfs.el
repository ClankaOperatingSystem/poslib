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
;; kubo must give the CID the fixture records it giving.  Every block in
;; fixtures/dag-json/, and every schema 3 event in fixtures/ledger/, kubo
;; must store unchanged under the CID recorded, with `ipfs dag put'.

;;; Code:

(require 'pos-cid)
(require 'pos-ledger)
(require 'pos-fixtures)

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
        (error "Running ipfs %s failed: %s" (car args) (buffer-string)))
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

(defun pos-cid-ipfs-block-agrees (repo label text recorded)
  "Return non-nil if kubo in REPO stores TEXT unchanged under the CID RECORDED.
As a dag-json block; LABEL names it in the message."
  (let* ((bytes (encode-coding-string text 'utf-8 t))
         (file (make-temp-file "pos-cid-ipfs-block"))
         (ours (pos-cid-block pos-cid-dag-json bytes)))
    (unwind-protect
        (let* ((_ (let ((coding-system-for-write 'binary))
                    (with-temp-file file
                      (set-buffer-multibyte nil)
                      (insert bytes))))
               (kubo (pos-cid-ipfs-run repo "dag" "put" "--input-codec" "dag-json"
                                       "--store-codec" "dag-json" file))
               (stored (let ((coding-system-for-read 'utf-8))
                         (pos-cid-ipfs-run repo "block" "get" kubo)))
               (agrees (and (equal kubo recorded) (equal ours recorded)
                            (equal stored text))))
          (if agrees
              (message "ok    %s" label)
            (message "FAIL  %s: recorded %s, kubo %s, ours %s" label recorded kubo ours))
          agrees)
      (delete-file file))))

(defun pos-cid-ipfs-check-blocks (repo)
  "Compare every DAG-JSON fixture block with kubo in REPO; count failures.
The blocks of fixtures/dag-json/, and every schema 3 event a ledger
fixture holds or expects."
  (let ((failed 0))
    (dolist (named (pos-fixtures "dag-json"))
      (let-alist (cdr named)
        (unless (or .error
                    (pos-cid-ipfs-block-agrees repo (concat "dag-json " (car named))
                                               .encoded .cid))
          (setq failed (1+ failed)))))
    (dolist (named (pos-fixtures "ledger"))
      (let ((fixture (cdr named)) events)
        (unless (alist-get 'error fixture)
          (seq-doseq (entry (alist-get 'tree fixture))
            (let ((path (alist-get 'path entry)) (text (alist-get 'text entry)))
              (when (and text (string-match-p "/ledger/" path))
                (push (cons (file-name-nondirectory path) text) events))))
          (seq-doseq (event (append (and (alist-get 'event fixture)
                                         (list (alist-get 'event fixture)))
                                    (alist-get 'converted fixture) nil))
            (push (cons (alist-get 'name event) (alist-get 'encoded event)) events))
          (dolist (event (nreverse events))
            (let ((id (substring (car event) 9 -5)))
              (when (pos-ledger--event-cid-p id)
                (unless (pos-cid-ipfs-block-agrees
                         repo (format "ledger %s %s" (car named) (substring (car event) 0 8))
                         (cdr event) id)
                  (setq failed (1+ failed)))))))))
    failed))

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
          (setq failed (+ failed (pos-cid-ipfs-check-inventory repo)
                          (pos-cid-ipfs-check-blocks repo))))
      (delete-directory repo t))
    (kill-emacs (if (zerop failed) 0 1))))

(provide 'pos-cid-ipfs)
;;; pos-cid-ipfs.el ends here
