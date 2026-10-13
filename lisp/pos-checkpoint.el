;;; pos-checkpoint.el --- Checkpoints of ledger heads, and repair  -*- lexical-binding: t; -*-

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

;; A checkpoint records the ledger heads under a root, once the check
;; is clean, as doc/formats.org specifies; repair restores what a
;; checkout loses of an archive, its recorded empty directories and
;; read bits, and never enrols or rewrites a ledger.  Both read the
;; report of `pos-ledger-check'.
;;
;; - `pos-checkpoint-findings-p': whether a check report has a finding.
;; - `pos-checkpoint-record': record the heads under a root.
;; - `pos-checkpoint-repair': restore directories and read bits.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pos-bytes)
(require 'pos-ledger)

(defun pos-checkpoint-findings-p (report)
  "Return non-nil if REPORT, from `pos-ledger-check', has any finding."
  (seq-some (lambda (archive)
              (let-alist archive
                (or (eq .head :null)
                    (seq-some (lambda (list) (> (length list) 0))
                              (list .changed .missing .new .writable
                                    .hidden .undeclared))
                    (and (not (eq .recorded_root :null))
                         (not (equal .recorded_root .root)))
                    ;; A keeper asked, whose head is not the ledger's.
                    (and (not (eq .keeper :null))
                         (not (equal (alist-get 'head .keeper) .head))))))
            report))

(defun pos-checkpoint-record (root)
  "Record the ledger heads of every archive under ROOT; return where.
Refused unless the check is clean of changed, missing and new files and
of write bits.  The checkpoint covers an archive if ROOT is one, else
the tree; one with the same bytes already recorded is kept."
  (let ((report (pos-ledger-check root)))
    (when (seq-some (lambda (archive)
                      (let-alist archive
                        (seq-some (lambda (list) (> (length list) 0))
                                  (list .changed .missing .new .writable))))
                    report)
      (pos-ledger-refuse 'unclean "Enrol new records and restore permissions \
before checkpointing: %s" root))
    (let* ((heads (sort (seq-uniq (seq-remove (lambda (h) (eq h :null))
                                              (mapcar (lambda (a) (alist-get 'head a))
                                                      report)))
                        #'string<))
           (home (pos-ledger-anchor-home (pos-ledger-anchor-base root))))
      (when heads
        (let* ((coverage (if (equal (file-name-nondirectory
                                     (directory-file-name (file-truename root)))
                                    "archives")
                             "archive" "tree"))
               (bytes (pos-bytes-json `((schema . 1) (heads . ,(vconcat heads))
                                        (coverage . ,coverage))))
               (file (expand-file-name (concat (pos-bytes-sha bytes) ".json") home)))
          (cond ((not (file-exists-p file)) (pos-ledger-write-new file bytes))
                ((not (equal (pos-bytes-read file) bytes))
                 (pos-ledger-refuse 'checkpoint "Checkpoint conflict: %s" file)))))
      home)))

(defun pos-checkpoint--missing-directories (archive known recorded empty)
  "Return missing directories in ARCHIVE needed by the ledger's EMPTY paths.
KNOWN entries and EMPTY must fold to RECORDED.  Check every existing
component before returning paths in parent-before-child order."
  (when empty
    (unless (equal recorded (cdr (assoc "." (pos-ledger-fold known empty))))
      (pos-ledger-refuse 'root "Ledger cannot reconstruct the recorded root: %s" archive))
    (let (missing)
      (dolist (name empty)
        (let ((path archive))
          (dolist (part (split-string name "/"))
            (setq path (expand-file-name part path))
            (cond
             ((or (file-symlink-p path)
                  (and (file-exists-p path) (not (file-directory-p path))))
              (pos-ledger-refuse 'differs "Recorded directory is obstructed: %s" path))
             ((not (file-exists-p path))
              (unless (member path missing) (push path missing)))))))
      (nreverse missing))))

(defun pos-checkpoint--read-bits-only-p (file entry)
  "Return non-nil if FILE differs from recorded ENTRY only in read or write bits."
  (let ((actual (if (assq 'cid entry) (pos-ledger-record-with-cid file)
                  (pos-ledger-record file))))
    (seq-every-p
     (lambda (pair)
       (let ((now (alist-get (car pair) actual)))
         (if (eq (car pair) 'mode)
             (equal (logand now (lognot #o444)) (logand (cdr pair) (lognot #o444)))
           (equal now (cdr pair)))))
     entry)))

(defun pos-checkpoint-repair (root)
  "Restore recorded empty directories and protect verified evidence under ROOT.
Never enrols or rewrites a ledger.  Restores recorded read bits, but refuses
changed bytes or other mode bits, and missing files.
Return an alist: repaired, the files protected; restored,
the directories created; and unregistered, the files the ledgers do not know."
  (let ((report (pos-ledger-check root)) (count 0) (restored 0) directories)
    (when (seq-some (lambda (a) (> (length (alist-get 'missing a)) 0)) report)
      (pos-ledger-refuse 'differs "Evidence changed or is missing; repair refused: %s"
                         root))
    ;; Validate every archive and destination before changing any of them.
    (dolist (a report)
      (when (eq (alist-get 'kept a) :null)
        (let* ((archive (alist-get 'archive a))
               (history (pos-ledger-history archive)))
          (seq-doseq (name (alist-get 'changed a))
            (unless (pos-checkpoint--read-bits-only-p
                     (expand-file-name name archive) (cdr (assoc name (pos-history-entries history))))
              (pos-ledger-refuse 'differs "Evidence changed; repair refused: %s" archive)))
          (setq directories
                (append directories
                        (pos-checkpoint--missing-directories
                         archive (pos-history-entries history)
                         (pos-history-root history) (pos-history-empty history)))))))
    (dolist (directory directories)
      (make-directory directory)
      (setq restored (1+ restored)))
    (let ((protect (lambda (file &optional recorded-mode)
                     (pos-ledger-regular file)
                     (let* ((mode (pos-ledger-permissions file))
                            (target (or recorded-mode (logand mode (lognot #o222)))))
                       (unless (= mode target)
                         (set-file-modes file target 'nofollow)
                         (setq count (1+ count)))))))
      (dolist (a report)
        (let ((archive (alist-get 'archive a)))
          (pcase-let (((cl-struct pos-history (entries known) files) (pos-ledger-history archive)))
            ;; A keeper holds a kept archive's files; only its ledger is here.
            (when (eq (alist-get 'kept a) :null)
              (dolist (pair known)
                (funcall protect (expand-file-name (car pair) archive)
                         (alist-get 'mode (cdr pair)))))
            (mapc protect files))))
      (mapc protect (pos-ledger-checkpoint-files root (pos-ledger-roots root))))
    `((repaired . ,count) (restored . ,restored)
      (unregistered . ,(apply #'+ (mapcar (lambda (a) (length (alist-get 'new a))) report))))))

(provide 'pos-checkpoint)
;;; pos-checkpoint.el ends here
