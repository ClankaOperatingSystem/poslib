;;; pos-ledger-test.el --- Tests for pos-ledger.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The expectations are the shared fixtures in
;; fixtures/json/ and fixtures/ledger/, which pyposlib meets too.

;;; Code:

(require 'ert)
(require 'pos-ledger)
(require 'pos-fixtures
         (expand-file-name "pos-fixtures"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defun pos-ledger-test-same (expected actual)
  "Check that EXPECTED and ACTUAL are the same JSON value."
  (should (equal (pos-ledger-json expected) (pos-ledger-json actual))))

(defmacro pos-ledger-test-refused (kind &rest body)
  "Check that BODY is refused with KIND, a string."
  (declare (indent 1))
  `(should (equal ,kind (condition-case err (progn ,@body nil)
                          (pos-ledger-refused (symbol-name (cadr err)))))))

(defun pos-ledger-test-relative (report dir)
  "Return REPORT with its absolute paths made relative to DIR."
  (let ((root (file-name-as-directory (file-truename dir))))
    (vconcat
     (mapcar (lambda (entry)
               (mapcar (lambda (pair)
                         (pcase (car pair)
                           ('archive (cons 'archive (file-relative-name (cdr pair) root)))
                           ('checkpoint_writable
                            (cons 'checkpoint_writable
                                  (vconcat (mapcar (lambda (f) (file-relative-name f root))
                                                   (cdr pair)))))
                           (_ pair)))
                       entry))
             report))))

;;;; Canonical JSON

(ert-deftest pos-ledger/json-is-written-one-way ()
  "Keys in UTF-8 byte order, no spaces, raw UTF-8, few escapes, a newline."
  (dolist (named (pos-fixtures "json"))
    (ert-info ((car named))
      (let-alist (cdr named)
        (should (equal (encode-coding-string .encoded 'utf-8 t)
                       (pos-ledger-json .value)))))))

;;;; Inventories and events

(ert-deftest pos-ledger/an-inventory-records-every-archived-file ()
  "Hidden and underscored files included; modes lose their write bits;
the first event enrols them all under the ledger's identity."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "inventory")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let ((archive (expand-file-name .archive dir)))
              (if .error
                  (pos-ledger-test-refused .error (pos-ledger-inventory archive))
                (let ((inventory (pos-ledger-inventory archive)))
                  (pos-ledger-test-same .inventory inventory)
                  (let ((event (pos-ledger-event inventory nil 1 .ledger_id)))
                    (should (equal .event.name (car event)))
                    (should (equal (encode-coding-string .event.encoded 'utf-8 t)
                                   (cdr event)))))))))))))

;;;; Ledgers

(ert-deftest pos-ledger/a-ledger-is-a-hash-chain-of-additions ()
  "Valid ledgers give their entries, head, recorded root CID and collections;
each fault is refused by kind.  A ledger lies beside its archive, in
archive-integrity/ledger/, or in the legacy folder inside it, not both."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "history")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let ((archive (expand-file-name .archive dir)))
              (if .error
                  (pos-ledger-test-refused .error (pos-ledger-history archive))
                (pcase-let ((`(,entries ,head ,events ,_ ,root ,collections ,items)
                             (pos-ledger-history archive)))
                  (should (equal .items (vconcat items)))
                  (should (equal .head head))
                  (should (equal .events events))
                  (should (equal .root (or root :null)))
                  (should (equal .collections (vconcat collections)))
                  (pos-ledger-test-same .entries entries))))))))))

;;;; Checks

(ert-deftest pos-ledger/a-check-reports-each-archive ()
  "Archives found outside hidden and underscored directories, checkpoints
honoured, and every changed, missing, new, writable and hidden file named;
the archive's CID beside the one recorded; collections no longer declared."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "report")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (pos-ledger-test-same
             .report (pos-ledger-test-relative (pos-ledger-check dir) dir))))))))

(ert-deftest pos-ledger/a-vanished-archive-is-detected ()
  "A checkpoint names ledger heads; removing an archive leaves one unmatched."
  (pos-fixture-with (pos-fixture "ledger" "report-clean") dir
    (let ((archive (expand-file-name "projects/A/archives" dir)))
      (pos-fixture-writable archive)
      (delete-directory archive t)
      (pos-ledger-test-refused "anchor" (pos-ledger-check dir)))))

(provide 'pos-ledger-test)
;;; pos-ledger-test.el ends here
