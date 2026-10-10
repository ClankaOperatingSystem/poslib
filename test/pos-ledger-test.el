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
(require 'pos-remote)
(require 'pos-fixtures)

;;;; Canonical JSON

(ert-deftest pos-ledger/json-is-written-one-way ()
  "Keys in UTF-8 byte order, no spaces, raw UTF-8, few escapes, a newline."
  (dolist (named (pos-fixtures "json"))
    (ert-info ((car named))
      (let-alist (cdr named)
        (should (equal (encode-coding-string .encoded 'utf-8 t)
                       (pos-bytes-json .value)))))))

(ert-deftest pos-ledger/a-block-is-dag-json-written-one-way ()
  "Every fixture in fixtures/dag-json/ encodes, or is refused.
A value's block and its CID; or bytes that are not the one block of
their value, refused."
  (dolist (named (pos-fixtures "dag-json"))
    (ert-info ((car named))
      (let-alist (cdr named)
        (if .error
            (pos-test-refused pos-ledger-refused .error
              (pos-ledger--strict (encode-coding-string .bytes 'utf-8 t) (car named)))
          (let ((block (encode-coding-string .encoded 'utf-8 t)))
            (should (equal block (pos-bytes-block .value)))
            (should (equal .cid (pos-ledger--event-cid block)))
            (pos-test-same-json .value (pos-ledger--strict block (car named)))))))))

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
                  (pos-test-refused pos-ledger-refused .error (pos-ledger-inventory archive))
                (let ((inventory (pos-ledger-inventory archive)))
                  (pos-test-same-json .inventory inventory)
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
                  (pos-test-refused pos-ledger-refused .error (pos-ledger-history archive))
                (pcase-let ((`(,entries ,head ,events ,_ ,root ,collections ,items ,empty)
                             (pos-ledger-history archive)))
                  (should (equal (or .empty []) (vconcat empty)))
                  (should (equal .items (vconcat items)))
                  (should (equal .head head))
                  (should (equal .events events))
                  (should (equal .root (or root :null)))
                  (should (equal .collections (vconcat collections)))
                  (pos-test-same-json .entries entries))))))))))

;;;; Checks

(ert-deftest pos-ledger/a-check-reports-each-archive ()
  "A check reports each archive it finds, as the fixtures record.
Archives found outside hidden and underscored directories, checkpoints
honoured, and every changed, missing, new, writable and hidden file named;
the archive's CID beside the one recorded; collections no longer declared.
An archive a keeper keeps is found by its ledger and reported from it,
and where a fixture has recorded its keeper, with what the keeper holds."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "report")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (pos-test-with-keeper .keeper nil
              (if .error
                  (pos-test-refused pos-ledger-refused .error (pos-ledger-check dir))
                (pos-test-same-json
                 .report (pos-test-report-relative (pos-ledger-check dir) dir))))))))))

(ert-deftest pos-ledger/a-vanished-archive-is-detected ()
  "A checkpoint names ledger heads; removing an archive leaves one unmatched."
  (pos-fixture-with (pos-fixture "ledger" "report-clean") dir
    (let ((archive (expand-file-name "projects/A/archives" dir)))
      (pos-test-writable archive)
      (delete-directory archive t)
      (pos-test-refused pos-ledger-refused "anchor" (pos-ledger-check dir)))))

;;;; Kept archives

(ert-deftest pos-ledger/a-scope-s-own-repository-says-how-its-archive-is-kept ()
  "The nearest repository at or above a scope decides where it is kept.
By the scope's path in it: a keeper's URL for a remote archive, and disk
for every other.  A repository mounted beneath another is not its
container's to configure."
  (pos-test-with-temp-dir tmp
    (let ((dir (file-truename tmp)))
      (let ((kept (lambda (scope)
                    (pos-ledger-kept (expand-file-name (concat scope "archives") dir)))))
        (make-directory (expand-file-name ".git" dir))
        (make-directory (expand-file-name "projects/c/.git" dir) t)
        (pos-test-write-bytes
         (expand-file-name ".pos/config.yaml" dir)
         (concat "pos: 2\nprojects: projects/\narchives:\n"
                 "  - scope: \".\"\n    kept: remote\n    url: https://keeper.example/root\n"
                 "  - scope: projects/a\n    kept: remote\n    url: https://keeper.example/a\n"
                 "  - scope: projects/b\n    kept: uncommitted\n"))
        (should (equal "https://keeper.example/root" (funcall kept "")))
        (should (equal "https://keeper.example/a" (funcall kept "projects/a/")))
        (should-not (funcall kept "projects/b/"))
        (should-not (funcall kept "projects/a/projects/d/"))
        (should-not (funcall kept "projects/c/"))))))

(ert-deftest pos-ledger/an-archive-s-entry-is-in-its-nearest-node ()
  "The node is the nearest directory with a configuration.
Of either name, whether or not it is a repository; two in one node are
refused."
  (pos-test-with-temp-dir tmp
    (let ((dir (file-truename tmp)))
      (let ((kept (lambda (scope)
                    (pos-ledger-kept (expand-file-name (concat scope "archives") dir)))))
        (pos-test-write-bytes
         (expand-file-name ".clanka/config.yml" dir)
         (concat "pos: 2\nprojects: projects/\narchives:\n"
                 "  - scope: \".\"\n    kept: remote\n    url: https://keeper.example/root\n"
                 "  - scope: health\n    kept: remote\n    url: https://keeper.example/wrong\n"))
        (pos-test-write-bytes
         (expand-file-name "health/.pos/config.yaml" dir)
         (concat "pos: 2\nprojects: projects/\narchives:\n"
                 "  - scope: \".\"\n    kept: remote\n    url: https://keeper.example/health\n"))
        (should (equal "https://keeper.example/root" (funcall kept "")))
        (should (equal "https://keeper.example/health" (funcall kept "health/")))
        (should-not (funcall kept "health/diet/"))
        (pos-test-write-bytes (expand-file-name "health/.clanka/config.yaml" dir)
                              "pos: 2\nprojects: projects/\n")
        (should (eq 'config
                    (condition-case err (funcall kept "health/")
                      (pos-ledger-refused (nth 1 err)))))))))

(ert-deftest pos-ledger/a-kept-archive-is-checked-by-its-own-path ()
  "A kept archive named as the root is checked though no directory is there."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let ((report (pos-ledger-check (expand-file-name "projects/a/archives" dir))))
      (should (equal 1 (length report)))
      (should (equal 4 (alist-get 'files (car report)))))))

(ert-deftest pos-ledger/a-kept-archive-s-vanished-ledger-is-detected ()
  "A kept archive's checkpoint is unmatched once its ledger is gone.
A checkpoint names the archive's head; with the ledger gone, nothing is
found there and the head is unmatched."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let ((integrity (expand-file-name "projects/a/archive-integrity" dir)))
      (pos-test-writable integrity)
      (delete-directory integrity t)
      (pos-test-refused pos-ledger-refused "anchor" (pos-ledger-check dir)))))

(provide 'pos-ledger-test)
;;; pos-ledger-test.el ends here
