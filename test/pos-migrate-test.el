;;; pos-migrate-test.el --- Tests for pos-migrate.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The seal fixtures in fixtures/ledger/ are shared
;; with pyposlib, which must seal the same bytes.
;;; Commentary:

;; Run: make test.  A legacy archive holding each thing the migration
;; must deal with is migrated, checked, and migrated again, idle.

;;; Code:

(require 'ert)
(require 'pos-migrate)
(require 'pos-seal-test
         (expand-file-name "pos-seal-test"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defun pos-migrate-test-enrol (archive)
  "Enrol everything in ARCHIVE in a legacy, schema 1 ledger, write-protected."
  (let* ((inventory (pos-ledger-inventory archive))
         (event (pos-ledger-event inventory nil 1 "0f1e2d3c-4b5a-4968-8778-a6b5c4d3e2f1")))
    (pos-seal--write-new (expand-file-name (concat pos-ledger-directory "/" (car event)) archive)
                         (cdr event))
    (pos-migrate--protect archive)
    (pos-ledger--sha (cdr event))))

(defmacro pos-migrate-test-with-legacy (&rest body)
  "Evaluate BODY with `scope' holding a legacy archive of every kind of case."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pos-migrate" t))
          (scope (expand-file-name "scope" dir))
          (archive (expand-file-name "archives" scope)))
     (unwind-protect
         (progn
           (dolist (f '(("plan.org" . "#+TITLE: The plan\n")
                        ("canon.org" . "See [[file:archives/receipt/source/.gitignore][it]].\n")
                        ("archives/2026-01-01-first.md" . "# First\n\nThen [second](2026-02-01-second.md).\n")
                        ("archives/2026-02-01-second.md" . "# Second\n\nAfter [first](2026-01-01-first.md), see [the plan](../plan.org) and [gone](gone.md).\n")
                        ("archives/receipt/manifest.json" . "{}\n")
                        ("archives/receipt/README.md" . "# Receipt\n\n[source](source/notes.org)\n")
                        ("archives/receipt/source/notes.org" . "#+TITLE: Notes\n\n[[file:../README.md][back]]\n")
                        ("archives/receipt/source/.gitignore" . "*.pyc\n")
                        ("archives/.DS_Store" . "finder\n")
                        ("archives/old/archives/r.md" . "retired\n")))
             (pos-fixture-write (expand-file-name (car f) scope) (cdr f)))
           ;; A retired scope carries its own legacy ledger.
           (pos-migrate-test-enrol (expand-file-name "archives/old/archives" scope))
           (pos-migrate-test-enrol archive)
           ,@body)
       (pos-fixture-writable dir)
       (delete-directory dir t))))

(defun pos-migrate-test-run (scope)
  "Plan and apply the migration of SCOPE's archive; return the plan."
  (let ((plan (pos-migrate-plan (expand-file-name "archives" scope) scope nil "2026-09-28")))
    (pos-migrate-apply plan (pos-ledger--sha (pos-ledger-json plan)))
    plan))

(ert-deftest pos-migrate/a-migrated-archive-checks-clean ()
  "Every file converted, the recorded root the archive's CID, no finding."
  (pos-migrate-test-with-legacy
    (pos-migrate-test-run scope)
    (let ((report (car (pos-ledger-check scope))))
      (let-alist report
        (should (equal .recorded_root .root))
        (dolist (list (list .missing .changed .new .writable .hidden .undeclared))
          (should (equal list [])))))
    (should (pos-migrate--converted-p (expand-file-name "archives" scope)))))

(ert-deftest pos-migrate/hidden-evidence-is-renamed-and-junk-removed ()
  "A hidden file that is evidence keeps its bytes under a dot prefix;
Finder's junk goes; a retired scope's ledger moves beside its archive;
canon's link to the renamed file follows it."
  (pos-migrate-test-with-legacy
    (pos-migrate-test-run scope)
    (should (equal "*.pyc\n" (pos-ledger--read (expand-file-name
                                                  "archives/receipt/source/dot.gitignore" scope))))
    (should-not (file-exists-p (expand-file-name "archives/.DS_Store" scope)))
    (should (directory-files (expand-file-name "archives/old/archive-integrity/ledger" scope)
                             nil "\\.json\\'"))
    (should (string-match-p "archives/receipt/source/dot.gitignore"
                            (pos-ledger--read (expand-file-name "canon.org" scope))))))

(ert-deftest pos-migrate/links-become-cids-rumours-or-annotations ()
  "Within the receipt, a collection, links stay; the later record cites
the earlier by CID, and the earlier's link forward is annotated; canon
gets a rumour; a broken link is annotated."
  (pos-migrate-test-with-legacy
    (let* ((plan (pos-migrate-test-run scope))
           (first (pos-ledger--read (expand-file-name "archives/2026-01-01-first.md" scope)))
           (second (pos-ledger--read (expand-file-name "archives/2026-02-01-second.md" scope))))
      (should (member "receipt" (append (alist-get 'collections plan) nil)))
      (should (string-match-p "#\\+COLLECTION: t"
                              (pos-ledger--read (expand-file-name "archives/receipt/README.org" scope))))
      (should (string-match-p "\\[later record: 2026-02-01-second.md\\]" first))
      (should (string-match-p "(ipfs://bafk" second))
      (should (string-match-p "\\[broken link: gone.md\\]" second))
      (should (directory-files (expand-file-name "archives/rumours" scope) nil "\\.org\\'")))))

(ert-deftest pos-migrate/migrating-twice-changes-nothing ()
  "Applied again, the plan finds its work done; planning again is refused."
  (pos-migrate-test-with-legacy
    (let* ((plan (pos-migrate-test-run scope))
           (before (pos-ledger-json (vconcat (pos-ledger-check scope)))))
      (pos-migrate-apply plan (pos-ledger--sha (pos-ledger-json plan)))
      (should (equal before (pos-ledger-json (vconcat (pos-ledger-check scope)))))
      (should-error (pos-migrate-plan (expand-file-name "archives" scope)) :type 'pos-ledger-refused))))

(provide 'pos-migrate-test)
;;; pos-migrate-test.el ends here
