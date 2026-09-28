;;; pos-seal-test.el --- Tests for pos-seal.el  -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'pos-seal)
(require 'pos-ledger-test
         (expand-file-name "pos-ledger-test"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defun pos-seal-test-relative-plan (plan dir)
  "Return PLAN with its absolute paths made relative to DIR."
  (let ((root (file-name-as-directory (file-truename dir))))
    (mapcar (lambda (pair)
              (if (memq (car pair) '(source destination archive ledger))
                  (cons (car pair) (file-relative-name (cdr pair) root))
                pair))
            plan)))

(defun pos-seal-test-run (fixture dir)
  "Seal FIXTURE, built in DIR; return what happened, relative to DIR.
An alist of plan, event and report, or of error."
  (let-alist fixture
    (condition-case err
        (let* ((plan (pos-seal-plan (expand-file-name .source dir)
                                    (expand-file-name .destination dir) .ledger_id
                                    (or .date "2026-09-28")))
               (result (pos-seal-apply plan (pos-ledger--sha (pos-ledger-json plan))))
               (scope (file-name-directory (alist-get 'archive plan))))
          `((plan . ,(pos-seal-test-relative-plan plan dir))
            (event . ((name . ,(file-name-nondirectory (car result)))
                      (encoded . ,(decode-coding-string (pos-ledger--read (car result))
                                                        'utf-8))))
            (report . ,(pos-ledger-test-relative (pos-ledger-check scope) dir))))
      (pos-ledger-refused `((error . ,(symbol-name (cadr err))))))))

(ert-deftest pos-seal/every-shared-fixture-seals-the-same-bytes ()
  "Plan, event and the report after it, or the refusal, as fixtures/ledger/."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "seal")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let ((got (pos-seal-test-run (cdr named) dir)))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (pos-ledger-test-same .plan (alist-get 'plan got))
                (pos-ledger-test-same .event (alist-get 'event got))
                (pos-ledger-test-same .report (alist-get 'report got))))))))))

(defmacro pos-seal-test-with-scope (&rest body)
  "Evaluate BODY with `scope' a temporary scope holding archives/ and an item."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pos-seal" t))
          (scope (expand-file-name "scope" dir)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "archives" scope) t)
           (pos-fixture-write (expand-file-name "trial/result.md" scope) "result")
           ,@body)
       (pos-fixture-writable dir)
       (delete-directory dir t))))

(defun pos-seal-test-plan (scope)
  "Return the plan to seal SCOPE's trial into its archive."
  (pos-seal-plan (expand-file-name "trial" scope)
                 (expand-file-name "archives/trial" scope)))

(ert-deftest pos-seal/sealing-removes-write-bits ()
  "The sealed files and the ledger event are read-only; CIDs do not change."
  (pos-seal-test-with-scope
    (let* ((plan (pos-seal-test-plan scope))
           (event (car (pos-seal-apply plan (pos-ledger--sha (pos-ledger-json plan)))))
           (file (expand-file-name "archives/trial/result.md" scope)))
      (should (zerop (logand (file-modes file) #o222)))
      (should (equal #o444 (logand (file-modes event) #o777)))
      (should (equal (pos-cid-file file)
                     (alist-get 'cid (cdr (assoc "trial/result.md" (alist-get 'add plan)))))))))

(ert-deftest pos-seal/a-plan-is-applied-only-as-reviewed ()
  "A plan whose hash differs from the one reviewed is refused, as is a
plan whose item changed after review; nothing moves."
  (pos-seal-test-with-scope
    (let ((plan (pos-seal-test-plan scope)))
      (should-error (pos-seal-apply plan (make-string 64 ?0)) :type 'pos-ledger-refused)
      (pos-fixture-write (expand-file-name "trial/result.md" scope) "changed")
      (should-error (pos-seal-apply plan (pos-ledger--sha (pos-ledger-json plan)))
                    :type 'pos-ledger-refused)
      (should (file-exists-p (expand-file-name "trial/result.md" scope)))
      (should-not (file-exists-p (expand-file-name "archives/trial" scope))))))

(ert-deftest pos-seal/an-interrupted-seal-resumes ()
  "After the move but before the event, the same plan finishes the work;
applied twice, it adds nothing."
  (pos-seal-test-with-scope
    (let* ((plan (pos-seal-test-plan scope))
           (hash (pos-ledger--sha (pos-ledger-json plan))))
      (make-directory (expand-file-name "archives" scope) t)
      (rename-file (expand-file-name "trial" scope) (expand-file-name "archives/trial" scope))
      (let ((first (pos-seal-apply plan hash)))
        (should (equal first (pos-seal-apply plan hash))))
      (should (equal 1 (nth 2 (pos-ledger-history (expand-file-name "archives" scope))))))))

(ert-deftest pos-seal/a-new-record-is-staged-then-sealed ()
  "New bytes are staged beside the archive, then sealed like any item."
  (pos-seal-test-with-scope
    (let* ((plan (pos-seal-stage "handover\n" (expand-file-name "archives/journal/h.md" scope)))
           (staged (alist-get 'source plan)))
      (should (string-prefix-p (file-truename (expand-file-name "_seal/" scope)) staged))
      (pos-seal-apply plan (pos-ledger--sha (pos-ledger-json plan)))
      (should-not (file-exists-p staged))
      (should (equal "handover\n" (pos-ledger--read (expand-file-name "archives/journal/h.md"
                                                                        scope)))))))

(ert-deftest pos-seal/a-program-applies-its-own-plan-explicitly ()
  "write-new DESTINATION --apply seals at once; without it, only a plan."
  (pos-seal-test-with-scope
    (let* ((target (expand-file-name "archives/journal/h.md" scope))
           (run (lambda (&rest args)
                  (with-temp-buffer
                    (insert "handover\n")
                    (list (apply #'call-process-region (point-min) (point-max)
                                 (expand-file-name invocation-name invocation-directory)
                                 t t nil "-Q" "--batch"
                                 "-L" (file-name-directory
                                       (expand-file-name (locate-library "markdown-mode")))
                                 "-L"
                                 (file-name-directory (expand-file-name (locate-library "pos-seal")))
                                 "-l" "pos-seal" "-f" "pos-seal-batch" "write-new" target args)
                          (buffer-string))))))
      (let ((planned (funcall run)))
        (ert-info ((cadr planned)) (should (equal 0 (car planned)))))
      (should-not (file-exists-p target))
      (let ((applied (funcall run "--apply")))
        (ert-info ((cadr applied)) (should (equal 0 (car applied)))))
      (should (equal "handover\n" (pos-ledger--read target))))))

(ert-deftest pos-seal/reading-links-reads-nothing-else ()
  "A #+SETUPFILE in an item is not followed while its links are read."
  (pos-seal-test-with-scope
    (let ((outside (expand-file-name "../outside.setup" scope)) read)
      (pos-fixture-write outside "#+TITLE: never read\n")
      (pos-fixture-write (expand-file-name "trial/r.org" scope)
                         (concat "#+SETUPFILE: " outside "\n\nText.\n"))
      (advice-add 'insert-file-contents :before
                  (lambda (file &rest _) (when (equal (expand-file-name file) outside)
                                           (setq read t)))
                  '((name . pos-seal-test-watch)))
      (unwind-protect (pos-seal-test-plan scope)
        (advice-remove 'insert-file-contents 'pos-seal-test-watch))
      (should-not read))))

(ert-deftest pos-seal/a-rumour-reads-nothing-outside-the-garden ()
  "A link out of the garden's repository gets a rumour naming where it
pointed; the target is not read."
  (pos-seal-test-with-scope
    (let ((outside (expand-file-name "../elsewhere.md" scope)) read)
      (make-directory (expand-file-name ".git" scope))
      (pos-fixture-write outside "# Somewhere else\n")
      (pos-fixture-write (expand-file-name "trial/r.md" scope) "See [there](../../elsewhere.md).\n")
      (advice-add 'insert-file-contents-literally :before
                  (lambda (file &rest _) (when (equal (expand-file-name file) outside)
                                           (setq read t)))
                  '((name . pos-seal-test-watch)))
      (let ((plan (unwind-protect (pos-seal-test-plan scope)
                    (advice-remove 'insert-file-contents-literally 'pos-seal-test-watch))))
        (should-not read)
        (should (string-match-p "outside the garden, and was not read"
                                (alist-get 'text (aref (alist-get 'rumours plan) 0))))))))

(provide 'pos-seal-test)
;;; pos-seal-test.el ends here
