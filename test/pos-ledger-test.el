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
(require 'pos-fixtures
         (expand-file-name "pos-fixtures"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

;; A test asks no keeper but one a fixture has recorded.
(setq pos-ledger-offline t)

(defun pos-ledger-test-tape (recorded)
  "Return a function to send requests with that plays RECORDED, a keeper's.
Each request must be the next one recorded, and is answered as it was.
Called with no argument, it gives the exchanges not yet played."
  (let ((left (append (alist-get 'exchanges recorded) nil))
        (base (alist-get 'url recorded)))
    (lambda (&optional method url headers body)
      (if (null method)
          left
        (let* ((exchange (or (pop left)
                             (error "A request the recording does not have: %s %s"
                                    method url)))
               (sent `((method . ,method)
                       (path . ,(substring url (length base)))
                       (authorization . ,(or (cdr (assoc "Authorization" headers)) :null))
                       ,@(when body
                           `((content_type . ,(cdr (assoc "Content-Type" headers)))
                             (body_sha256 . ,(pos-ledger--sha body)))))))
          (unless (equal (pos-ledger-json sent)
                         (pos-ledger-json (alist-get 'request exchange)))
            (error "Not the request recorded: %S" sent))
          (cons (alist-get 'status (alist-get 'response exchange))
                (encode-coding-string (alist-get 'body (alist-get 'response exchange))
                                      'utf-8)))))))

(defmacro pos-ledger-test-with-keeper (recorded &rest body)
  "Evaluate BODY with RECORDED, a fixture's keeper, answering if there is one.
With one, checks ask it, and it must be played out; with none, they
ask nobody."
  (declare (indent 1))
  `(let* ((recorded ,recorded)
          (tape (and recorded (pos-ledger-test-tape recorded)))
          (pos-ledger-offline (not tape))
          (pos-remote-send-function (or tape pos-remote-send-function))
          (pos-remote-keeper-function
           (if tape
               (lambda (url)
                 (pos-remote-http-create :url url :token (alist-get 'token recorded)))
             pos-remote-keeper-function)))
     (prog1 (progn ,@body)
       (when (and tape (funcall tape))
         (error "The recording was not played out")))))

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

(ert-deftest pos-ledger/a-block-is-dag-json-written-one-way ()
  "Every fixture in fixtures/dag-json/: a value's block and its CID, or
bytes that are not the one block of their value, refused."
  (dolist (named (pos-fixtures "dag-json"))
    (ert-info ((car named))
      (let-alist (cdr named)
        (if .error
            (pos-ledger-test-refused .error
              (pos-ledger--strict (encode-coding-string .bytes 'utf-8 t) (car named)))
          (let ((block (encode-coding-string .encoded 'utf-8 t)))
            (should (equal block (pos-ledger-block .value)))
            (should (equal .cid (pos-ledger--event-cid block)))
            (pos-ledger-test-same .value (pos-ledger--strict block (car named)))))))))

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
                (pcase-let ((`(,entries ,head ,events ,_ ,root ,collections ,items ,empty)
                             (pos-ledger-history archive)))
                  (should (equal (or .empty []) (vconcat empty)))
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
the archive's CID beside the one recorded; collections no longer declared.
An archive a keeper keeps is found by its ledger and reported from it,
and where a fixture has recorded its keeper, with what the keeper holds."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "report")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (pos-ledger-test-with-keeper .keeper
              (if .error
                  (pos-ledger-test-refused .error (pos-ledger-check dir))
                (pos-ledger-test-same
                 .report (pos-ledger-test-relative (pos-ledger-check dir) dir))))))))))

(ert-deftest pos-ledger/a-vanished-archive-is-detected ()
  "A checkpoint names ledger heads; removing an archive leaves one unmatched."
  (pos-fixture-with (pos-fixture "ledger" "report-clean") dir
    (let ((archive (expand-file-name "projects/A/archives" dir)))
      (pos-fixture-writable archive)
      (delete-directory archive t)
      (pos-ledger-test-refused "anchor" (pos-ledger-check dir)))))

;;;; Kept archives

(ert-deftest pos-ledger/a-scope-s-own-repository-says-how-its-archive-is-kept ()
  "The nearest repository at or above a scope decides, by the scope's path
in it: a keeper's URL for a remote archive, and disk for every other.  A
repository mounted beneath another is not its container's to configure."
  (let ((dir (file-truename (make-temp-file "pos-kept" t))))
    (unwind-protect
        (let ((kept (lambda (scope)
                      (pos-ledger-kept (expand-file-name (concat scope "archives") dir)))))
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "projects/c/.git" dir) t)
          (pos-fixture-write
           (expand-file-name ".pos/config.yaml" dir)
           (concat "pos: 1\narchives:\n"
                   "  - scope: \".\"\n    kept: remote\n    url: https://keeper.example/root\n"
                   "  - scope: projects/a\n    kept: remote\n    url: https://keeper.example/a\n"
                   "  - scope: projects/b\n    kept: uncommitted\n"))
          (should (equal "https://keeper.example/root" (funcall kept "")))
          (should (equal "https://keeper.example/a" (funcall kept "projects/a/")))
          (should-not (funcall kept "projects/b/"))
          (should-not (funcall kept "projects/a/projects/d/"))
          (should-not (funcall kept "projects/c/")))
      (delete-directory dir t))))

(ert-deftest pos-ledger/a-kept-archive-is-checked-by-its-own-path ()
  "Named as the root, an archive a keeper keeps is checked though no
directory is there."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let ((report (pos-ledger-check (expand-file-name "projects/a/archives" dir))))
      (should (equal 1 (length report)))
      (should (equal 4 (alist-get 'files (car report)))))))

(ert-deftest pos-ledger/a-kept-archive-s-vanished-ledger-is-detected ()
  "A checkpoint names a kept archive's head; with its ledger gone, nothing
is found there and the head is unmatched."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let ((integrity (expand-file-name "projects/a/archive-integrity" dir)))
      (pos-fixture-writable integrity)
      (delete-directory integrity t)
      (pos-ledger-test-refused "anchor" (pos-ledger-check dir)))))

(provide 'pos-ledger-test)
;;; pos-ledger-test.el ends here
