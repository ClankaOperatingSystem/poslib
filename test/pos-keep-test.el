;;; pos-keep-test.el --- Tests for pos-keep.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The convert, keep and recall fixtures in
;; fixtures/ledger/ are shared with pyposlib: each gives a tree, what
;; the command does to it and what it answers, and for a keeper, each
;; exchange as its keeper recorded it.

;;; Code:

(require 'ert)
(require 'pos-keep)
(require 'pos-fixtures)
(require 'pos-test-support)

(ert-deftest pos-seal/every-shared-fixture-converts-the-same-bytes ()
  "Each convert fixture in fixtures/ledger/ converts as recorded.
The conversion events, what was skipped and the report after, or the
refusal."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "convert")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let* ((root (expand-file-name .root dir))
                   (base (file-name-as-directory (file-truename dir)))
                   (got (condition-case err
                            (pos-keep-convert root)
                          (pos-ledger-refused
                           `((error . ,(symbol-name (cadr err))))))))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (should-not (alist-get 'error got))
                (pos-test-same-json
                 .converted
                 (vconcat
                  (mapcar (lambda (c)
                            (let ((file (alist-get 'event c)))
                              `((name . ,(file-name-nondirectory file))
                                (encoded . ,(decode-coding-string
                                             (pos-bytes-read file) 'utf-8)))))
                          (alist-get 'converted got))))
                (pos-test-same-json
                 .skipped
                 (vconcat
                  (mapcar (lambda (s)
                            `((archive . ,(file-relative-name (alist-get 'archive s) base))
                              (reason . ,(alist-get 'reason s))))
                          (alist-get 'skipped got))))
                (pos-test-same-json
                 .report (pos-test-report-relative (pos-ledger-check root) dir))))))))))

(ert-deftest pos-seal/every-shared-fixture-keeps-the-same-way ()
  "Each keep fixture in fixtures/ledger/ keeps as recorded.
What was moved to a keeper, what was left with its reason and the
report after, or the refusal: each request as its keeper recorded it,
and a kept archive gone from disk."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "keep")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let* ((root (expand-file-name .root dir))
                   (base (file-name-as-directory (file-truename dir)))
                   (relative (lambda (items)
                               (vconcat
                                (mapcar (lambda (item)
                                          (cons (cons 'archive
                                                      (file-relative-name
                                                       (alist-get 'archive item) base))
                                                (assq-delete-all 'archive
                                                                 (copy-alist item))))
                                        items))))
                   (pos-seal-claims-function (lambda (&rest _) .claims))
                   (got (pos-test-with-keeper .keeper nil
                          (condition-case err
                              (pos-keep root)
                            (pos-ledger-refused
                             `((error . ,(symbol-name (cadr err)))))))))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (should-not (alist-get 'error got))
                (pos-test-same-json .kept (funcall relative (alist-get 'kept got)))
                (pos-test-same-json .skipped (funcall relative (alist-get 'skipped got)))
                (seq-doseq (item (alist-get 'kept got))
                  (should-not (file-exists-p (alist-get 'archive item))))
                (pos-test-same-json
                 .report (pos-test-report-relative (pos-ledger-check root) dir))))))))))

(ert-deftest pos-seal/every-shared-fixture-recalls-the-same-way ()
  "Each recall fixture in fixtures/ledger/ recalls as recorded.
What was brought back from a keeper, what was left with its reason and
the archive on disk after, or the refusal: each request as its keeper
recorded it."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "recall")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let* ((root (expand-file-name .root dir))
                   (base (file-name-as-directory (file-truename dir)))
                   (relative (lambda (items)
                               (vconcat
                                (mapcar (lambda (item)
                                          (cons (cons 'archive
                                                      (file-relative-name
                                                       (alist-get 'archive item) base))
                                                (assq-delete-all 'archive
                                                                 (copy-alist item))))
                                        items))))
                   (got (pos-test-with-keeper .keeper nil
                          (condition-case err
                              (pos-keep-recall root)
                            (pos-ledger-refused
                             `((error . ,(symbol-name (cadr err)))))))))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (should-not (alist-get 'error got))
                (pos-test-same-json .recalled (funcall relative (alist-get 'recalled got)))
                (pos-test-same-json .skipped (funcall relative (alist-get 'skipped got)))
                (let (after)
                  (named-let walk ((here (expand-file-name "archives" (file-truename root))))
                    (let ((names (directory-files here nil directory-files-no-dot-files-regexp)))
                      (unless names
                        (push (list (file-relative-name here base) 'directory) after))
                      (dolist (name names)
                        (let ((path (expand-file-name name here)))
                          (if (file-directory-p path)
                              (walk path)
                            (push (list (file-relative-name path base)
                                        (decode-coding-string (pos-bytes-read path) 'utf-8)
                                        (logand (file-modes path) #o777))
                                  after))))))
                  (should (equal (mapcar (lambda (entry)
                                           (let-alist entry
                                             (if .directory
                                                 (list .path 'directory)
                                               (list .path .text .mode))))
                                         .after)
                                 (sort after (lambda (a b) (string< (car a) (car b)))))))))))))))

(provide 'pos-keep-test)
;;; pos-keep-test.el ends here
