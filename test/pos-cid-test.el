;;; pos-cid-test.el --- Tests for pos-cid.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The CIDs are those IPFS computes, from the shared
;; fixtures in fixtures/cid/.

;;; Code:

(require 'ert)
(require 'pos-cid)
(require 'pos-fixtures
         (expand-file-name "pos-fixtures"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defun pos-cid-test-cid (path)
  "Return the CID of PATH, a file or a directory."
  (if (file-directory-p path) (pos-cid-directory path) (pos-cid-file path)))

(defun pos-cid-test-run (fixture)
  "Return FIXTURE's CID, or the kind of error it gets."
  (let-alist fixture
    (pos-fixture-with fixture dir
      (let ((pos-cid-chunk-size (or .params.chunk pos-cid-chunk-size))
            (pos-cid-file-max-links (or .params.links pos-cid-file-max-links)))
        (condition-case nil
            (pos-cid-test-cid (expand-file-name .entry dir))
          (pos-cid-sharding-unsupported "sharding-unsupported"))))))

(defun pos-cid-test-agrees (name)
  "Check that fixture NAME gets the CID IPFS gave it."
  (let ((fixture (pos-fixture "cid" name)))
    (should (equal (or (alist-get 'cid fixture) (alist-get 'error fixture))
                   (pos-cid-test-run fixture)))))

(ert-deftest pos-cid/every-shared-fixture-agrees ()
  "Every fixture in fixtures/cid/, as pyposlib must also agree."
  (dolist (named (pos-fixtures "cid"))
    (ert-info ((car named))
      (pos-cid-test-agrees (car named)))))

;;;; Files

(ert-deftest pos-cid/a-file-of-one-chunk-is-one-raw-block ()
  "Up to 1 MiB, a file's CID is its bytes' sha2-256 as a raw CIDv1."
  (pos-cid-test-agrees "empty-file")
  (pos-cid-test-agrees "hello")
  (pos-cid-test-agrees "one-chunk"))

(ert-deftest pos-cid/a-longer-file-links-its-chunks-from-a-dag-pb-node ()
  "One byte over a chunk, the file becomes a UnixFS node over two leaves."
  (pos-cid-test-agrees "one-chunk-and-a-byte"))

(ert-deftest pos-cid/a-file-past-the-link-limit-grows-a-balanced-level ()
  "With 4 links a node, 18 chunks need three levels, filled from the left."
  (pos-cid-test-agrees "three-levels"))

(ert-deftest pos-cid/content-alone-decides-a-file-cid ()
  "Name, place, permissions and times do not enter a file's CID.
Sealing removes write bits without changing any CID."
  (let ((dir (make-temp-file "pos-cid" t)))
    (unwind-protect
        (let ((a (expand-file-name "a.org" dir))
              (b (expand-file-name "deep/er/b.txt" dir)))
          (pos-fixture-write a "same")
          (pos-fixture-write b "same")
          (let ((before (pos-cid-file a)))
            (set-file-modes a #o444)
            (set-file-times a 0)
            (should (equal before (pos-cid-file a)))
            (should (equal before (pos-cid-file b)))))
      (delete-directory dir t))))

(ert-deftest pos-cid/bytes-and-their-file-share-a-cid ()
  (let ((bytes (pos-fixture-bytes '((pattern . 3000))))
        (pos-cid-chunk-size 256)
        (pos-cid-file-max-links 4)
        (dir (make-temp-file "pos-cid" t)))
    (unwind-protect
        (let ((file (expand-file-name "f" dir)))
          (pos-fixture-write file bytes)
          (should (equal (pos-cid-file file) (pos-cid-bytes bytes))))
      (delete-directory dir t))))

;;;; Directories

(ert-deftest pos-cid/an-empty-directory-has-a-cid ()
  (pos-cid-test-agrees "empty-directory"))

(ert-deftest pos-cid/a-directory-links-its-entries-in-byte-order ()
  "Upper case sorts before lower, and UTF-8 after both; subdirectories
and chunked files nest."
  (pos-cid-test-agrees "small-tree")
  (pos-cid-test-agrees "names"))

(ert-deftest pos-cid/hidden-files-are-left-out ()
  "As `ipfs add' leaves them out, so an archive's ledger is outside its CID."
  (pos-fixture-with (pos-fixture "cid" "small-tree") dir
    (let* ((path (expand-file-name "t" dir))
           (with (pos-cid-directory path)))
      (delete-file (expand-file-name ".hidden" path))
      (should (equal with (pos-cid-directory path))))))

(ert-deftest pos-cid/names-are-hashed-as-stored ()
  "The same name composed and decomposed are different bytes, so
different CIDs, each as IPFS computes it."
  (pos-cid-test-agrees "composed-name")
  (pos-cid-test-agrees "decomposed-name")
  (should-not (equal (alist-get 'cid (pos-fixture "cid" "composed-name"))
                     (alist-get 'cid (pos-fixture "cid" "decomposed-name")))))

(ert-deftest pos-cid/a-directory-at-the-sharding-threshold-stays-whole ()
  "A directory block of exactly 262144 bytes is not sharded."
  (pos-cid-test-agrees "at-sharding-threshold"))

(ert-deftest pos-cid/a-directory-past-the-threshold-is-refused ()
  "IPFS shards a block of 262145 bytes; without sharding, no CID is given."
  (pos-cid-test-agrees "past-sharding-threshold"))

(ert-deftest pos-cid/symlinks-are-refused ()
  (let ((dir (make-temp-file "pos-cid" t)))
    (unwind-protect
        (progn
          (pos-fixture-write (expand-file-name "target" dir) "t")
          (make-symbolic-link "target" (expand-file-name "link" dir))
          (should-error (pos-cid-directory dir)))
      (delete-directory dir t))))

;;;; Trees

(ert-deftest pos-cid/a-tree-names-every-file-and-directory ()
  "Relative paths, the root as \".\", each with the CID it has alone."
  (pos-fixture-with (pos-fixture "cid" "small-tree") dir
    (let* ((path (expand-file-name "t" dir))
           (tree (pos-cid-tree path)))
      (should (equal '("." "B" "a" "sub" "sub/c")
                     (sort (mapcar #'car tree) #'string<)))
      (should (equal (pos-cid-directory path) (cdr (assoc "." tree))))
      (should (equal (pos-cid-directory (expand-file-name "sub" path))
                     (cdr (assoc "sub" tree))))
      (should (equal (pos-cid-file (expand-file-name "sub/c" path))
                     (cdr (assoc "sub/c" tree)))))))

(provide 'pos-cid-test)
;;; pos-cid-test.el ends here
