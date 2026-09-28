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

;; Run: make test.  The CIDs are those IPFS computes; see
;; pos-cid-fixtures.el.

;;; Code:

(require 'ert)
(require 'pos-cid)
(require 'pos-cid-fixtures
         (expand-file-name "pos-cid-fixtures"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defun pos-cid-test-cid (path)
  "Return the CID of PATH, a file or a directory."
  (if (file-directory-p path) (pos-cid-directory path) (pos-cid-file path)))

(defun pos-cid-test-agrees (name)
  "Check that fixture NAME gets the CID IPFS gave it."
  (let ((fixture (pos-cid-fixture name)))
    (pos-cid-fixture-with fixture path
      (should (equal (nth 1 fixture) (pos-cid-test-cid path))))))

;;;; Files

(ert-deftest pos-cid/a-file-of-one-chunk-is-one-raw-block ()
  "Up to 1 MiB, a file's CID is its bytes' sha2-256 as a raw CIDv1."
  (pos-cid-test-agrees 'empty-file)
  (pos-cid-test-agrees 'hello)
  (pos-cid-test-agrees 'one-chunk))

(ert-deftest pos-cid/a-longer-file-links-its-chunks-from-a-dag-pb-node ()
  "One byte over a chunk, the file becomes a UnixFS node over two leaves."
  (pos-cid-test-agrees 'one-chunk-and-a-byte))

(ert-deftest pos-cid/a-file-past-the-link-limit-grows-a-balanced-level ()
  "With 4 links a node, 18 chunks need three levels, filled from the left."
  (pos-cid-test-agrees 'three-levels))

(ert-deftest pos-cid/content-alone-decides-a-file-cid ()
  "Name, place, permissions and times do not enter a file's CID.
Sealing removes write bits without changing any CID."
  (let ((dir (make-temp-file "pos-cid" t)))
    (unwind-protect
        (let ((a (expand-file-name "a.org" dir))
              (b (expand-file-name "deep/er/b.txt" dir)))
          (pos-cid-fixture-write a "same")
          (pos-cid-fixture-write b "same")
          (let ((before (pos-cid-file a)))
            (set-file-modes a #o444)
            (set-file-times a 0)
            (should (equal before (pos-cid-file a)))
            (should (equal before (pos-cid-file b)))))
      (delete-directory dir t))))

(ert-deftest pos-cid/bytes-and-their-file-share-a-cid ()
  (let ((bytes (pos-cid-fixture-bytes 3000))
        (pos-cid-chunk-size 256)
        (pos-cid-file-max-links 4)
        (dir (make-temp-file "pos-cid" t)))
    (unwind-protect
        (let ((file (expand-file-name "f" dir)))
          (pos-cid-fixture-write file bytes)
          (should (equal (pos-cid-file file) (pos-cid-bytes bytes))))
      (delete-directory dir t))))

;;;; Directories

(ert-deftest pos-cid/an-empty-directory-has-a-cid ()
  (pos-cid-test-agrees 'empty-directory))

(ert-deftest pos-cid/a-directory-links-its-entries-in-byte-order ()
  "Upper case sorts before lower, and UTF-8 after both; subdirectories
and chunked files nest."
  (pos-cid-test-agrees 'small-tree)
  (pos-cid-test-agrees 'names))

(ert-deftest pos-cid/hidden-files-are-left-out ()
  "As `ipfs add' leaves them out, so an archive's ledger is outside its CID."
  (pos-cid-fixture-with (pos-cid-fixture 'small-tree) path
    (let ((with (pos-cid-directory path)))
      (delete-file (expand-file-name ".hidden" path))
      (should (equal with (pos-cid-directory path))))))

(ert-deftest pos-cid/names-are-hashed-as-stored ()
  "The same name composed and decomposed are different bytes, so
different CIDs, each as IPFS computes it."
  (pos-cid-test-agrees 'composed-name)
  (pos-cid-test-agrees 'decomposed-name)
  (should-not (equal (nth 1 (pos-cid-fixture 'composed-name))
                     (nth 1 (pos-cid-fixture 'decomposed-name)))))

(ert-deftest pos-cid/a-directory-at-the-sharding-threshold-stays-whole ()
  "A directory block of exactly 262144 bytes is not sharded."
  (pos-cid-test-agrees 'at-sharding-threshold))

(ert-deftest pos-cid/a-directory-past-the-threshold-is-refused ()
  "IPFS shards a block of 262145 bytes; without sharding, no CID is given."
  (let ((dir (make-temp-file "pos-cid" t)))
    (unwind-protect
        (progn
          (pos-cid-fixture-threshold dir 1)
          (should-error (pos-cid-directory dir)
                        :type 'pos-cid-sharding-unsupported))
      (delete-directory dir t))))

(ert-deftest pos-cid/symlinks-are-refused ()
  (let ((dir (make-temp-file "pos-cid" t)))
    (unwind-protect
        (progn
          (pos-cid-fixture-write (expand-file-name "target" dir) "t")
          (make-symbolic-link "target" (expand-file-name "link" dir))
          (should-error (pos-cid-directory dir)))
      (delete-directory dir t))))

;;;; Trees

(ert-deftest pos-cid/a-tree-names-every-file-and-directory ()
  "Relative paths, the root as \".\", each with the CID it has alone."
  (pos-cid-fixture-with (pos-cid-fixture 'small-tree) path
    (let ((tree (pos-cid-tree path)))
      (should (equal '("." "B" "a" "sub" "sub/c")
                     (sort (mapcar #'car tree) #'string<)))
      (should (equal (pos-cid-directory path) (cdr (assoc "." tree))))
      (should (equal (pos-cid-directory (expand-file-name "sub" path))
                     (cdr (assoc "sub" tree))))
      (should (equal (pos-cid-file (expand-file-name "sub/c" path))
                     (cdr (assoc "sub/c" tree)))))))

(provide 'pos-cid-test)
;;; pos-cid-test.el ends here
