;;; pos-bytes-test.el --- Tests for pos-bytes.el  -*- lexical-binding: t -*-

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

;; Run: make test.
;;
;; Each test is named for the rule it pins: a file's bytes as they
;; are, their hash, and canonical JSON as fixtures/json/ has it, which
;; pyposlib runs too.  DAG-JSON blocks, which a ledger refuses or
;; takes, are pinned with the ledger in test/pos-ledger-test.el.

;;; Code:

(require 'ert)
(require 'pos-bytes)
(require 'pos-fixtures)
(require 'pos-test-support)

(ert-deftest pos-bytes/bytes-are-read-and-written-as-they-are ()
  "A file's bytes come back as written: NUL, high bytes, no final newline.
Writing replaces what the file held."
  (pos-test-with-temp-dir dir
    (let ((file (expand-file-name "blob" dir))
          (bytes (unibyte-string 0 1 #x7f #x80 #xff ?a ?\n ?b)))
      (pos-bytes-write file "old contents\n")
      (pos-bytes-write file bytes)
      (should (equal bytes (pos-bytes-read file)))
      (should-not (multibyte-string-p (pos-bytes-read file))))))

(ert-deftest pos-bytes/the-hash-is-the-sha-256-of-the-bytes-in-lower-case ()
  "The hash of \"abc\" is the one SHA-256 gives, in lower-case hex."
  (should (equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                 (pos-bytes-sha "abc")))
  (should (equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                 (pos-bytes-sha ""))))

(ert-deftest pos-bytes/json-is-written-one-way ()
  "Keys in UTF-8 byte order, no spaces, raw UTF-8, few escapes, a newline.
Every fixture in fixtures/json/ encodes to its bytes."
  (dolist (named (pos-fixtures "json"))
    (ert-info ((car named))
      (let-alist (cdr named)
        (should (equal (encode-coding-string .encoded 'utf-8 t)
                       (pos-bytes-json .value)))))))

(ert-deftest pos-bytes/a-block-is-the-json-without-its-newline ()
  "A block is the canonical bytes alone; printing decodes them for a terminal."
  (dolist (named (pos-fixtures "json"))
    (ert-info ((car named))
      (let-alist (cdr named)
        (should (equal (pos-bytes-json .value)
                       (concat (pos-bytes-block .value) "\n")))
        (should (equal (string-trim-right .encoded "\n")
                       (string-trim-right
                        (with-output-to-string (pos-bytes-print .value)) "\n")))))))

(ert-deftest pos-bytes/what-is-written-is-parsed-back ()
  "Parsing the JSON of a value gives the value: objects alists, null :null.
A key is a string whether given as a symbol or a string."
  (dolist (named (pos-fixtures "json"))
    (ert-info ((car named))
      (let-alist (cdr named)
        (pos-test-same-json .value (pos-bytes-parse (pos-bytes-json .value))))))
  (should (equal "add" (pos-bytes-key 'add)))
  (should (equal "add" (pos-bytes-key "add")))
  (should (pos-bytes-utf8< "Z" "a"))
  (should-not (pos-bytes-utf8< "a" "Z")))

(ert-deftest pos-bytes/what-has-no-canonical-json-is-refused ()
  "An integer of more than 64 bits, a float, a boolean or a symbol has no JSON."
  (should-error (pos-bytes-json (expt 2 63)))
  (should-error (pos-bytes-json 1.5))
  (should-error (pos-bytes-json t))
  (should-error (pos-bytes-json 'symbol)))

(provide 'pos-bytes-test)
;;; pos-bytes-test.el ends here
