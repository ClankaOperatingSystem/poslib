;;; pos-index-test.el --- Tests for pos-index.el  -*- lexical-binding: t -*-

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

;; Run: make test.

;;; Code:

(require 'ert)
(require 'org)
(require 'pos-index)
(require 'pos-seal-test
         (expand-file-name "pos-seal-test"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defmacro pos-index-test-with-sealed (&rest body)
  "Evaluate BODY with `scope' holding a sealed trial/ and a sealed note.md,
`trial-cid' and `note-cid' their CIDs."
  (declare (indent 0))
  `(pos-seal-test-with-scope
     (pos-fixture-write (expand-file-name "note.md" scope) "note")
     (dolist (item '("trial" "note.md"))
       (let ((plan (pos-seal-plan (expand-file-name item scope)
                                  (expand-file-name (concat "archives/" item) scope))))
         (pos-seal-apply plan (pos-ledger--sha (pos-ledger-json plan)))))
     (let ((trial-cid (pos-cid-directory (expand-file-name "archives/trial" scope)))
           (note-cid (pos-cid-file (expand-file-name "archives/note.md" scope))))
       ,@body)))

(ert-deftest pos-index/a-cid-and-a-path-name-a-file-in-an-item ()
  "ipfs://ITEM-CID/PATH names a file within a sealed directory; a file
sealed alone is ipfs://ITS-CID."
  (pos-index-test-with-sealed
    (should (equal (file-truename (expand-file-name "archives/trial/result.md" scope))
                   (pos-index-resolve scope (concat "ipfs://" trial-cid "/result.md"))))
    (should (equal (file-truename (expand-file-name "archives/note.md" scope))
                   (pos-index-resolve scope (concat "ipfs://" note-cid))))
    (should-error (pos-index-resolve scope "ipfs://bafkreiaaaa"))))

(ert-deftest pos-index/the-index-is-ephemera ()
  "Deleted, the index is rebuilt from the sealed archives when needed,
byte for byte as it was."
  (pos-index-test-with-sealed
    (pos-index-build scope)
    (let* ((file (expand-file-name pos-index-file scope))
           (saved (pos-ledger--read file)))
      (delete-directory (file-name-directory file) t)
      (should (pos-index-resolve scope (concat "ipfs://" note-cid)))
      (should (equal saved (pos-ledger--read file))))))

(ert-deftest pos-index/org-follows-ipfs-links ()
  "An ipfs: link in canon opens the archived file it names."
  (pos-index-test-with-sealed
    (let ((canon (expand-file-name "notes.org" scope)))
      (pos-fixture-write canon (concat "[[ipfs://" trial-cid "/result.md][the result]]\n"))
      (with-current-buffer (find-file-noselect canon)
        (unwind-protect
            (progn
              (goto-char (point-min))
              (org-open-at-point)
              (should (equal (file-truename (expand-file-name "archives/trial/result.md" scope))
                             (file-truename buffer-file-name)))
              (kill-buffer))
          (kill-buffer (find-buffer-visiting canon)))))))

(provide 'pos-index-test)
;;; pos-index-test.el ends here
