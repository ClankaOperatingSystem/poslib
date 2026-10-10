;;; pos-relink-test.el --- Tests for pos-relink.el  -*- lexical-binding: t -*-

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
;; Each test is named for the rule it pins.  The tree is a garden with
;; an item, trial, an archive to seal it into, and an intray that
;; links to the item by file and by ID.  The seals are real.

;;; Code:

(require 'ert)
(require 'pos-relink)
(require 'pos-seal)
(require 'pos-test-support)

(defconst pos-relink-test-intray
  (concat "* Unsorted\n"
          "ü [[file:trial/notes.org::*Result][by file]] and [[file:trial][the item]]\n"
          "[[id:trial-id]] [[id:heading-id][a heading]] [[id:custom-id]]\n"
          "[[file:other.org][elsewhere]] [[id:other-id]] [[https://example.org][web]]\n")
  "An intray with five links into the item and three that are not.")

(defmacro pos-relink-test-with-garden (&rest body)
  "Evaluate BODY with `root' a garden holding the item trial and an intray.
`seal' is the plan to seal trial into the garden's archive."
  (declare (indent 0))
  `(pos-test-with-files dir
       `(("garden/.git/HEAD" . "ref: refs/heads/master\n")
         ("garden/intray.org" . ,pos-relink-test-intray)
         ("garden/other.org" . ":PROPERTIES:\n:ID: other-id\n:END:\n")
         ("garden/trial/notes.org"
          . ,(concat ":PROPERTIES:\n:ID: trial-id\n:END:\n"
                     "* Result\n:PROPERTIES:\n:ID: heading-id\n:END:\n"
                     "* Other :tag:\n:PROPERTIES:\n:ID: custom-id\n"
                     ":CUSTOM_ID: other\n:END:\n"))
         ("garden/trial/data.txt" . "data\n"))
     (let* ((root (file-name-as-directory
                   (file-truename (expand-file-name "garden" dir))))
            (seal (progn
                    (make-directory (expand-file-name "archives" root))
                    (pos-seal-plan (expand-file-name "trial" root)
                                   (expand-file-name "archives/trial" root)))))
       ,@body)))

(defun pos-relink-test-approve (plan)
  "Apply PLAN, a relink plan, with the hash of its own bytes."
  (pos-relink-apply plan (pos-ledger-sha (pos-ledger-json plan))))

(ert-deftest pos-relink/the-plan-lists-each-link-into-the-item ()
  "The plan names each file with links into the item, and each link.
A link has its byte offset, its text, the path within the item it
leads to and the search option to follow it.  A file link keeps its
own search option.  An id link takes its file's path and, for a
heading, the heading's CUSTOM_ID if it has one, else its title
without state and tags.  Links elsewhere are not in the plan.  The
plan names the seal plan by the SHA-256 of its bytes."
  (pos-relink-test-with-garden
    (let ((plan (pos-relink-plan root seal)))
      (should (equal "relink" (alist-get 'operation plan)))
      (should (equal (pos-ledger-sha (pos-ledger-json seal))
                     (alist-get 'seal_sha256 plan)))
      (should (equal [] (alist-get 'unwritable plan)))
      (should (= 1 (length (alist-get 'files plan))))
      (let ((entry (aref (alist-get 'files plan) 0)))
        (should (equal "intray.org" (alist-get 'file entry)))
        (should (equal (pos-ledger-sha
                        (encode-coding-string pos-relink-test-intray 'utf-8))
                       (alist-get 'sha256 entry)))
        (should (equal '(("file:trial/notes.org::*Result" "notes.org" "::*Result")
                         ("file:trial" "" "")
                         ("id:trial-id" "notes.org" "")
                         ("id:heading-id" "notes.org" "::*Result")
                         ("id:custom-id" "notes.org" "::#other"))
                       (mapcar (lambda (link)
                                 (let-alist link (list .from .path .suffix)))
                               (alist-get 'links entry))))
        ;; The first link follows a two-byte character.
        (should (= 16 (alist-get 'offset (aref (alist-get 'links entry) 0))))))))

(ert-deftest pos-relink/applied-after-the-seal-each-link-cites-the-item ()
  "Applied after the seal, each link is the item's ipfs:// link and a path.
The item's link is the one `pos-links-link' gives for the sealed
destination.  The item itself is cited whole.  What is not a link
into the item is as it was."
  (pos-relink-test-with-garden
    (let ((plan (pos-relink-plan root seal)))
      (pos-test-approve seal)
      (should (equal '("intray.org") (pos-relink-test-approve plan)))
      (let ((item (pos-links-link (expand-file-name "archives/trial" root))))
        (should (string-prefix-p "ipfs://" item))
        (should (equal (concat
                        "* Unsorted\n"
                        "ü [[" item "/notes.org::*Result][by file]]"
                        " and [[" item "][the item]]\n"
                        "[[" item "/notes.org]]"
                        " [[" item "/notes.org::*Result][a heading]]"
                        " [[" item "/notes.org::#other]]\n"
                        "[[file:other.org][elsewhere]] [[id:other-id]]"
                        " [[https://example.org][web]]\n")
                       (pos-test-file-string (expand-file-name "intray.org" root))))))))

(ert-deftest pos-relink/a-plan-is-refused-before-the-seal-or-after-a-change ()
  "A relink plan is applied only to what it was made for.
Refused: a hash that is not the plan's (plan); a destination not yet
sealed (unsealed); a file changed since the plan was made (plan); a
plan that is not a relink plan (plan).  Nothing is written."
  (pos-relink-test-with-garden
    (let ((plan (pos-relink-plan root seal))
          (intray (expand-file-name "intray.org" root)))
      (pos-test-refused pos-ledger-refused 'plan (pos-relink-apply plan "0"))
      (pos-test-refused pos-ledger-refused 'unsealed (pos-relink-test-approve plan))
      (pos-test-refused pos-ledger-refused 'plan (pos-relink-test-approve seal))
      (pos-test-approve seal)
      (pos-test-write-bytes intray "changed\n")
      (pos-test-refused pos-ledger-refused 'plan (pos-relink-test-approve plan))
      (should (equal "changed\n" (pos-test-file-string intray))))))

(ert-deftest pos-relink/an-id-two-files-hold-is-refused ()
  "An ID of the item that another file holds too is refused (unresolved).
So is a plan that is not a seal plan (plan).  An ID two files outside
the item hold is no matter of this plan's."
  (pos-relink-test-with-garden
    (pos-test-write-bytes (expand-file-name "twins-a.org" root)
                          ":PROPERTIES:\n:ID: twin\n:END:\n")
    (pos-test-write-bytes (expand-file-name "twins-b.org" root)
                          ":PROPERTIES:\n:ID: twin\n:END:\n")
    (should (pos-relink-plan root seal))
    (pos-test-refused pos-ledger-refused 'plan (pos-relink-plan root '((operation . "migrate"))))
    (pos-test-write-bytes (expand-file-name "copy.org" root)
                          ":PROPERTIES:\n:ID: trial-id\n:END:\n")
    (pos-test-refused pos-ledger-refused 'unresolved (pos-relink-plan root seal))))

(provide 'pos-relink-test)
;;; pos-relink-test.el ends here
