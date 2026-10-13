;;; pos-node-test.el --- Tests for pos-node.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The configuration fixtures in fixtures/pos-directory/
;; and the declaration fixtures in fixtures/pos-methodology/ are for
;; pyposlib too: each gives a file's text and what a reader makes of
;; it, or the kind of refusal.

;;; Code:

(require 'ert)
(require 'pos-node)
(require 'pos-fixtures)

;;;; The file

(defun pos-node-test-sorted (value)
  "Return VALUE with every alist in it sorted by key, keys as strings."
  (cond
   ((vectorp value) (vconcat (mapcar #'pos-node-test-sorted value)))
   ((and (consp value) (consp (car value)))
    (sort (mapcar (lambda (pair)
                    (cons (format "%s" (car pair)) (pos-node-test-sorted (cdr pair))))
                  value)
          (lambda (a b) (string< (car a) (car b)))))
   (t value)))

(ert-deftest pos-node/a-config-is-read-as-the-fixtures-say ()
  "Each fixture's file gives its configuration, or its kind of refusal."
  (let ((fixtures (pos-fixtures "pos-directory")))
    (should (> (length fixtures) 20))
    (dolist (named fixtures)
      (ert-info ((car named) :prefix "fixture: ")
        (let-alist (cdr named)
          (if .refused
              (should (equal .refused
                             (condition-case err
                                 (progn (pos-node-read-config .yaml) "accepted")
                               (pos-node-refused (symbol-name (nth 1 err))))))
            (should (equal (pos-node-test-sorted .config)
                           (pos-node-test-sorted
                            (pos-node-read-config .yaml))))))))))

(ert-deftest pos-node/a-declaration-is-read-as-the-fixtures-say ()
  "Each fixture's methodology.yaml gives its declaration, or its kind of refusal."
  (let ((fixtures (pos-fixtures "pos-methodology")))
    (should (> (length fixtures) 5))
    (dolist (named fixtures)
      (ert-info ((car named) :prefix "fixture: ")
        (let-alist (cdr named)
          (if .refused
              (should (equal .refused
                             (condition-case err
                                 (progn (pos-node-read-methodology .yaml) "accepted")
                               (pos-node-refused (symbol-name (nth 1 err))))))
            (should (equal (pos-node-test-sorted .declaration)
                           (pos-node-test-sorted
                            (pos-node-read-methodology .yaml))))))))))

(ert-deftest pos-node/an-exclude-entry-is-a-name-or-a-path-read-exactly ()
  "An entry is a name at any depth or a path beneath the node, read exactly.
journal/ is a path of one name: the node's own journal and what lies
beneath it, not a journal deeper down, where the name journal is one
at any depth.  A name is matched letter for letter, so attic leaves
Attic to be walked, and only * stands for other text: ? and [ are
themselves."
  (let ((path (pos-node-exclusions
               `((exclude . ,(pos-node--exclusions ["journal/" "meta/notes/"])))))
        (name '("journal" "attic" "tmp*" "what?" "[draft]")))
    (should (equal '("journal/" "meta/notes") path))
    (should (pos-node-unwalked-p "journal" path))
    (should (pos-node-unwalked-p "journal/2026" path))
    (should-not (pos-node-unwalked-p "a/b/journal" path))
    (should (pos-node-unwalked-p "meta/notes/old" path))
    (should (pos-node-unwalked-p "a/b/journal" name))
    (should (pos-node-unwalked-p "a/attic" name))
    (should-not (pos-node-unwalked-p "a/Attic" name))
    (should (pos-node-unwalked-p "tmpfiles" name))
    (should-not (pos-node-unwalked-p "a/Tmpfiles" name))
    (should (pos-node-unwalked-p "what?" name))
    (should-not (pos-node-unwalked-p "whats" name))
    (should (pos-node-unwalked-p "[draft]" name))
    (should-not (pos-node-unwalked-p "d" name))))

(provide 'pos-node-test)
;;; pos-node-test.el ends here
