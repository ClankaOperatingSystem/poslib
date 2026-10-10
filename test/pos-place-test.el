;;; pos-place-test.el --- Tests for pos-place.el  -*- lexical-binding: t -*-

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
;; Each test is named for the rule it pins.  The tree is two files,
;; intray.org and tasks.org, at a root with no configuration.

;;; Code:

(require 'ert)
(require 'pos-place)
(require 'pos-test-support)

(defconst pos-place-test-stamp
  "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [[:alpha:]]+ [0-9]\\{2\\}:[0-9]\\{2\\}\\]"
  "A regexp matching an inactive timestamp with a time.")

(defconst pos-place-test-intray
  (concat "* Unsorted\n"
          "** TODO Call the roofer\n:PROPERTIES:\n:ID:       roofer\n:END:\n"
          "The gutter leaks.\n"
          "*** TODO Find the number\n"
          "** TODO Sort the shelf\n")
  "An intray with two items; the first has an ID, a body and a child.")

(defconst pos-place-test-tasks
  "* Tasks\n** NEXT Mend the house :home:\n*** DONE Buy nails\n* Notes\n"
  "A file of tasks; Mend the house has a state, a tag and a child.")

(defmacro pos-place-test-with-files (&rest body)
  "Evaluate BODY with `root' holding the intray and the file of tasks."
  (declare (indent 0))
  `(pos-test-with-files root `(("intray.org" . ,pos-place-test-intray)
                               ("tasks.org" . ,pos-place-test-tasks))
     ,@body))

(defun pos-place-test-file (root name)
  "Return the saved contents of NAME under ROOT."
  (pos-test-file-string (expand-file-name name root)))

(ert-deftest pos-place/an-item-is-moved-whole-to-the-end-of-a-headings-children ()
  "The item leaves its file and is the last child of the heading named.
Its properties, body and child go with it, at the level of the new
place.  The heading is named by its title alone, whatever state and
tags it has, here Tasks/Mend the house.  The item is returned as it
is afterwards."
  (pos-place-test-with-files
    (should (equal (list (expand-file-name "tasks.org" root) 4 "TODO"
                         "Call the roofer")
                   (pos-place root "roofer" "tasks.org" "Tasks/Mend the house")))
    (should (equal "* Unsorted\n** TODO Sort the shelf\n"
                   (pos-place-test-file root "intray.org")))
    (should (string-match-p
             (concat "\\`\\* Tasks\n\\*\\* NEXT Mend the house :home:\n"
                     "\\*\\*\\* DONE Buy nails\n"
                     "\\*\\*\\* TODO Call the roofer\n"
                     ":PROPERTIES:\n:ID:       roofer\n:END:\n"
                     ":LOGBOOK:\n\\(?:.*\n\\)+:END:\n"
                     "The gutter leaks\\.\n"
                     "\\*\\*\\*\\* TODO Find the number\n"
                     "\\* Notes\n\\'")
             (pos-place-test-file root "tasks.org")))))

(ert-deftest pos-place/the-move-is-recorded-with-the-place-it-came-from ()
  "The record is Org's for a refile, with the old file and outline path.
With a state, the item is set to it and that change is recorded too."
  (pos-place-test-with-files
    (should (equal "NEXT" (nth 2 (pos-place root "intray.org:8" "tasks.org"
                                            "Tasks" "NEXT"))))
    (should (string-match-p
             (concat "^\\*\\* NEXT Sort the shelf\n:LOGBOOK:\n"
                     "- Refiled on " pos-place-test-stamp " \\\\\\\\\n"
                     "  From intray\\.org, Unsorted\n"
                     "- State \"NEXT\" +from \"TODO\" +" pos-place-test-stamp "\n"
                     ":END:\n")
             (pos-place-test-file root "tasks.org")))))

(ert-deftest pos-place/an-item-is-placed-within-its-own-file ()
  "The heading may be in the item's own file; the file is saved once."
  (pos-place-test-with-files
    (should (equal (list (expand-file-name "intray.org" root) 8 "TODO"
                         "Find the number")
                   (pos-place root "intray.org:7" "intray.org" "Unsorted")))
    (should (string-match-p
             (concat "\\`\\* Unsorted\n\\*\\* TODO Call the roofer\n"
                     ":PROPERTIES:\n:ID:       roofer\n:END:\n"
                     "The gutter leaks\\.\n"
                     "\\*\\* TODO Sort the shelf\n"
                     "\\*\\* TODO Find the number\n:LOGBOOK:\n"
                     "- Refiled on .*\n  From intray\\.org, Unsorted, Call the roofer\n"
                     ":END:\n\\'")
             (pos-place-test-file root "intray.org")))))

(ert-deftest pos-place/what-cannot-be-placed-is-refused-and-the-files-unchanged ()
  "A refusal is a `user-error' and writes nothing.
Refused: a path the file does not have; a heading that is the item,
or within it; a state not in `pos-todo-keywords'; the state the item
has; a file the tree does not read; a line that is not an item's
heading."
  (pos-place-test-with-files
    (dolist (arguments '(("roofer" "tasks.org" "Tasks/Absent")
                         ("roofer" "tasks.org" "Mend the house")
                         ("roofer" "intray.org" "Unsorted/Call the roofer")
                         ("roofer" "intray.org"
                          "Unsorted/Call the roofer/Find the number")
                         ("roofer" "tasks.org" "Tasks" "BLOCKED")
                         ("roofer" "tasks.org" "Tasks" "TODO")
                         ("roofer" "other.org" "Tasks")
                         ("intray.org:1" "tasks.org" "Tasks")))
      (ert-info ((format "%S" arguments))
        (should-error (apply #'pos-place root arguments) :type 'user-error)))
    (should (equal pos-place-test-intray (pos-place-test-file root "intray.org")))
    (should (equal pos-place-test-tasks (pos-place-test-file root "tasks.org")))))

(ert-deftest pos-place/links-that-name-the-old-place-are-listed ()
  "A link by file and title, or by title within the file, is listed.
A link by ID is not, since the ID goes with the item, nor is a link
to a heading of the same title in another file.  The command prints
the item where it now is, then each link where it is after the move."
  (pos-test-with-files root
      `(("intray.org" . ,(concat pos-place-test-intray
                                 "See [[*Call the roofer]].\n"))
        ("tasks.org" . ,(concat pos-place-test-tasks
                                "[[file:intray.org::*Call the roofer][by file]]\n"
                                "[[id:roofer][by ID]]\n"
                                "[[file:tasks.org::*Call the roofer][elsewhere]]\n")))
    (should (equal (list (cons (expand-file-name "intray.org" root) 9)
                         (cons (expand-file-name "tasks.org" root) 5))
                   (pos-place-stale-links root "intray.org" "Call the roofer")))
    (let ((pos-directory root)
          (command-line-args-left (list "roofer" "tasks.org" "Tasks")))
      (should (equal (format (concat "%1$stasks.org:4: TODO Call the roofer\n"
                                     "Link to the old place: %1$sintray.org:3\n"
                                     "Link to the old place: %1$stasks.org:15\n")
                             root)
                     (with-output-to-string (pos-place-batch))))
      (should-not command-line-args-left))))

(provide 'pos-place-test)
;;; pos-place-test.el ends here
