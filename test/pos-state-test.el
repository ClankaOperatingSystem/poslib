;;; pos-state-test.el --- Tests for pos-state.el  -*- lexical-binding: t -*-

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
;; Each test is named for the rule it pins.  The tree is one file,
;; tasks.org, at a root with no configuration.  Dates are relative to
;; today, so a repeating item is tested as its date falls.

;;; Code:

(require 'ert)
(require 'pos-state)
(require 'pos-test-support)

(defconst pos-state-test-stamp
  "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [[:alpha:]]+ [0-9]\\{2\\}:[0-9]\\{2\\}\\]"
  "A regexp matching an inactive timestamp with a time.")

(defun pos-state-test-day (days)
  "Return the date DAYS from today, as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (time-add nil (days-to-time days))))

(defmacro pos-state-test-with-tasks (contents &rest body)
  "Evaluate BODY with `root' a directory with a tasks.org of CONTENTS."
  (declare (indent 1))
  `(pos-test-with-files root `(("tasks.org" . ,,contents))
     ,@body))

(defun pos-state-test-tasks (root)
  "Return the saved contents of ROOT's tasks.org."
  (pos-test-file-string (expand-file-name "tasks.org" root)))

(ert-deftest pos-state/an-item-named-by-its-id-is-set-and-the-change-logged ()
  "An item is named by its ID; the change and its note go in LOGBOOK.
The record is Org's own for a state change.  An open state adds no
closing date.  The item is returned as it is afterwards."
  (pos-state-test-with-tasks
      "* TODO Call the roofer\n:PROPERTIES:\n:ID:       roofer\n:END:\nBody.\n* NEXT Other\n"
    (should (equal (list (expand-file-name "tasks.org" root) 1 "WAITING"
                         "Call the roofer")
                   (pos-set-state root "roofer" "WAITING" " Asked by phone ")))
    (should (string-match-p
             (concat "\\`\\* WAITING Call the roofer\n"
                     ":PROPERTIES:\n:ID:       roofer\n:END:\n"
                     ":LOGBOOK:\n"
                     "- State \"WAITING\" +from \"TODO\" +" pos-state-test-stamp
                     " \\\\\\\\\n  Asked by phone\n"
                     ":END:\nBody.\n\\* NEXT Other\n\\'")
             (pos-state-test-tasks root)))))

(ert-deftest pos-state/an-item-named-by-file-and-line-is-set ()
  "An item is named by its file, relative to the root, and its heading's line.
With no note, the record is the change and its time."
  (pos-state-test-with-tasks "* Tasks\n** TODO First\n** NEXT Second\n"
    (should (equal "SOMEDAY" (nth 2 (pos-set-state root "tasks.org:3" "SOMEDAY"))))
    (should (string-match-p
             (concat "\\`\\* Tasks\n\\*\\* TODO First\n\\*\\* SOMEDAY Second\n"
                     ":LOGBOOK:\n- State \"SOMEDAY\" +from \"NEXT\" +"
                     pos-state-test-stamp "\n:END:\n\\'")
             (pos-state-test-tasks root)))))

(ert-deftest pos-state/a-done-state-adds-the-closing-date ()
  "DONE and CANCELLED add CLOSED, as `org-log-done' set to time has it."
  (dolist (state '("DONE" "CANCELLED"))
    (ert-info ((format "set to %s" state))
      (pos-state-test-with-tasks "* TODO Pay the bill\n"
        (pos-set-state root "tasks.org:1" state)
        (should (string-match-p
                 (format "\\`\\* %s Pay the bill\nCLOSED: %s\n:LOGBOOK:\n"
                         state pos-state-test-stamp)
                 (pos-state-test-tasks root)))))))

(ert-deftest pos-state/a-repeating-item-set-done-is-open-on-its-next-date ()
  "A repeating item set to DONE has its date moved on by its own repeater.
Its state is put back to TODO, no CLOSED is added, LAST_REPEAT is
written and the record says DONE.  Here the item was due yesterday
and repeats weekly from the date it was due, so it is next due six
days from today."
  (pos-state-test-with-tasks
      (format "* TODO Weekly review :review:\nSCHEDULED: <%s ++1w>\n"
              (pos-state-test-day -1))
    (should (equal "TODO" (nth 2 (pos-set-state root "tasks.org:1" "DONE"))))
    (let ((text (pos-state-test-tasks root)))
      (should (string-match-p
               (format "^SCHEDULED: <%s [[:alpha:]]+ \\+\\+1w>$"
                       (pos-state-test-day 6))
               text))
      (should (string-match-p "^\\* TODO Weekly review" text))
      (should-not (string-match-p "CLOSED" text))
      (should (string-match-p (concat "^:LAST_REPEAT: " pos-state-test-stamp) text))
      (should (string-match-p "^- State \"DONE\" +from \"TODO\"" text)))))

(ert-deftest pos-state/what-cannot-be-set-is-refused-and-the-file-unchanged ()
  "A refusal is a `user-error' and writes nothing.
Refused: a state not in `pos-todo-keywords'; the state the item has;
an ID no heading has; an ID two headings have; a line that is not an
item's heading; a file the tree does not read; a note of two lines."
  (let ((tasks (concat "* TODO One\n:PROPERTIES:\n:ID:       twin\n:END:\n"
                       "* TODO Two\n:PROPERTIES:\n:ID:       twin\n:END:\n"
                       "* Plain heading\n")))
    (pos-state-test-with-tasks tasks
      (dolist (arguments '(("tasks.org:1" "BLOCKED")
                           ("tasks.org:1" "TODO")
                           ("absent" "DONE")
                           ("twin" "DONE")
                           ("tasks.org:2" "DONE")
                           ("tasks.org:9" "DONE")
                           ("other.org:1" "DONE")
                           ("tasks.org:1" "DONE" "two\nlines")))
        (ert-info ((format "%S" arguments))
          (should-error (apply #'pos-set-state root arguments) :type 'user-error)))
      (should (equal tasks (pos-state-test-tasks root))))))

(provide 'pos-state-test)
;;; pos-state-test.el ends here
