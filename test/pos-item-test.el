;;; pos-item-test.el --- Tests for pos-item.el  -*- lexical-binding: t -*-

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
;; tasks.org, at a root with no configuration.

;;; Code:

(require 'ert)
(require 'pos-item)
(require 'pos-test-support)

(defmacro pos-item-test-with-tasks (contents &rest body)
  "Evaluate BODY with `root' a directory with a tasks.org of CONTENTS."
  (declare (indent 1))
  `(pos-test-with-files root `(("tasks.org" . ,,contents))
     ,@body))

(defun pos-item-test-tasks (root)
  "Return the saved contents of ROOT's tasks.org."
  (pos-test-file-string (expand-file-name "tasks.org" root)))

(ert-deftest pos-item/a-scheduled-date-is-set-on-an-item-named-by-its-id ()
  "An item named by its ID is given a scheduled date on its planning line.
The line goes where Org puts it, above the property drawer.  The item
is returned as it is afterwards, with its timestamp."
  (pos-item-test-with-tasks
      "* TODO Call the roofer\n:PROPERTIES:\n:ID:       roofer\n:END:\nBody.\n"
    (should (equal (list (expand-file-name "tasks.org" root) 1 "TODO"
                         "Call the roofer" "<2031-03-04 Tue>")
                   (pos-set-date root "roofer" "scheduled" "2031-03-04")))
    (should (equal (concat "* TODO Call the roofer\nSCHEDULED: <2031-03-04 Tue>\n"
                           ":PROPERTIES:\n:ID:       roofer\n:END:\nBody.\n")
                   (pos-item-test-tasks root)))))

(ert-deftest pos-item/a-deadline-is-set-beside-a-scheduled-date ()
  "A deadline is set on an item named by file and line.
The item keeps its scheduled date."
  (pos-item-test-with-tasks
      "* Tasks\n** NEXT File the return\nSCHEDULED: <2031-03-04 Tue>\n"
    (should (equal "<2031-03-31 Mon>"
                   (nth 4 (pos-set-date root "tasks.org:2" "deadline" "2031-03-31"))))
    (should (string-match-p
             (concat "\\`\\* Tasks\n\\*\\* NEXT File the return\n"
                     "\\(DEADLINE: <2031-03-31 Mon> SCHEDULED: <2031-03-04 Tue>"
                     "\\|SCHEDULED: <2031-03-04 Tue> DEADLINE: <2031-03-31 Mon>\\)\n\\'")
             (pos-item-test-tasks root)))))

(ert-deftest pos-item/none-removes-a-date ()
  "The date \"none\" removes the date.
The item then has no timestamp of that kind."
  (pos-item-test-with-tasks "* TODO Mow\nSCHEDULED: <2031-03-04 Tue>\n"
    (should-not (nth 4 (pos-set-date root "tasks.org:1" "scheduled" "none")))
    (should (equal "* TODO Mow\n" (pos-item-test-tasks root)))))

(ert-deftest pos-item/a-repeater-is-written-when-given-and-kept-when-not ()
  "A date with a repeater writes that repeater in place of the item's own.
A date without one keeps the repeater the item has, as Org does."
  (pos-item-test-with-tasks "* TODO Water\nSCHEDULED: <2031-03-04 Tue +1w>\n"
    (should (equal "<2031-03-05 Wed +2d>"
                   (nth 4 (pos-set-date root "tasks.org:1" "scheduled" "2031-03-05 +2d"))))
    (should (equal "<2031-03-07 Fri +2d>"
                   (nth 4 (pos-set-date root "tasks.org:1" "scheduled" "2031-03-07"))))
    (should (equal "* TODO Water\nSCHEDULED: <2031-03-07 Fri +2d>\n"
                   (pos-item-test-tasks root)))))

(ert-deftest pos-item/a-date-that-is-no-date-is-refused-and-nothing-written ()
  "A date that is no date is refused, and nothing is written.
So are a day no month has and an unknown kind of date."
  (pos-item-test-with-tasks "* TODO Mow\n"
    (should-error (pos-set-date root "tasks.org:1" "scheduled" "tomorrow") :type 'user-error)
    (should-error (pos-set-date root "tasks.org:1" "scheduled" "2031-02-30") :type 'user-error)
    (should-error (pos-set-date root "tasks.org:1" "closed" "2031-03-04") :type 'user-error)
    (should (equal "* TODO Mow\n" (pos-item-test-tasks root)))))

(ert-deftest pos-item/a-date-logs-nothing ()
  "Changing a date an item has writes no LOGBOOK record."
  (pos-item-test-with-tasks "* TODO Mow\nDEADLINE: <2031-03-04 Tue>\n"
    (pos-set-date root "tasks.org:1" "deadline" "2031-03-11")
    (should (equal "* TODO Mow\nDEADLINE: <2031-03-11 Tue>\n" (pos-item-test-tasks root)))))

(ert-deftest pos-item/tags-are-added-and-removed ()
  "A change +TAG adds the tag and -TAG removes it; the item's tags are returned.
Adding a tag the item has and removing one it lacks change nothing."
  (pos-item-test-with-tasks "* TODO Call the roofer :home:\n* NEXT Other\n"
    (should (equal '("home" "phone")
                   (nth 4 (pos-set-tags root "tasks.org:1" '("+phone" "+home" "-errand")))))
    (should (equal '("phone")
                   (nth 4 (pos-set-tags root "tasks.org:1" '("-home")))))
    (should (string-match-p "\\`\\* TODO Call the roofer +:phone:\n\\* NEXT Other\n\\'"
                            (pos-item-test-tasks root)))))

(ert-deftest pos-item/removing-the-last-tag-leaves-the-heading-bare ()
  "An item whose only tag is removed has no tags and no trailing space."
  (pos-item-test-with-tasks "* TODO Mow :home:\n"
    (should-not (nth 4 (pos-set-tags root "tasks.org:1" '("-home"))))
    (should (equal "* TODO Mow\n" (pos-item-test-tasks root)))))

(ert-deftest pos-item/a-change-that-is-not-a-tag-is-refused ()
  "A change that is not +TAG or -TAG is refused.
So are a tag with a space or a colon, and no change at all."
  (pos-item-test-with-tasks "* TODO Mow\n"
    (should-error (pos-set-tags root "tasks.org:1" '("home")) :type 'user-error)
    (should-error (pos-set-tags root "tasks.org:1" '("+at home")) :type 'user-error)
    (should-error (pos-set-tags root "tasks.org:1" '("+a:b")) :type 'user-error)
    (should-error (pos-set-tags root "tasks.org:1" nil) :type 'user-error)
    (should (equal "* TODO Mow\n" (pos-item-test-tasks root)))))

(ert-deftest pos-item/a-line-that-is-no-item-is-refused ()
  "A heading with no state is not an item, for a date as for a state."
  (pos-item-test-with-tasks "* Tasks\n** TODO Mow\n"
    (should-error (pos-set-date root "tasks.org:1" "scheduled" "2031-03-04")
                  :type 'user-error)
    (should-error (pos-set-tags root "tasks.org:1" '("+home")) :type 'user-error)))

(provide 'pos-item-test)
;;; pos-item-test.el ends here
