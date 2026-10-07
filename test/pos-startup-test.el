;;; pos-startup-test.el --- Tests for pos-startup.el  -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'pos-startup)
(require 'pos-test-support)

(defun pos-startup-test-day (days)
  "Return the Org date DAYS from today, as <YYYY-MM-DD>."
  (format-time-string "<%Y-%m-%d>" (time-add nil (days-to-time days))))

(defmacro pos-startup-test-with-tree (&rest body)
  "Evaluate BODY with `root' holding a tree of every kind of case."
  (declare (indent 0))
  `(pos-test-with-files root
       `(("intray.org" . "* Unsorted\n** NEXT Answer the letter\n** TODO Sort the shelf\n")
         ("projects/alpha/project.org"
          . ,(concat ":PROPERTIES:\n:STATUS:   COMMITTED\n:END:\n#+TITLE: Alpha\n\n"
                     "* NEXT Draft the outline\n"
                     "* TODO Review Alpha :review:\nSCHEDULED: "
                     (pos-startup-test-day -3) "\n"))
         ("projects/alpha/notes.org" . "* NEXT Read [[https://example.org][the paper]]\n")
         ("projects/beta/project.org"
          . ,(concat ":PROPERTIES:\n:STATUS:   WIP\n:END:\n#+TITLE: Beta\n\n"
                     "* TODO Book the room\nSCHEDULED: " (pos-startup-test-day 5) "\n"
                     "* TODO File the return\nDEADLINE: "
                     (substring (pos-startup-test-day 40) 0 -1) " -3d>\n"
                     "* DONE Old review :review:\nSCHEDULED: "
                     (pos-startup-test-day 2) "\n"))
         ("projects/gamma/project.org"
          . ":PROPERTIES:\n:STATUS:   COMPLETE\n:END:\n#+TITLE: Gamma\n")
         ("responsibilities/home/index.org"
          . ,(concat "* TODO Review the home :review:\nSCHEDULED: "
                     (pos-startup-test-day 3) "\n"))
         ("responsibilities/garden/index.org" . "* TODO Prune the hedge\n")
         ("responsibilities/home/projects/roof/project.org"
          . ":PROPERTIES:\n:STATUS:   WIP\n:END:\n* NEXT Call the roofer\n")
         ("health/.clanka/config.yml" . "pos: 2\nprojects: projects/\n")
         ("health/intray.org"
          . ,(concat "* Unsorted\n** TODO Book [[https://example.org][the dentist]]\n"
                     "** DONE Buy floss\n"
                     "* TODO Review health :review:\nSCHEDULED: "
                     (pos-startup-test-day 2) "\n"))
         ("health/projects/checkup.org"
          . ":PROPERTIES:\n:STATUS: COMMITTED\n:END:\n* TODO Find the card\n")
         ("health/teeth/.pos/config.yaml" . "pos: 2\nprojects: projects/\n")
         ("health/teeth/intray.org" . "* Unsorted\n")
         ("tools/widget/.clanka/config.yml" . "pos: 2\nmethodologies: methodologies/\n")
         ("tools/widget/notes.org" . "* TODO Oil the widget\n")
         ("archives/old.org" . "* NEXT Archived\n")
         ("projects/alpha/attic/draft.org" . "* NEXT In the attic\n")
         ("_tmp/scratch.org" . "* NEXT Generated\n")
         (".hidden/secret.org" . "* NEXT Hidden\n"))
     ,@body))

(defun pos-startup-test-lines (text)
  "Return the item lines of TEXT, each as (LABEL . REST), trimmed."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`  \\(\\S-+\\) +\\(.*\\)\\'" line)
                    (cons (match-string 1 line) (string-trim (match-string 2 line)))))
                (split-string text "\n"))))

(ert-deftest pos-startup-files/leaves-out-archives-attics-and-ephemera ()
  "Archives, attics, underscore and hidden directories are not read."
  (pos-startup-test-with-tree
    (should (equal (mapcar (lambda (file) (file-relative-name file root))
                           (pos-startup-files root))
                   '("health/intray.org"
                     "health/projects/checkup.org"
                     "health/teeth/intray.org"
                     "intray.org"
                     "projects/alpha/notes.org"
                     "projects/alpha/project.org"
                     "projects/beta/project.org"
                     "projects/gamma/project.org"
                     "responsibilities/garden/index.org"
                     "responsibilities/home/index.org"
                     "responsibilities/home/projects/roof/project.org"
                     "tools/widget/notes.org")))))

(ert-deftest pos-startup-owner/is-the-last-scope-the-path-names ()
  "A project within a responsibility is a project; the root owns the rest."
  (let ((root "/r/"))
    (should (equal (pos-startup--owner "/r/projects/alpha/project.org" root)
                   '(project . "projects/alpha")))
    (should (equal (pos-startup--owner "/r/responsibilities/home/index.org" root)
                   '(responsibility . "responsibilities/home")))
    (should (equal (pos-startup--owner "/r/responsibilities/home/projects/roof/project.org" root)
                   '(project . "responsibilities/home/projects/roof")))
    (should (equal (pos-startup--owner "/r/projects/solo.org" root)
                   '(project . "projects/solo")))
    (should-not (pos-startup--owner "/r/intray.org" root))))

(ert-deftest pos-startup-owner/a-configured-directory-is-a-responsibility ()
  "A configuration that says where projects belong makes a responsibility."
  (pos-startup-test-with-tree
    (should (equal (pos-startup--configured root) '("health" "health/teeth")))
    (should (equal (pos-startup--owner (concat root "health/intray.org") root)
                   '(responsibility . "health")))
    (should (equal (pos-startup--owner (concat root "health/teeth/intray.org") root)
                   '(responsibility . "health/teeth")))
    (should (equal (pos-startup--owner (concat root "health/projects/checkup.org") root)
                   '(project . "health/projects/checkup")))
    (should-not (pos-startup--owner (concat root "tools/widget/notes.org") root))))

(ert-deftest pos-startup-view/next-labels-each-item-by-its-scope ()
  "The label drops projects segments and project.org; a link is its text."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-view root "next")))
      (should (string-prefix-p "NEXT items\n" text))
      (should (equal (pos-startup-test-lines text)
                     '(("intray" . "NEXT Answer the letter")
                       ("alpha/notes" . "NEXT Read the paper")
                       ("alpha" . "NEXT Draft the outline")
                       ("responsibilities/home/roof" . "NEXT Call the roofer")))))))

(ert-deftest pos-startup-view/scheduled-leaves-out-reviews-and-deadlines ()
  "Scheduled items within the horizon, without reviews or deadlines."
  (pos-startup-test-with-tree
    (let ((lines (pos-startup-test-lines (pos-startup-view root "scheduled"))))
      (should (equal (mapcar #'car lines) '("beta")))
      (should (string-match-p "Book the room" (cdar lines))))))

(ert-deftest pos-startup-view/deadlines-shows-every-open-one ()
  "A deadline beyond its own warning period is still listed."
  (pos-startup-test-with-tree
    (let ((lines (pos-startup-test-lines (pos-startup-view root "deadlines"))))
      (should (equal (mapcar #'car lines) '("beta")))
      (should (string-match-p "Due in  40 days: +TODO File the return" (cdar lines))))))

(ert-deftest pos-startup-view/reviews-are-late-or-due-and-apart-by-kind ()
  "Open review headings, projects' and responsibilities' in two lists."
  (pos-startup-test-with-tree
    (let* ((text (pos-startup-view root "reviews"))
           (parts (split-string text "\n\n" t)))
      (should (= 2 (length parts)))
      (should (string-prefix-p "Project reviews, late or due in the next 7 days\n"
                               (nth 0 parts)))
      (should (string-match-p "alpha +3 days late: +TODO Review Alpha" (nth 0 parts)))
      (should-not (string-match-p "Old review" text))
      (should (string-prefix-p "Responsibility reviews, late or due in the next 7 days\n"
                               (nth 1 parts)))
      (should (string-match-p "responsibilities/home/index +Scheduled: +TODO Review the home"
                              (nth 1 parts)))
      (should (string-match-p "health/intray +Scheduled: +TODO Review health"
                              (nth 1 parts))))))

(ert-deftest pos-startup-view/reviews-to-schedule-names-active-scopes-without-one ()
  "Active projects and responsibilities with no open, scheduled review."
  (pos-startup-test-with-tree
    (let ((parts (split-string (pos-startup-view root "reviews-to-schedule") "\n\n" t)))
      (should (equal (mapcar #'car (pos-startup-test-lines (nth 0 parts)))
                     '("health/projects/checkup" "projects/beta"
                       "responsibilities/home/projects/roof")))
      (should (equal (split-string (nth 1 parts) "\n" t " +")
                     '("Responsibilities with a review to be scheduled"
                       "health/teeth" "responsibilities/garden"))))))

(ert-deftest pos-startup-view/intray-lists-what-is-captured-and-not-placed ()
  "Open items under Unsorted in each intray, labelled by its scope."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-view root "intray")))
      (should (string-prefix-p "Intray, to be placed\n" text))
      (should (equal (pos-startup-test-lines text)
                     '(("health/intray" . "TODO Book the dentist")
                       ("intray" . "NEXT Answer the letter")
                       ("intray" . "TODO Sort the shelf")))))))

(ert-deftest pos-startup-view/an-empty-view-says-none ()
  "A view with no item, and each empty list of a view, says so."
  (pos-test-with-files root '(("intray.org" . "* Unsorted\n"))
    (dolist (view '("next" "scheduled" "deadlines" "intray" "all"))
      (should (string-suffix-p "\n  (none)\n" (pos-startup-view root view))))
    (should (= 2 (length (split-string (pos-startup-view root "reviews")
                                       "  (none)\n" t))))
    (should (= 2 (length (split-string (pos-startup-view root "reviews-to-schedule")
                                       "  (none)\n" t))))))

(ert-deftest pos-startup-report/prompts-then-the-default-views ()
  "The prompts come first; every TODO item is listed only when asked for."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-report root)))
      (should (string-prefix-p pos-startup-prompts text))
      (should (string-match-p "^Files read: 12$" text))
      (dolist (title '("NEXT items" "Scheduled items, next 14 days" "Deadlines, all open"
                       "Project reviews, late" "Projects with a review to be scheduled"
                       "Intray, to be placed"))
        (should (string-match-p (concat "^" title) text)))
      (should-not (string-match-p "^All TODO items" text)))
    (let ((text (pos-startup-report root '("all"))))
      (should (string-match-p "^All TODO items" text))
      (should (string-match-p "Prune the hedge" text))
      (should-not (string-match-p "^NEXT items" text)))))

(ert-deftest pos-startup-report/an-unknown-view-is-refused ()
  "A view not in `pos-startup-views' is an error, before anything is read."
  (should-error (pos-startup-report "/nonexistent/" '("bogus")) :type 'user-error))

(provide 'pos-startup-test)
;;; pos-startup-test.el ends here
