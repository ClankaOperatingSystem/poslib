;;; pos-refile-test.el --- Tests for pos-refile.el  -*- lexical-binding: t -*-

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

;; The specification of pos-refile.el, as tests.  Each test is named
;; by the rule it pins, as a sentence.
;;
;; The words the rules use.  The "root" is the repository the tools
;; work on.  The "corpus" is the Org files beneath the root that its
;; configurations allow, at any depth, as pos-corpus.el walks them:
;; the files every command reads.  The commands that write, the sweep
;; and the fixers, write only the corpus files of the root's own
;; repository, never those of a repository mounted within it.  The
;; "intray" is the root's intray.org, where new and salvaged tasks
;; wait to be filed.
;;
;; Pure rules, such as excerpts, are tested on values alone; the rest
;; through files in a temporary root.  Run: make test.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'pos)
(require 'pos-refile)
(require 'pos-test-support)

;;;; Excerpts

(ert-deftest pos-refile/an-excerpt-is-the-first-line-of-the-body-proper ()
  "An excerpt is a subtree's first body line, after planning and drawer lines.
The heading, a SCHEDULED, DEADLINE or CLOSED line and the lines of a
property drawer, which begin with a colon, are not what the entry
says; blank lines are passed over and the line is trimmed.  An entry
with no body has an empty excerpt."
  (should (equal "book it"
                 (pos--excerpt (pos-test-lines "** TODO Dentist checkup"
                                               "SCHEDULED: <2026-09-14 Mon>"
                                               ":PROPERTIES:"
                                               ":ID: a"
                                               ":END:"
                                               ""
                                               "  book it  "
                                               "*** sub"))))
  (should (equal "" (pos--excerpt "** TODO Dentist checkup")))
  (should (equal "" (pos--excerpt "** TODO x\nDEADLINE: <2026-09-14 Mon>"))))

(ert-deftest pos-refile/an-excerpt-fits-a-table-cell ()
  "An excerpt has its bars made slashes and is cut to sixty columns.
A bar would end the plan table's cell; a long line would push the
other columns off the screen.  The cut is marked with an ellipsis."
  (should (equal "hinge / latch" (pos--excerpt "* TODO x\nhinge | latch")))
  (should (equal (concat (make-string 59 ?a) "…")
                 (pos--excerpt (concat "* TODO x\n" (make-string 70 ?a))))))

;;;; Refile

(ert-deftest pos-refile/the-first-matching-refile-rule-wins ()
  "A heading is suggested the file of the first rule it matches, or none.
Rules match without regard to case; a heading no rule matches has no
suggestion, and is left for a person."
  (pos-test-configured
    (should (equal "work/work-projects.org"
                   (pos-refile-suggest "Client invoice query")))
    (should (equal "body/body-projects.org"
                   (pos-refile-suggest "Dentist checkup")))
    (should-not (pos-refile-suggest "Visitor arrives"))))

(ert-deftest pos-refile/the-refile-plan-has-one-row-per-intray-entry ()
  "The refile plan has a row per level-two intray entry, with a suggestion.
A matched entry is a move to the file its rule names, written as the
rule has it; the rest are questions.  Each entry is quoted whole below
the table in an example block, its headings escaped, so the plan, a
file of the corpus while it sits in the root, is not itself a file of
duplicate tasks."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . ,(pos-test-lines "* Unsorted"
                                           "** TODO Dentist checkup"
                                           "SCHEDULED: <2026-09-14 Mon>"
                                           "book it"
                                           "*** sub"
                                           "** Visitor arrives"
                                           "* Sorted"
                                           "** TODO Invoice query")))
      (let ((plan (expand-file-name "refile.org" root)))
        (with-temp-file plan (insert (pos-refile-plan root)))
        (should (equal '(("move" 2 "Dentist checkup"
                          "body/body-projects.org" "")
                         ("?" 6 "Visitor arrives" "" "")
                         ("move" 8 "Invoice query"
                          "work/work-projects.org" ""))
                       (pos-refile-read-plan plan)))
        (let ((text (pos-test-file-string plan)))
          (should (string-match-p "| book it *|" text))
          (should (string-match-p
                   (concat "^\\*\\* 2: Dentist checkup (under Unsorted)\n"
                           "#\\+begin_example\n,\\*\\* TODO Dentist checkup\n")
                   text))
          (should (string-match-p ",\\*\\*\\* sub" text)))
        (should-not (pos-lint-duplicate-tasks root))))))

(ert-deftest pos-refile/a-move-lands-under-the-named-heading-or-at-the-end ()
  "A moved entry goes under the heading named, or at the target's end.
It is cut from the intray with its subtree and pasted at the level
below its new parent, or at level one when no heading is named.  A
row marked skip is left in place and counted."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-test-lines "* Unsorted"
                                         "** TODO Dentist checkup"
                                         "body"
                                         "*** sub"
                                         "** TODO Invoice query"
                                         "** keep me"))
        ("body/body-projects.org" . "* Appointments\n** existing\n* Other\n")
        ("work/work-projects.org" . "* Admin\n")
        ("refile.org"
         . ,(pos-test-lines
             (concat "| move | 2 | Dentist checkup | body/body-projects.org"
                     " | Appointments |")
             "| move | 5 | Invoice query | work/work-projects.org | |"
             "| skip | 6 | keep me | | |")))
    (let ((plan (expand-file-name "refile.org" root)))
      (should (equal '(:moved 2 :left 1 :missing 0 :unwritable 0 :vanished 0)
                     (pos-refile-apply root plan)))
      (should (equal "* Unsorted\n** keep me\n"
                     (pos-test-text root "intray.org")))
      (should (equal (pos-test-lines "* Appointments"
                                     "** existing"
                                     "** TODO Dentist checkup"
                                     "body"
                                     "*** sub"
                                     "* Other")
                     (pos-test-text root "body/body-projects.org")))
      (should (equal "* Admin\n* TODO Invoice query\n"
                     (pos-test-text root "work/work-projects.org"))))))

(ert-deftest pos-refile/an-outline-path-targets-the-nested-heading-not-a-namesake ()
  "A slash-separated path names a nested heading, not a top-level namesake.
Each name in the path is sought below the one before, so a top-level
Housekeeping elsewhere in the file is not mistaken for the one under
Operations."
  (pos-test-with-files root
      `(("intray.org" . "* Unsorted\n** TODO Invoicing\n")
        ("work/work-areas.org" . ,(pos-test-lines "* Operations"
                                                  "** Housekeeping"
                                                  "*** existing"
                                                  "** Delivery"
                                                  "* Housekeeping"))
        ("refile.org"
         . ,(pos-test-lines
             (concat "| move | 2 | Invoicing | work/work-areas.org"
                     " | Operations/Housekeeping |"))))
    (let ((plan (expand-file-name "refile.org" root)))
      (should (equal '(:moved 1 :left 0 :missing 0 :unwritable 0 :vanished 0)
                     (pos-refile-apply root plan)))
      (should (equal (pos-test-lines "* Operations"
                                     "** Housekeeping"
                                     "*** existing"
                                     "*** TODO Invoicing"
                                     "** Delivery"
                                     "* Housekeeping")
                     (pos-test-text root "work/work-areas.org"))))))

(ert-deftest pos-refile/a-missing-target-or-a-moved-line-is-counted-not-fatal ()
  "A missing target, or a heading no longer at its line, is counted, not fatal.
A target file that does not exist, or a path not in it, counts as
missing; a row whose heading is not at its line counts as vanished.
Nothing moves, and the intray is unchanged."
  (pos-test-with-files root
      `(("intray.org" . "* Unsorted\n** TODO a\n** TODO b\n** TODO c\n")
        ("body/body-projects.org" . "* Appointments\n")
        ("refile.org"
         . ,(pos-test-lines
             "| move | 2 | a | nowhere/nowhere-projects.org | |"
             "| move | 3 | b | body/body-projects.org | Surgery |"
             "| move | 4 | zzz | body/body-projects.org | |")))
    (let ((plan (expand-file-name "refile.org" root)))
      (should (equal '(:moved 0 :left 0 :missing 2 :unwritable 0 :vanished 1)
                     (pos-refile-apply root plan)))
      (should (equal "* Unsorted\n** TODO a\n** TODO b\n** TODO c\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos-refile/a-target-in-a-child-repository-is-unwritable-and-left ()
  "A row whose target is in a child repository is left and counted.
The child is configured, so its file is read and is no missing
target; but the root may not write it, so the entry stays in the
intray and the row counts as unwritable."
  (pos-test-with-files root
      `(("intray.org" . "* Unsorted\n** TODO a\n")
        (".pos/config.yaml" . ,pos-test-root-config)
        ("child/.git/HEAD" . ,pos-test-git-head)
        ("child/.clanka/config.yml" . ,pos-test-child-config)
        ("child/projects/p.org" . "* Inbox\n")
        ("refile.org" . "| move | 2 | a | child/projects/p.org | Inbox |\n"))
    (let ((plan (expand-file-name "refile.org" root)))
      (should (equal '(:moved 0 :left 0 :missing 0 :unwritable 1 :vanished 0)
                     (pos-refile-apply root plan)))
      (should (equal "* Unsorted\n** TODO a\n"
                     (pos-test-text root "intray.org")))
      (should (equal "* Inbox\n"
                     (pos-test-text root "child/projects/p.org"))))))

(ert-deftest pos-refile/a-dry-run-refile-moves-nothing ()
  "A dry run counts what it would move and changes no file."
  (pos-test-with-files root
      '(("intray.org" . "* Unsorted\n** TODO a\n")
        ("body/body-projects.org" . "* Appointments\n")
        ("refile.org"
         . "| move | 2 | a | body/body-projects.org | Appointments |\n"))
    (let ((plan (expand-file-name "refile.org" root)))
      (should (equal '(:moved 1 :left 0 :missing 0 :unwritable 0 :vanished 0)
                     (pos-refile-apply root plan t)))
      (should (equal "* Unsorted\n** TODO a\n"
                     (pos-test-text root "intray.org"))))))

(provide 'pos-refile-test)
;;; pos-refile-test.el ends here
