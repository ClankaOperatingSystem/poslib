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
;;
;; Each test is named for the rule it pins, and its docstring says
;; which entry of the shared tree is there for that rule.  Dates are
;; relative to today, so the windows the views cover are tested as
;; they fall.

;;; Code:

(require 'ert)
(require 'pos-startup)
(require 'pos-test-support)

(defun pos-startup-test-day (days)
  "Return the Org date DAYS from today, as <YYYY-MM-DD>."
  (format-time-string "<%Y-%m-%d>" (time-add nil (days-to-time days))))

(defmacro pos-startup-test-with-tree (&rest body)
  "Evaluate BODY with `root' holding the shared tree of cases.
Each entry is there for a rule, which the docstring of the test that
pins it names."
  (declare (indent 0))
  `(pos-test-with-files root
       `(;; The root's own files: an intray, and names that are not read.
         ("intray.org" . "* Unsorted\n** NEXT Answer the letter\n** TODO Sort the shelf\n")
         ("notes.txt" . "Not an Org file.\n")
         (".#lock.org" . "* NEXT Locked\n")
         (".dotfile.org" . "* NEXT Dotted\n")
         ;; Projects at the root: alpha is active with a late review and
         ;; a second file; beta is active with items at the edges of the
         ;; scheduled window, an open deadline and a done one, and a done
         ;; review; gamma is complete; delta's STATUS sits on a heading.
         ("projects/alpha/project.org"
          . ,(concat ":PROPERTIES:\n:STATUS:   COMMITTED\n:END:\n#+TITLE: Alpha\n\n"
                     "* NEXT Draft the outline\n"
                     "* TODO Review Alpha :review:\nSCHEDULED: "
                     (pos-startup-test-day -3) "\n"))
         ("projects/alpha/notes.org" . "* NEXT Read [[https://example.org][the paper]]\n")
         ("projects/beta/project.org"
          . ,(concat ":PROPERTIES:\n:STATUS:   WIP\n:END:\n#+TITLE: Beta\n\n"
                     "* TODO Book the room\nSCHEDULED: " (pos-startup-test-day 5) "\n"
                     "* TODO Order the paint\nSCHEDULED: " (pos-startup-test-day 13) "\n"
                     "* TODO Hang the paper\nSCHEDULED: " (pos-startup-test-day 14) "\n"
                     "* TODO File the return\nDEADLINE: "
                     (substring (pos-startup-test-day 40) 0 -1) " -3d>\n"
                     "* DONE Pay the bill\nDEADLINE: " (pos-startup-test-day 10) "\n"
                     "* DONE Old review :review:\nSCHEDULED: "
                     (pos-startup-test-day 2) "\n"))
         ("projects/gamma/project.org"
          . ":PROPERTIES:\n:STATUS:   COMPLETE\n:END:\n#+TITLE: Gamma\n")
         ("projects/delta/project.org"
          . "#+TITLE: Delta\n* TODO Plan the delta\n:PROPERTIES:\n:STATUS:   COMMITTED\n:END:\n")
         ;; Responsibilities by directory name: home has a review in the
         ;; window and a project of its own; garden has none; kitchen's
         ;; and cellar's reviews fall at the edges of the reviews window.
         ("responsibilities/home/index.org"
          . ,(concat "* TODO Review the home :review:\nSCHEDULED: "
                     (pos-startup-test-day 3) "\n"))
         ("responsibilities/garden/index.org" . "* TODO Prune the hedge\n")
         ("responsibilities/kitchen/index.org"
          . ,(concat "* TODO Review the kitchen :review:\nSCHEDULED: "
                     (pos-startup-test-day 6) "\n"))
         ("responsibilities/cellar/index.org"
          . ,(concat "* TODO Review the cellar :review:\nSCHEDULED: "
                     (pos-startup-test-day 7) "\n"))
         ("responsibilities/home/projects/roof/project.org"
          . ":PROPERTIES:\n:STATUS:   WIP\n:END:\n* NEXT Call the roofer\n")
         ;; Responsibilities by configuration: health, and teeth within it.
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
         ;; Configured directories that are no responsibility: widget's
         ;; configuration is a project's; twice has two configurations.
         ("tools/widget/.clanka/config.yml" . "pos: 2\nmethodologies: methodologies/\n")
         ("tools/widget/notes.org" . "* TODO Oil the widget\n")
         ("twice/.pos/config.yaml" . "pos: 2\nprojects: projects/\n")
         ("twice/.clanka/config.yml" . "pos: 2\nprojects: projects/\n")
         ;; Directories that are not read, with Org files and
         ;; configurations inside them.
         ("archives/old.org" . "* NEXT Archived\n")
         ("archives/kept/.pos/config.yaml" . "pos: 2\nprojects: projects/\n")
         ("projects/alpha/attic/draft.org" . "* NEXT In the attic\n")
         ("_tmp/scratch.org" . "* NEXT Generated\n")
         (".hidden/secret.org" . "* NEXT Hidden\n")
         (".hidden/.pos/config.yaml" . "pos: 2\nprojects: projects/\n"))
     ,@body))

(defun pos-startup-test-lines (text)
  "Return the item lines of TEXT, each as (LABEL . REST), trimmed."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`  \\(\\S-+\\) +\\(.*\\)\\'" line)
                    (cons (match-string 1 line) (string-trim (match-string 2 line)))))
                (split-string text "\n"))))

;;;; Files

(ert-deftest pos-startup/archives-attics-hidden-and-underscore-directories-are-not-read ()
  "The files read are the Org files outside the directories set aside.
Those are archives/ and attic/, by `pos-startup-excluded-directories',
and any directory whose name begins with a dot or an underscore.  In
the tree, archives/old.org, projects/alpha/attic/draft.org,
_tmp/scratch.org and .hidden/secret.org are inside such directories;
every other Org file is read."
  (pos-startup-test-with-tree
    (should (equal (pos-test-relative (pos-startup-files root) root)
                   '("health/intray.org"
                     "health/projects/checkup.org"
                     "health/teeth/intray.org"
                     "intray.org"
                     "projects/alpha/notes.org"
                     "projects/alpha/project.org"
                     "projects/beta/project.org"
                     "projects/delta/project.org"
                     "projects/gamma/project.org"
                     "responsibilities/cellar/index.org"
                     "responsibilities/garden/index.org"
                     "responsibilities/home/index.org"
                     "responsibilities/home/projects/roof/project.org"
                     "responsibilities/kitchen/index.org"
                     "tools/widget/notes.org")))))

(ert-deftest pos-startup/a-file-not-named-as-an-org-file-is-not-read ()
  "Only files named *.org, not beginning with a dot or a hash, are read.
In the tree, notes.txt is not an Org file, .#lock.org is a lock file
and .dotfile.org is hidden; none is read."
  (pos-startup-test-with-tree
    (let ((files (pos-test-relative (pos-startup-files root) root)))
      (dolist (name '("notes.txt" ".#lock.org" ".dotfile.org"))
        (ert-info ((format "%s is not read" name))
          (should-not (member name files)))))))

;;;; Scopes

(ert-deftest pos-startup/a-configured-directory-is-a-responsibility ()
  "A responsibility is a directory whose configuration places its projects.
In the tree, health/.clanka/config.yml and health/teeth/.pos/config.yaml
each say where projects belong, so health and health/teeth are
responsibilities, one inside the other.  tools/widget's configuration
says where methodologies belong, a project's, so it is none.  The root
itself is never one of them."
  (pos-startup-test-with-tree
    (should (equal (pos-startup--configured root) '("health" "health/teeth")))))

(ert-deftest pos-startup/a-refused-configuration-makes-no-responsibility ()
  "A directory whose configuration is refused is no responsibility.
In the tree, twice/ holds both .pos/config.yaml and .clanka/config.yml,
which `pos-tree-config-file' refuses as two configurations; the
refusal is caught and twice is not a responsibility."
  (pos-startup-test-with-tree
    (should (file-directory-p (expand-file-name "twice" root)))
    (should-not (member "twice" (pos-startup--configured root)))))

(ert-deftest pos-startup/an-error-that-is-not-a-refusal-is-signalled ()
  "Only a refusal is caught when configurations are read; the rest propagate.
A refusal is `pos-tree-refused'.  Here reading any configuration
signals a plain error instead, and finding the responsibilities
signals it on."
  (pos-startup-test-with-tree
    (cl-letf (((symbol-function 'pos-tree-read-config)
               (lambda (_text) (error "The configuration cannot be read"))))
      (should-error (pos-startup--configured root) :type 'error))))

(ert-deftest pos-startup/a-configuration-in-a-directory-not-read-is-not-seen ()
  "A configuration inside a directory that is not read makes no scope.
In the tree, archives/kept/.pos/config.yaml and .hidden/.pos/config.yaml
would each make a responsibility were they read; archives/ and .hidden/
are not entered, so neither is one."
  (pos-startup-test-with-tree
    (let ((configured (pos-startup--configured root)))
      (should-not (member "archives/kept" configured))
      (should-not (member ".hidden" configured)))))

(ert-deftest pos-startup/the-owner-is-the-deepest-scope-on-the-path ()
  "A file belongs to the deepest scope its path names, else to the root.
A scope is a responsibility, which is a directory whose configuration
says where its projects belong or, today, a directory directly inside
one named responsibilities; or a project, which is what lies directly
inside a directory named projects.  A scope's path is given without
its .org, so a file directly inside projects/ or responsibilities/ is
today a scope of its own name.  Where a configured directory is itself
named projects, today the projects clause wins: what it holds are
projects, and the configuration is not seen.  No disk is read: the
configured responsibilities are given, relative to the root."
  (let ((root "/r/"))
    (pcase-dolist (`(,path ,configured ,expected)
                   '(;; The root owns what no scope claims.
                     ("intray.org" () nil)
                     ("tools/widget/notes.org" ("health" "health/teeth") nil)
                     ;; What is directly inside projects/ is a project,
                     ;; and owns everything beneath it.
                     ("projects/alpha/project.org" () (project . "projects/alpha"))
                     ("projects/alpha/notes/more.org" () (project . "projects/alpha"))
                     ("projects/solo.org" () (project . "projects/solo"))
                     ;; Today, what is directly inside responsibilities/
                     ;; is a responsibility.
                     ("responsibilities/home/index.org"
                      () (responsibility . "responsibilities/home"))
                     ("responsibilities/foo.org"
                      () (responsibility . "responsibilities/foo"))
                     ("responsibilities/home/projects/roof/project.org"
                      () (project . "responsibilities/home/projects/roof"))
                     ;; A configured directory, and one nested in another.
                     ("health/intray.org" ("health" "health/teeth")
                      (responsibility . "health"))
                     ("health/teeth/intray.org" ("health" "health/teeth")
                      (responsibility . "health/teeth"))
                     ("health/projects/checkup.org" ("health" "health/teeth")
                      (project . "health/projects/checkup"))
                     ;; A configured directory named projects: the
                     ;; projects clause wins today.
                     ("projects/alpha/project.org" ("projects")
                      (project . "projects/alpha"))
                     ("projects/notes.org" ("projects") (project . "projects/notes"))
                     ("health/projects/checkup.org" ("health" "health/projects")
                      (project . "health/projects/checkup"))))
      (ert-info ((format "%s, with %S configured" path configured))
        (should (equal (pos-startup--owner (concat root path) root configured)
                       expected))))))

(ert-deftest pos-startup/a-status-on-a-heading-is-not-the-files-status ()
  "A file's STATUS is the property it has before its first heading.
In the tree, projects/delta/project.org carries STATUS COMMITTED on a
heading instead, so the file has no status and delta is not active."
  (pos-startup-test-with-tree
    (should (equal "COMMITTED"
                   (pos-startup--file-status
                    (expand-file-name "projects/alpha/project.org" root))))
    (should-not (pos-startup--file-status
                 (expand-file-name "projects/delta/project.org" root)))))

;;;; Views

(ert-deftest pos-startup/the-next-view-labels-each-item-by-its-scope ()
  "The next view lists each open NEXT item, labelled by its scope.
The label is the file's directory without its projects segments,
then the file's base name unless that is project; a link is shown as
its description."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-view root "next")))
      (should (string-prefix-p "NEXT items\n" text))
      (should (equal (pos-startup-test-lines text)
                     '(("intray" . "NEXT Answer the letter")
                       ("alpha/notes" . "NEXT Read the paper")
                       ("alpha" . "NEXT Draft the outline")
                       ("responsibilities/home/roof" . "NEXT Call the roofer")))))))

(ert-deftest pos-startup/the-scheduled-view-leaves-out-reviews-and-deadlines ()
  "The scheduled view lists scheduled items, not reviews or deadlines.
Reviews and deadlines have views of their own.  In the tree, beta's
Book the room is scheduled in the window; its deadline, File the
return, and the reviews of alpha, home, health and kitchen are not
listed."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-view root "scheduled")))
      (should (string-match-p "Book the room" text))
      (should-not (string-match-p "File the return" text))
      (should-not (string-match-p "Review" text)))))

(ert-deftest pos-startup/the-scheduled-window-is-today-and-the-days-after-it ()
  "The scheduled view covers `pos-startup-scheduled-days' days from today.
Today is the first of the 14, so day 13 is the last day in the window
and day 14 is the first outside it.  In the tree, beta's Order the
paint is scheduled on day 13 and Hang the paper on day 14."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-view root "scheduled")))
      (should (string-match-p "Order the paint" text))
      (should-not (string-match-p "Hang the paper" text))
      (should (equal (mapcar #'car (pos-startup-test-lines text)) '("beta" "beta"))))))

(ert-deftest pos-startup/the-deadlines-view-lists-every-open-deadline ()
  "The deadlines view lists each open deadline, however far off it is.
In the tree, beta's File the return is due in 40 days with a warning
period of 3, which would hide it from an ordinary agenda."
  (pos-startup-test-with-tree
    (let ((lines (pos-startup-test-lines (pos-startup-view root "deadlines"))))
      (should (equal (mapcar #'car lines) '("beta")))
      (should (string-match-p "Due in  40 days: +TODO File the return" (cdar lines))))))

(ert-deftest pos-startup/a-done-deadline-is-not-listed ()
  "A deadline on a done item is not listed, whatever date it carries.
In the tree, beta's Pay the bill is DONE with a deadline on day 10."
  (pos-startup-test-with-tree
    (should-not (string-match-p "Pay the bill" (pos-startup-view root "deadlines")))))

(ert-deftest pos-startup/reviews-are-listed-late-or-due-and-apart-by-kind ()
  "The reviews view lists open reviews, projects' and responsibilities' apart.
A review is a heading tagged `pos-startup-review-tag'; it is listed
when late or scheduled in the window.  In the tree, alpha's review is
three days late, and home's and health's are in the window; beta's
Old review is DONE, so not listed."
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

(ert-deftest pos-startup/the-reviews-window-is-today-and-the-days-after-it ()
  "The reviews view covers `pos-startup-review-days' days from today.
Today is the first of the 7, so day 6 is the last day in the window
and day 7 is the first outside it.  In the tree, kitchen's review is
scheduled on day 6 and cellar's on day 7."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-view root "reviews")))
      (should (string-match-p "Review the kitchen" text))
      (should-not (string-match-p "Review the cellar" text)))))

(ert-deftest pos-startup/scopes-with-no-review-scheduled-are-named ()
  "Active projects and responsibilities with no open review are named.
A project is active when its file's STATUS is one of
`pos-startup-active-statuses'; every responsibility counts.  A review
scheduled on any date, in the window or beyond it, is a review.  In
the tree, beta, checkup and roof are active without one; gamma is
complete and delta has no status.  Of the responsibilities, health/teeth
and garden have none; home, health and kitchen have one in the window,
and cellar one beyond it."
  (pos-startup-test-with-tree
    (let ((parts (split-string (pos-startup-view root "reviews-to-schedule") "\n\n" t)))
      (should (equal (mapcar #'car (pos-startup-test-lines (nth 0 parts)))
                     '("health/projects/checkup" "projects/beta"
                       "responsibilities/home/projects/roof")))
      (should (equal (split-string (nth 1 parts) "\n" t " +")
                     '("Responsibilities with a review to be scheduled"
                       "health/teeth" "responsibilities/garden"))))))

(ert-deftest pos-startup/the-intray-view-lists-what-is-captured-and-not-placed ()
  "The intray view lists each open item under Unsorted in an intray.org.
Each is labelled by its scope.  In the tree, the root's intray has two
open items and health's has one open and one done."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-view root "intray")))
      (should (string-prefix-p "Intray, to be placed\n" text))
      (should (equal (pos-startup-test-lines text)
                     '(("health/intray" . "TODO Book the dentist")
                       ("intray" . "NEXT Answer the letter")
                       ("intray" . "TODO Sort the shelf")))))))

(ert-deftest pos-startup/an-empty-view-says-none ()
  "A view with no item says so, as does each empty list within a view.
The tree here holds one empty intray and nothing else."
  (pos-test-with-files root '(("intray.org" . "* Unsorted\n"))
    (dolist (view '("next" "scheduled" "deadlines" "intray" "all"))
      (ert-info ((format "the %s view" view))
        (should (string-suffix-p "\n  (none)\n" (pos-startup-view root view)))))
    (should (= 2 (length (split-string (pos-startup-view root "reviews")
                                       "  (none)\n" t))))
    (should (= 2 (length (split-string (pos-startup-view root "reviews-to-schedule")
                                       "  (none)\n" t))))))

;;;; The report

(ert-deftest pos-startup/the-report-gives-the-prompts-then-the-default-views ()
  "The report is the prompts, the count of files read, then the views asked.
The default views are `pos-startup-default-views', which leave out
all; asked for by name, all is given instead.  The tree has 15 files
that are read."
  (pos-startup-test-with-tree
    (let ((text (pos-startup-report root)))
      (should (string-prefix-p pos-startup-prompts text))
      (should (string-match-p "^Files read: 15$" text))
      (dolist (title '("NEXT items" "Scheduled items, next 14 days" "Deadlines, all open"
                       "Project reviews, late" "Projects with a review to be scheduled"
                       "Intray, to be placed"))
        (ert-info ((format "the %s view" title))
          (should (string-match-p (concat "^" title) text))))
      (should-not (string-match-p "^All TODO items" text)))
    (let ((text (pos-startup-report root '("all"))))
      (should (string-match-p "^All TODO items" text))
      (should (string-match-p "Prune the hedge" text))
      (should-not (string-match-p "^NEXT items" text)))))

(ert-deftest pos-startup/an-unknown-view-is-refused-before-anything-is-read ()
  "A view not in `pos-startup-views' is a user error, before any file is read.
The root here does not exist, and is never looked at."
  (should-error (pos-startup-report "/nonexistent/" '("bogus")) :type 'user-error))

(provide 'pos-startup-test)
;;; pos-startup-test.el ends here
