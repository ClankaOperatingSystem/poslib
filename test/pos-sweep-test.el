;;; pos-sweep-test.el --- Tests for pos-sweep.el  -*- lexical-binding: t -*-

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

;; The specification of pos-sweep.el, as tests.  The sections follow
;; pos-sweep.el's own, and each test is named by the rule it pins.
;;
;; The words the rules use.  The "sweep" retires the DONE and CANCELLED
;; entries of the writable corpus files into an archive of the week
;; that ended.  A "week" is named as ISO 8601 names it, 2026-W36, and
;; closes at its "boundary", Sunday 23:00 local.  A scope's "adapter"
;; is how its done items are retired: weekly, into Org archive files
;; beneath the scope at a path; or sealed, beneath _sweep/ until the
;; week closes and is sealed into the scope's archive.  The "plan" is
;; the Org table a reviewer marks before the "apply" archives.
;;
;; Pure rules, such as week names, boundaries and destinations, are
;; tested on values alone; the rest through files in a temporary
;; root, with the configurations the corpus reads.  Run: make test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pos-sweep)
(require 'pos-tree)
(require 'pos-test-support)

;;;; Fixtures

(defconst pos-sweep-test-week "2026-W36"
  "The week `pos-test-sunday' closes.")

(defconst pos-sweep-test-git-head "ref: refs/heads/master\n"
  "The HEAD of a repository; a directory holding one in .git is a repository.")

(defconst pos-sweep-test-child-config "pos: 2\nprojects: projects/\n"
  "The configuration of a child repository: a node of the root's tree.")

(defconst pos-sweep-test-intray
  "#+TODO: TODO | DONE CANCELLED
* TODO open task
* DONE finished task
* CANCELLED dropped task
* DONE parent with open child
** TODO still open
* Container
** DONE nested finished
** TODO nested open
"
  "An intray with every shape the sweep must handle.")

(defun pos-sweep-test-lines (&rest lines)
  "Return LINES as the text of a file: each ended by a newline."
  (mapconcat (lambda (line) (concat line "\n")) lines ""))

(defun pos-sweep-test-text (root file)
  "Return the text of FILE, named relative to ROOT."
  (pos-test-file-string (expand-file-name file root)))

(defun pos-sweep-test-config (&rest entries)
  "Return a node's configuration with projects in projects/ and ENTRIES.
Each entry is the lines of one archives entry, as a string."
  (concat "pos: 2\nprojects: projects/\n"
          (when entries
            (concat "archives:\n" (apply #'concat entries)))))

(defun pos-sweep-test-entry (scope &rest settings)
  "Return the lines of an archives entry for SCOPE, kept committed.
SETTINGS are further \"key: value\" lines."
  (concat (format "  - scope: %s\n    kept: committed\n" scope)
          (mapconcat (lambda (setting) (concat "    " setting "\n")) settings "")))

(defun pos-sweep-test-adapter (root file)
  "Return the adapter of FILE, relative to ROOT, with paths relative too.
\(KIND SCOPE . DIR): the sweep's kind, the entry's scope or nil for
the default, and the archive scope's directory relative to ROOT,
\".\" for the root itself."
  (let* ((corpus (pos-corpus root))
         (adapter (pos-sweep-adapter
                   corpus (pos-corpus-owner corpus (expand-file-name file root)))))
    (cons (car adapter)
          (cons (alist-get 'scope (cadr adapter))
                (directory-file-name (file-relative-name (cddr adapter) root))))))

(defmacro pos-sweep-test-with-plan (plan root week &rest body)
  "Evaluate BODY with PLAN a file holding the plan to sweep ROOT for WEEK.
The file is written outside ROOT and deleted afterwards."
  (declare (indent 3) (debug (symbolp form form body)))
  `(let ((,plan (make-temp-file "pos-sweep-test-" nil ".org"
                                (pos-sweep-plan ,root ,week))))
     (unwind-protect
         (progn ,@body)
       (delete-file ,plan))))

(defun pos-sweep-test-edit-plan (plan from to)
  "Replace the first FROM in the file PLAN with TO, as a reviewer edits a row."
  (let ((text (pos-test-file-string plan)))
    (with-temp-file plan
      (insert (string-replace from to text)))))

(defun pos-sweep-test-archive-time (file heading)
  "Return the ARCHIVE_TIME of the entry HEADING in the archive FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (concat "^\\*+ " (regexp-quote heading) "$"))
    (org-entry-get (point) "ARCHIVE_TIME")))

;;;; Weeks

(ert-deftest pos-sweep/sunday-night-belongs-to-its-iso-week ()
  "A week is named by its ISO year and week number, as 2026-W36.
Sunday is the last day of an ISO week, so the sweep boundary, Sunday
night, names the week it closes."
  (should (equal "2026-W36" (pos-week-name (pos-test-time 2026 9 6 23 0)))))

(ert-deftest pos-sweep/the-iso-year-can-differ-from-the-calendar-year ()
  "The week's year is the ISO year, which need not be the calendar year.
2027-01-01 is a Friday in the last week of 2026, so it is 2026-W53,
while 2026-01-01 is 2026-W01."
  (should (equal "2026-W01" (pos-week-name (pos-test-time 2026 1 1 12 0))))
  (should (equal "2026-W53" (pos-week-name (pos-test-time 2027 1 1 12 0)))))

(ert-deftest pos-sweep/the-boundary-itself-is-the-boundary ()
  "A sweep run at the boundary, Sunday 23:00, finds that very moment.
The boundary is the latest Sunday 23:00 at or before now; at, so a run
the clock fires on time is not pushed back a week."
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 6 23 0)))))

(ert-deftest pos-sweep/the-boundary-a-minute-early-is-last-week ()
  "On Sunday before 23:00 the latest boundary is the previous Sunday's.
The week is not over until the hour strikes."
  (should (time-equal-p (pos-test-time 2026 8 30 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 6 22 59)))))

(ert-deftest pos-sweep/a-late-run-on-monday-still-finds-sunday ()
  "A run on Monday morning sweeps the week that ended on Sunday night.
The sweep need not run on time; whenever it runs it archives into the
week most recently closed."
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 7 8 15)))))

(ert-deftest pos-sweep/midweek-finds-the-previous-sunday ()
  "A run in the middle of the week finds the Sunday before it."
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 9 12 0)))))

(ert-deftest pos-sweep/the-boundary-may-fall-in-the-previous-month ()
  "The boundary before Tuesday 2026-09-01 is Sunday 2026-08-30.
Going back by days can cross a month; `encode-time' normalises a day
of zero or less into the month before."
  (should (time-equal-p (pos-test-time 2026 8 30 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 1 9 0)))))

(ert-deftest pos-sweep/a-week-s-time-is-the-boundary-that-closes-it ()
  "The time of a week is its Sunday at the sweep hour, the boundary.
2026-W36's is `pos-test-sunday'.  What a plan's week stamps the
entries with is what the boundary would have, so a sweep applied
late writes the same bytes."
  (should (time-equal-p pos-test-sunday (pos-sweep-week-time "2026-W36"))))

(ert-deftest pos-sweep/week-one-is-found-from-january-the-fourth ()
  "January 4th is always in week 1, whatever weekday it falls on.
In 2026 it is a Sunday, so 2026-W01 closes that very day; in 2021 it
is a Monday, so 2021-W01 closes on the 10th.  Week 53 of 2026 closes
in January 2027: the days run past the year's end and `encode-time'
carries them."
  (should (time-equal-p (pos-test-time 2026 1 4 23 0) (pos-sweep-week-time "2026-W01")))
  (should (time-equal-p (pos-test-time 2021 1 10 23 0) (pos-sweep-week-time "2021-W01")))
  (should (time-equal-p (pos-test-time 2027 1 3 23 0) (pos-sweep-week-time "2026-W53"))))

(ert-deftest pos-sweep/a-week-s-time-is-named-back-as-the-week ()
  "The week name of a week's time is the week again, for any week.
Across years with 52 and 53 weeks, and whichever weekday the year
begins on."
  (dolist (week '("2020-W01" "2020-W53" "2021-W01" "2024-W52" "2025-W01"
                  "2026-W01" "2026-W36" "2026-W53" "2027-W01"))
    (should (equal week (pos-week-name (pos-sweep-week-time week))))))

(ert-deftest pos-sweep/a-string-that-is-not-a-week-is-refused ()
  "A string not of the form YYYY-Wnn is a `user-error', not a time.
A plan's title or a command-line argument that is misspelt stops the
sweep rather than stamping entries with nonsense."
  (should-error (pos-sweep-week-time "2026-09-06") :type 'user-error)
  (should-error (pos-sweep-week-time "2026-W5") :type 'user-error)
  (should-error (pos-sweep-week-time "W36") :type 'user-error))

;;;; Archiving a file

(ert-deftest pos-sweep/done-entries-are-counted-once-with-their-subtrees ()
  "Counting a file's done entries counts each with its subtree, once.
A done parent with done children is one entry to archive; a done
entry with an open entry beneath it is named as skipped instead; and
the file is not changed by the count."
  (pos-test-with-files root
      `(("intray.org" . ,pos-sweep-test-intray)
        ("nest.org" . ,(pos-sweep-test-lines "* DONE parent"
                                             "** DONE child one"
                                             "** DONE child two")))
    (should (equal '(3 "parent with open child")
                   (pos-sweep-done-entries (expand-file-name "intray.org" root))))
    (should (equal '(1) (pos-sweep-done-entries (expand-file-name "nest.org" root))))
    (should (equal pos-sweep-test-intray (pos-sweep-test-text root "intray.org")))))

(ert-deftest pos-sweep/done-entries-move-to-the-archive-and-open-ones-stay ()
  "Archiving a file moves its DONE and CANCELLED entries, however nested.
An open entry stays, and so does a done entry with an open task below
it, which is reported as skipped rather than taking the open task
with it.  Each archived entry is stamped with the week's time, and a
nested one records the outline path it came from."
  (pos-test-with-files root `(("intray.org" . ,pos-sweep-test-intray))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-sweep-destination 'weekly nil root file pos-sweep-test-week))
           (result (pos-archive-done-in-file file archive "intray.org" pos-test-sunday))
           (archived (pos-test-file-string archive)))
      (should (equal 3 (plist-get result :archived)))
      (should (equal '("parent with open child") (plist-get result :skipped)))
      (should (equal (pos-sweep-test-lines "#+TODO: TODO | DONE CANCELLED"
                                           "* TODO open task"
                                           "* DONE parent with open child"
                                           "** TODO still open"
                                           "* Container"
                                           "** TODO nested open")
                     (pos-test-file-string file)))
      (should (string-search "* DONE finished task" archived))
      (should (string-search "* CANCELLED dropped task" archived))
      (should (string-search "* DONE nested finished" archived))
      (should (string-search ":ARCHIVE_OLPATH: Container" archived))
      (should (equal 3 (cl-count-if
                        (lambda (line)
                          (string-search "ARCHIVE_TIME: 2026-09-06 Sun 23:00"
                                         line))
                        (split-string archived "\n")))))))

(ert-deftest pos-sweep/nothing-done-means-no-archive-file ()
  "A file with nothing done leaves no archive file behind.
A header with no entries under it would be noise in the archive tree."
  (pos-test-with-files root '(("todo.org" . "* TODO only open\n"))
    (let* ((file (expand-file-name "todo.org" root))
           (archive (pos-sweep-destination 'weekly nil root file pos-sweep-test-week))
           (result (pos-archive-done-in-file file archive "todo.org" pos-test-sunday)))
      (should (equal 0 (plist-get result :archived)))
      (should-not (file-exists-p archive)))))

(ert-deftest pos-sweep/the-archive-names-its-source-as-it-is-told ()
  "An archive names its source by the relative path it is given.
Never absolutely: Org would record the absolute path and the wall
clock; both are replaced, so that any checkout on any machine writes
the same bytes.  The path is the file's within the scope the archive
belongs to."
  (pos-test-with-files root '(("life/life-areas.org" . "* DONE finished\n"))
    (let* ((file (expand-file-name "life/life-areas.org" root))
           (archive (pos-sweep-destination 'weekly nil root file pos-sweep-test-week)))
      (pos-archive-done-in-file file archive "life/life-areas.org" pos-test-sunday)
      (let ((archived (pos-test-file-string archive)))
        (should (string-search ":ARCHIVE_FILE: life/life-areas.org" archived))
        (should (string-search "Archived entries from file life/life-areas.org"
                               archived))
        (should-not (string-search root archived))))))

(ert-deftest pos-sweep/the-stamp-goes-on-the-archived-entry-not-its-last-child ()
  "ARCHIVE_FILE and ARCHIVE_TIME are set on the entry archived, not below it.
Org leaves point at the end of the pasted subtree, in its last child;
the stamp climbs back to the entry itself, so a parent with done
children is stamped once, on the parent."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-sweep-test-lines "* DONE parent"
                                               "** DONE child one"
                                               "** DONE child two")))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-sweep-destination 'weekly nil root file pos-sweep-test-week)))
      (pos-archive-done-in-file file archive "intray.org" pos-test-sunday)
      (with-temp-buffer
        (insert-file-contents archive)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\* DONE parent")
        (should (equal "2026-09-06 Sun 23:00"
                       (org-entry-get (point) "ARCHIVE_TIME")))
        (should (equal "intray.org" (org-entry-get (point) "ARCHIVE_FILE")))
        (re-search-forward "^\\*\\* DONE child two")
        (should-not (org-entry-get (point) "ARCHIVE_TIME"))
        (should-not (org-entry-get (point) "ARCHIVE_FILE"))))))

(ert-deftest pos-sweep/archiving-writes-no-backup-files ()
  "Archiving saves the source and the archive without backup files.
The repository is the history; a stray intray.org~ beside the file is
clutter for Git to ignore or a person to delete."
  (pos-test-with-files root `(("intray.org" . ,pos-sweep-test-intray))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-sweep-destination 'weekly nil root file pos-sweep-test-week))
           (backup-enable-predicate (lambda (_name) t)))
      (pos-archive-done-in-file file archive "intray.org" pos-test-sunday)
      (should-not (directory-files-recursively root "~\\'")))))

;;;; Adapters

(ert-deftest pos-sweep/with-no-entry-anywhere-the-root-sweeps-weekly ()
  "A tree with no archives entry is swept weekly beneath its root.
The adapter is (weekly nil . ROOT): no entry, and the root as the
archive scope, as before there was a choice."
  (pos-test-with-files root '(("intray.org" . "") ("life/notes.org" . ""))
    (let ((corpus (pos-corpus root)))
      (should (equal (cons 'weekly (cons nil root))
                     (pos-sweep-adapter
                      corpus (pos-corpus-owner
                              corpus (expand-file-name "life/notes.org" root))))))))

(ert-deftest pos-sweep/the-nearest-entry-with-a-sweep-above-a-file-retires-it ()
  "A file is retired by the nearest entry with sweep at or above its scope.
A root entry with sweep weekly and a path applies to a nested plain
file.  A responsibility whose own entry says sealed wins over the
root's for its files; and a project an entry in that responsibility's
configuration names wins over the responsibility's own entry for the
project's files.  A scope whose entry has no sweep is passed over for
the nearest above it that has one."
  (pos-test-with-files root
      `((".clanka/config.yml"
         . ,(concat (pos-sweep-test-config
                     (pos-sweep-test-entry "." "sweep: weekly" "path: history/"))
                    "children:\n  - path: health\n  - path: work\n"))
        ("intray.org" . "") ("life/notes.org" . "")
        ("health/.clanka/config.yml"
         . ,(pos-sweep-test-config
             (pos-sweep-test-entry "." "sweep: sealed")
             (pos-sweep-test-entry "projects/fix" "sweep: weekly")))
        ("health/intray.org" . "")
        ("health/projects/fix/plan.org" . "")
        ("health/projects/rest.org" . "")
        ("work/.clanka/config.yml" . ,(pos-sweep-test-config (pos-sweep-test-entry ".")))
        ("work/intray.org" . ""))
    (should (equal '(weekly "." . ".") (pos-sweep-test-adapter root "intray.org")))
    (should (equal '(weekly "." . ".") (pos-sweep-test-adapter root "life/notes.org")))
    (should (equal '(sealed "." . "health")
                   (pos-sweep-test-adapter root "health/intray.org")))
    (should (equal '(sealed "." . "health")
                   (pos-sweep-test-adapter root "health/projects/rest.org")))
    (should (equal '(weekly "projects/fix" . "health/projects/fix")
                   (pos-sweep-test-adapter root "health/projects/fix/plan.org")))
    (should (equal '(weekly "." . ".") (pos-sweep-test-adapter root "work/intray.org")))))

(ert-deftest pos-sweep/a-weekly-file-archives-under-the-week-by-its-path-from-the-scope ()
  "A weekly file archives under the week by its path relative to the scope.
With _archive appended, beneath the default path archive/orgmode:
intray.org and life/life-areas.org archive apart, and a week's
directory mirrors the tree it was swept from.  The scope's directory
may be given with or without its slash."
  (should (equal "/repo/archive/orgmode/2026-W36/intray.org_archive"
                 (pos-sweep-destination 'weekly nil "/repo/" "/repo/intray.org"
                                        "2026-W36")))
  (should (equal "/repo/archive/orgmode/2026-W36/life/life-areas.org_archive"
                 (pos-sweep-destination 'weekly nil "/repo/" "/repo/life/life-areas.org"
                                        "2026-W36")))
  (should (equal "/repo/archive/orgmode/2026-W36/intray.org_archive"
                 (pos-sweep-destination 'weekly nil "/repo" "/repo/intray.org"
                                        "2026-W36"))))

(ert-deftest pos-sweep/a-weekly-entry-s-path-is-where-its-files-archive ()
  "A weekly entry's path replaces archive/orgmode, its trailing slash gone.
The reader strips the slash a path is written with, so the week
directories lie directly beneath the path named."
  (let ((entry (aref (alist-get 'archives
                                (pos-tree-read-config
                                 (pos-sweep-test-config
                                  (pos-sweep-test-entry "." "sweep: weekly"
                                                        "path: history/"))))
                     0)))
    (should (equal "history" (alist-get 'path entry)))
    (should (equal "/repo/history/2026-W36/life/life-areas.org_archive"
                   (pos-sweep-destination 'weekly entry "/repo/" "/repo/life/life-areas.org"
                                          "2026-W36")))))

(ert-deftest pos-sweep/a-sealed-file-archives-under-sweep-until-the-week-closes ()
  "A sealed file archives beneath the scope's _sweep/, under the week.
The path within the week is the same as a weekly file's; the week's
directory waits there to be sealed when the week closes."
  (should (equal "/repo/health/_sweep/2026-W36/projects/rest.org_archive"
                 (pos-sweep-destination 'sealed nil "/repo/health/"
                                        "/repo/health/projects/rest.org" "2026-W36"))))

(ert-deftest pos-sweep/a-file-outside-the-scope-escapes-the-week-directory ()
  "A file outside the scope is given an archive path outside its week.
This characterises a latent defect.  Such a file's name relative to
the scope begins with \"..\", which `expand-file-name' resolves, so
the archive lands at the path beside the weeks, not within one.
Nothing passes such a file today: the rows are made from the corpus,
whose files lie beneath the scopes that own them."
  (should (equal "/repo/archive/orgmode/elsewhere/notes.org_archive"
                 (pos-sweep-destination 'weekly nil "/repo/" "/elsewhere/notes.org"
                                        "2026-W36"))))

;;;; The plan

(ert-deftest pos-sweep/the-plan-has-a-row-for-each-file-with-done-entries ()
  "The plan is titled by its week, with one row per file with done entries.
A file whose done entries can all go is marked sweep, with their
count and its destination relative to the root; one whose done
entries all have open children is marked skip, with a count of zero;
a file with nothing done has no row.  The skipped headings are listed
below the table by file.  A file of a configured child repository, of
a product, or in archives/ is never in the plan.  The reader gives
the week and the rows back."
  (pos-test-with-files root
      `(("intray.org" . ,pos-sweep-test-intray)
        ("life/notes.org" . "* DONE parent with open child\n** TODO open\n")
        ("todo.org" . "* TODO only open\n")
        ("archives/old.org" . "* DONE kept\n")
        ("vendor/lib/.git/HEAD" . ,pos-sweep-test-git-head)
        ("vendor/lib/README.org" . "* DONE released\n")
        ("child/.git/HEAD" . ,pos-sweep-test-git-head)
        ("child/.clanka/config.yml" . ,pos-sweep-test-child-config)
        ("child/intray.org" . "* DONE theirs\n"))
    (let ((text (pos-sweep-plan root pos-sweep-test-week)))
      (should (string-prefix-p "#+TITLE: Sweep 2026-W36\n" text))
      (should (string-search "\nDone, but with open children; these stay:\n" text))
      (should (string-search "\n- intray.org: parent with open child\n" text))
      (should (string-search "\n- life/notes.org: parent with open child\n" text))
      (should-not (string-search "todo.org" text))
      (should-not (string-search "child/intray.org" text))
      (should-not (string-search "README.org" text))
      (should-not (string-search "old.org" text))
      (pos-sweep-test-with-plan plan root pos-sweep-test-week
        (should (equal text (pos-test-file-string plan)))
        (should (equal `("2026-W36"
                         ("sweep" "intray.org" 3
                          "archive/orgmode/2026-W36/intray.org_archive"
                          ,(pos-sweep-done-digest (expand-file-name "intray.org" root)))
                         ("skip" "life/notes.org" 0
                          "archive/orgmode/2026-W36/life/notes.org_archive" ""))
                       (pos-sweep-read-plan plan)))))))

(ert-deftest pos-sweep/a-plan-s-destinations-follow-each-file-s-adapter ()
  "Each row's destination is the file's adapter's, relative to the root.
A root file under a weekly root entry goes to the entry's path; a
responsibility's file under its own sealed entry goes beneath its
_sweep/."
  (pos-test-with-files root
      `((".clanka/config.yml"
         . ,(concat (pos-sweep-test-config
                     (pos-sweep-test-entry "." "sweep: weekly" "path: history/"))
                    "children:\n  - path: health\n"))
        ("intray.org" . "* DONE finished\n")
        ("health/.clanka/config.yml"
         . ,(pos-sweep-test-config (pos-sweep-test-entry "." "sweep: sealed")))
        ("health/projects/rest.org" . "* DONE slept\n"))
    (pos-sweep-test-with-plan plan root pos-sweep-test-week
      (should (equal '(("sweep" "health/projects/rest.org" 1
                        "health/_sweep/2026-W36/projects/rest.org_archive")
                       ("sweep" "intray.org" 1 "history/2026-W36/intray.org_archive"))
                     (mapcar (lambda (row) (seq-take row 4))
                             (cdr (pos-sweep-read-plan plan))))))))

(ert-deftest pos-sweep/a-file-that-is-not-a-plan-is-refused ()
  "A file without a sweep title is not a plan, and is a `user-error'."
  (pos-test-with-files root '(("notes.org" . "* TODO not a plan\n"))
    (should-error (pos-sweep-read-plan (expand-file-name "notes.org" root))
                  :type 'user-error)))

;;;; The apply

(ert-deftest pos-sweep/applying-a-plan-archives-the-rows-marked-sweep ()
  "Each row marked sweep is archived to its destination, stamped with the week.
The stamp is the week's boundary, not the clock, so a late apply
writes what a timely one would; the skipped headings are reported;
and nothing is left or stale."
  (pos-test-with-files root
      `(("intray.org" . ,pos-sweep-test-intray)
        ("life/notes.org" . "* DONE shipped\n* TODO next\n"))
    (pos-sweep-test-with-plan plan root pos-sweep-test-week
      (should (equal '(:archived 4 :skipped ("parent with open child") :left 0 :stale 0)
                     (pos-sweep-apply root plan)))
      (should (equal "2026-09-06 Sun 23:00"
                     (pos-sweep-test-archive-time
                      (expand-file-name "archive/orgmode/2026-W36/intray.org_archive" root)
                      "DONE finished task")))
      (should (equal "2026-09-06 Sun 23:00"
                     (pos-sweep-test-archive-time
                      (expand-file-name
                       "archive/orgmode/2026-W36/life/notes.org_archive" root)
                      "DONE shipped")))
      (should (equal "* TODO next\n" (pos-sweep-test-text root "life/notes.org"))))))

(ert-deftest pos-sweep/a-row-edited-to-skip-is-left ()
  "A row a reviewer marks skip, or anything but sweep, is left as it is.
The file keeps its done entries and no archive is written for it;
the row is counted as left."
  (pos-test-with-files root
      `(("intray.org" . "* DONE finished\n")
        ("life/notes.org" . "* DONE shipped\n"))
    (pos-sweep-test-with-plan plan root pos-sweep-test-week
      (pos-sweep-test-edit-plan plan "| sweep | intray.org" "| skip | intray.org")
      (should (equal '(:archived 1 :skipped nil :left 1 :stale 0)
                     (pos-sweep-apply root plan)))
      (should (equal "* DONE finished\n" (pos-sweep-test-text root "intray.org")))
      (should-not (file-exists-p
                   (expand-file-name "archive/orgmode/2026-W36/intray.org_archive" root)))
      (should (file-exists-p
               (expand-file-name
                "archive/orgmode/2026-W36/life/notes.org_archive" root))))))

(ert-deftest pos-sweep/a-file-whose-done-entries-changed-since-the-plan-is-stale ()
  "A row whose file's done entries changed in number since the plan is stale.
Whether one was finished or one was deleted since the plan was
written, the file is left untouched and the row is counted as stale,
so a reviewer never approves what was not shown."
  (pos-test-with-files root
      `(("gained.org" . "* DONE first\n")
        ("lost.org" . "* DONE first\n* DONE second\n"))
    (pos-sweep-test-with-plan plan root pos-sweep-test-week
      (with-current-buffer (pos-visit (expand-file-name "gained.org" root))
        (goto-char (point-max))
        (insert "* DONE second\n")
        (save-buffer))
      (with-current-buffer (pos-visit (expand-file-name "lost.org" root))
        (erase-buffer)
        (insert "* DONE first\n")
        (save-buffer))
      (should (equal '(:archived 0 :skipped nil :left 0 :stale 2)
                     (pos-sweep-apply root plan)))
      (should (equal "* DONE first\n* DONE second\n"
                     (pos-sweep-test-text root "gained.org")))
      (should (equal "* DONE first\n" (pos-sweep-test-text root "lost.org")))
      (should-not (file-exists-p (expand-file-name "archive" root))))))

(ert-deftest pos-sweep/a-file-with-other-done-entries-than-the-plan-s-is-stale ()
  "A row is stale when its file's done entries are not the ones planned.
The count alone does not tell: with one entry finished and another
deleted since the plan, or one entry's text edited, the file has as
many done entries as the plan counted and is still left untouched.
An edit elsewhere in the file leaves the row fresh.  A row with no
entries column, as an older plan has, is held to its count alone."
  (pos-test-with-files root
      `(("swapped.org" . "* DONE first\n* TODO second\n")
        ("edited.org" . "* DONE first\n")
        ("elsewhere.org" . "* DONE first\n* TODO second\n"))
    (pos-sweep-test-with-plan plan root pos-sweep-test-week
      (dolist (change '(("swapped.org" . "* TODO first\n* DONE second\n")
                        ("edited.org" . "* DONE first\nand a note\n")
                        ("elsewhere.org" . "* DONE first\n* TODO third\n")))
        (with-current-buffer (pos-visit (expand-file-name (car change) root))
          (erase-buffer)
          (insert (cdr change))
          (save-buffer)))
      (should (equal '(:archived 1 :skipped nil :left 0 :stale 2)
                     (pos-sweep-apply root plan t)))
      ;; The same plan without its last column.
      (with-temp-file plan
        (insert "#+TITLE: Sweep 2026-W36\n\n"
                "| act | file | done | destination |\n|-\n"
                "| sweep | swapped.org | 1 | archive/orgmode/2026-W36/swapped.org_archive |\n"))
      (should (equal '(:archived 1 :skipped nil :left 0 :stale 0)
                     (pos-sweep-apply root plan t))))))

(ert-deftest pos-sweep/a-dry-run-counts-and-writes-nothing ()
  "A dry run gives the counts an apply would, and changes no file.
The files keep their entries and no archive directory is made."
  (pos-test-with-files root `(("intray.org" . ,pos-sweep-test-intray))
    (pos-sweep-test-with-plan plan root pos-sweep-test-week
      (should (equal '(:archived 3 :skipped ("parent with open child") :left 0 :stale 0)
                     (pos-sweep-apply root plan t)))
      (should (equal pos-sweep-test-intray (pos-sweep-test-text root "intray.org")))
      (should-not (file-exists-p (expand-file-name "archive" root))))))

;;;; The command line

(ert-deftest pos-sweep/dry-run-is-read-before-or-after-the-plan-s-name ()
  "What follows apply is read as a dry run wherever --dry-run stands.
Before the plan's name, after it, or alone; with neither, an apply of
the default plan.  Another option, or a second name, is no reading at
all, so that the command refuses it instead of applying for real."
  (should (equal '(nil) (pos-sweep--apply-arguments nil)))
  (should (equal '(nil . "sweep.org") (pos-sweep--apply-arguments '("sweep.org"))))
  (should (equal '(t) (pos-sweep--apply-arguments '("--dry-run"))))
  (should (equal '(t . "sweep.org")
                 (pos-sweep--apply-arguments '("--dry-run" "sweep.org"))))
  (should (equal '(t . "sweep.org")
                 (pos-sweep--apply-arguments '("sweep.org" "--dry-run"))))
  (should-not (pos-sweep--apply-arguments '("sweep.org" "--dryrun")))
  (should-not (pos-sweep--apply-arguments '("sweep.org" "other.org"))))

;;;; The report

(ert-deftest pos-sweep/the-report-names-each-skipped-entry ()
  "The report gives the counts on one line, then each skipped heading by name.
Entries archived, headings skipped, rows left and rows stale; a done
entry with open children is the one thing the sweep leaves for a
person to resolve, so the report says which."
  (let ((result '(:archived 194 :skipped ("call the plumber" "renew licence")
                  :left 1 :stale 2)))
    (should (equal (pos-sweep-test-lines
                    "Sweep 2026-W36: archived 194, skipped 2, 1 files left, 2 stale"
                    "  skipped (done, but has open children):"
                    "    call the plumber"
                    "    renew licence")
                   (concat (pos-sweep-report "2026-W36" result) "\n")))))

(ert-deftest pos-sweep/the-report-says-nothing-of-skipping-when-there-is-none ()
  "With nothing skipped, the report is its one line of counts.
A dry run's report says what would be swept."
  (let ((result '(:archived 3 :skipped nil :left 0 :stale 0)))
    (should (equal "Sweep 2026-W36: archived 3, skipped 0, 0 files left, 0 stale"
                   (pos-sweep-report "2026-W36" result)))
    (should (equal "Would sweep 2026-W36: archived 3, skipped 0, 0 files left, 0 stale"
                   (pos-sweep-report "2026-W36" result t)))))

;;;; The sweep

(ert-deftest pos-sweep/a-sweep-archives-every-writable-file-and-no-other ()
  "A sweep archives the done entries of every writable file, and only those.
Every Org file of the corpus the root may write, at any depth: a book
in a resources directory is swept with the top-level files.  Each
file's archive is written under the week by the file's own path.  A
done entry in a file the corpus does not read, in archives/ or in a
product repository, is left where it is, for lint to report as
stranded.  The counts and skipped headings are totalled."
  (pos-test-with-files root
      `(("intray.org" . ,pos-sweep-test-intray)
        ("life/life-projects.org" . "* DONE shipped\n* TODO next\n")
        ("life/resources/book.org" . "* DONE read\n")
        ("archives/old.org" . "* DONE kept\n")
        ("vendor/lib/.git/HEAD" . ,pos-sweep-test-git-head)
        ("vendor/lib/README.org" . "* DONE released\n"))
    (let* ((pos-directory root)
           (result (pos-sweep pos-sweep-test-week)))
      (should (equal '(:archived 5 :skipped ("parent with open child") :left 0 :stale 0)
                     result))
      (dolist (file '("intray.org" "life/life-projects.org"
                      "life/resources/book.org"))
        (should (file-exists-p
                 (expand-file-name
                  (concat "archive/orgmode/2026-W36/" file "_archive") root))))
      (should (equal "* DONE kept\n" (pos-sweep-test-text root "archives/old.org")))
      (should (equal "* DONE released\n"
                     (pos-sweep-test-text root "vendor/lib/README.org"))))))

(ert-deftest pos-sweep/a-sweep-without-arguments-finds-its-own-week ()
  "Called with no arguments, a sweep uses the latest boundary at or before now.
That is what a scheduled run does: the week directory is named from
the boundary, not from the moment the run began, and the entries are
stamped with the boundary too."
  (pos-test-with-files root '(("intray.org" . "* DONE finished\n"))
    (let* ((pos-directory root)
           (boundary (pos-sweep-boundary (current-time)))
           (archive (concat "archive/orgmode/" (pos-week-name boundary)
                            "/intray.org_archive")))
      (pos-sweep)
      (should (string-search
               (concat "ARCHIVE_TIME: "
                       (format-time-string "%F %a %H:%M" boundary))
               (pos-sweep-test-text root archive))))))

;;;; Closing weeks

(ert-deftest pos-sweep/a-swept-week-before-this-one-is-sealed-into-the-archive ()
  "Closing seals each swept week before the given one into the scope's archive.
With a root entry kept committed and swept sealed, a sweep for
2026-W35 writes beneath _sweep/2026-W35; closing for 2026-W36 gives
one seal plan, to archives/sweep/2026-W35, while closing for 2026-W35
itself gives none.  Applying it moves the week into the archive: the
staged directory is gone, the archive file is read-only, and the
ledger check of the root has no finding."
  (pos-test-with-files root
      `((".clanka/config.yml"
         . ,(pos-sweep-test-config (pos-sweep-test-entry "." "sweep: sealed")))
        ("intray.org" . "* DONE finished\n"))
    (pos-test-git-init root)
    (make-directory (expand-file-name "archives" root))
    (pos-sweep-test-with-plan plan root "2026-W35"
      (should (equal '(:archived 1 :skipped nil :left 0 :stale 0)
                     (pos-sweep-apply root plan))))
    (should (file-exists-p (expand-file-name "_sweep/2026-W35/intray.org_archive" root)))
    (should-not (pos-sweep-close root "2026-W35"))
    (let ((plans (pos-sweep-close root "2026-W36")))
      (should (equal 1 (length plans)))
      (should (equal "archives/sweep/2026-W35"
                     (file-relative-name (alist-get 'destination (car plans))
                                         (file-truename root))))
      (should (equal 1 (length (pos-sweep-close-apply plans)))))
    (let ((sealed (expand-file-name "archives/sweep/2026-W35/intray.org_archive" root)))
      (should-not (file-exists-p (expand-file-name "_sweep/2026-W35" root)))
      (should (file-exists-p sealed))
      (should (zerop (logand (file-modes sealed) #o222)))
      (should (string-search "* DONE finished" (pos-test-file-string sealed)))
      (should-not (pos-seal-findings-p (pos-ledger-check root))))))

(ert-deftest pos-sweep/a-weekly-scope-has-nothing-to-close ()
  "A scope swept weekly has no weeks to close.
Its archive files stay as they are, and no seal plan is made."
  (pos-test-with-files root
      `((".clanka/config.yml"
         . ,(pos-sweep-test-config (pos-sweep-test-entry "." "sweep: weekly")))
        ("intray.org" . "* DONE finished\n"))
    (pos-sweep-test-with-plan plan root "2026-W35"
      (pos-sweep-apply root plan))
    (should (file-exists-p
             (expand-file-name "archive/orgmode/2026-W35/intray.org_archive" root)))
    (should-not (pos-sweep-close root "2026-W36"))))

(ert-deftest pos-sweep/a-week-swept-in-a-child-repository-is-not-closed ()
  "Closing leaves the swept weeks of a child repository to that repository.
A child repository whose own configuration seals its sweep has a
staged week beneath its _sweep/.  Closing from the root gives no
plan for it, so nothing is renamed and no ledger event is written
inside the child; closing from the child gives the one plan."
  (pos-test-with-files root
      `((".clanka/config.yml" . ,(pos-sweep-test-config))
        ("child/.git/HEAD" . ,pos-sweep-test-git-head)
        ("child/.clanka/config.yml"
         . ,(pos-sweep-test-config (pos-sweep-test-entry "." "sweep: sealed")))
        ("child/_sweep/2026-W35/intray.org_archive" . "* DONE finished\n"))
    (should-not (pos-sweep-close root "2026-W36"))
    (should (equal 1 (length (pos-sweep-close (expand-file-name "child" root)
                                              "2026-W36"))))))

(provide 'pos-sweep-test)
;;; pos-sweep-test.el ends here
