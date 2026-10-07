;;; pos-test.el --- Tests for pos.el  -*- lexical-binding: t -*-

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

;; The specification of pos.el, as tests.  The sections follow pos.el's
;; own, and each test is named by the rule it pins, as a sentence.
;;
;; The words the rules use.  The "root" is the repository the tools
;; work on.  A "pillar" is a root subdirectory named in `pos-pillars',
;; one per area of life.  The "covered" files are the top-level Org
;; files of the root and of each pillar: the files the sweep visits.
;; The "uncovered" files are the rest, below those directories.  The
;; "intray" is the root's intray.org, where new and rescued tasks wait
;; to be filed.  The "sweep" archives the DONE and CANCELLED entries of
;; the covered files into the week's archive.  A "stranded" task is a
;; task keyword in an uncovered file, which no sweep will ever reach.
;;
;; Pure rules, such as week names, archive paths, dedupe suggestions
;; and excerpts, are tested on values alone; the rest through files in
;; a temporary root.  Run: make test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pos)
(require 'pos-test-support)

;;;; Fixtures

(defmacro pos-test-configured (&rest body)
  "Evaluate BODY with the configuration of test/pos-config.el in force.
Pillars life, sport, people, work, body and meta; prose directories
under meta; a refile rule each for work and body."
  (declare (indent 0))
  `(let ((pos-pillars '("life" "sport" "people" "work" "body" "meta"))
         (pos-prose-directories '("meta/journal" "meta/specs"))
         (pos-refile-rules '(("invoice\\|client" . "work")
                             ("dentist\\|checkup" . "body"))))
     ,@body))

(defconst pos-test-week "2026-W36"
  "The week `pos-test-sunday' closes.")

(defun pos-test-lines (&rest lines)
  "Return LINES as the text of a file: each ended by a newline."
  (mapconcat (lambda (line) (concat line "\n")) lines ""))

(defun pos-test-text (root file)
  "Return the text of FILE, named relative to ROOT."
  (pos-test-file-string (expand-file-name file root)))

(defconst pos-test-intray
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

;;;; Weeks

(ert-deftest pos/sunday-night-belongs-to-its-iso-week ()
  "A week is named by its ISO year and week number, as 2026-W36.
Sunday is the last day of an ISO week, so the sweep boundary, Sunday
night, names the week it closes."
  (should (equal "2026-W36" (pos-week-name (pos-test-time 2026 9 6 23 0)))))

(ert-deftest pos/the-iso-year-can-differ-from-the-calendar-year ()
  "The week's year is the ISO year, which need not be the calendar year.
2027-01-01 is a Friday in the last week of 2026, so it is 2026-W53,
while 2026-01-01 is 2026-W01."
  (should (equal "2026-W01" (pos-week-name (pos-test-time 2026 1 1 12 0))))
  (should (equal "2026-W53" (pos-week-name (pos-test-time 2027 1 1 12 0)))))

(ert-deftest pos/the-boundary-itself-is-the-boundary ()
  "A sweep run at the boundary, Sunday 23:00, finds that very moment.
The boundary is the latest Sunday 23:00 at or before now; at, so a run
the clock fires on time is not pushed back a week."
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 6 23 0)))))

(ert-deftest pos/the-boundary-a-minute-early-is-last-week ()
  "On Sunday before 23:00 the latest boundary is the previous Sunday's.
The week is not over until the hour strikes."
  (should (time-equal-p (pos-test-time 2026 8 30 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 6 22 59)))))

(ert-deftest pos/a-late-run-on-monday-still-finds-sunday ()
  "A run on Monday morning sweeps the week that ended on Sunday night.
The sweep need not run on time; whenever it runs it archives into the
week most recently closed."
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 7 8 15)))))

(ert-deftest pos/midweek-finds-the-previous-sunday ()
  "A run in the middle of the week finds the Sunday before it."
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 9 12 0)))))

(ert-deftest pos/the-boundary-may-fall-in-the-previous-month ()
  "The boundary before Tuesday 2026-09-01 is Sunday 2026-08-30.
Going back by days can cross a month; `encode-time' normalises a day
of zero or less into the month before."
  (should (time-equal-p (pos-test-time 2026 8 30 23 0)
                        (pos-sweep-boundary (pos-test-time 2026 9 1 9 0)))))

;;;; Files and paths

(ert-deftest pos/covered-files-are-the-top-of-the-root-and-each-pillar ()
  "The covered files are the root's top-level Org files and each pillar's.
Nothing deeper counts: a resources directory below a pillar is not
covered, nor is anything that is not an Org file."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "") ("todo.org" . "") ("notes.txt" . "")
          ("life/life-areas.org" . "") ("life/life-projects.org" . "")
          ("life/resources/book.org" . "")
          ("sport/sport-areas.org" . "")
          ("sport/resources/zettles/a.org" . "")
          ("archive/orgmode/2026-W35/intray.org_archive" . ""))
      (should (equal '("intray.org"
                       "life/life-areas.org" "life/life-projects.org"
                       "sport/sport-areas.org" "todo.org")
                     (pos-test-relative (pos-org-files root) root))))))

(ert-deftest pos/covered-files-are-listed-root-first-then-pillar-by-pillar ()
  "Covered files are listed root first, then one pillar after another.
The pillars come in `pos-pillars' order and each directory's files are
sorted by name within it, but the list as a whole is not sorted, which
is why the other tests compare through `pos-test-relative', which
sorts.  Pinned so that a corpus listing its files in one order is a
deliberate change."
  (pos-test-with-files root
      '(("todo.org" . "") ("alpha.org" . "")
        ("life/life-areas.org" . "") ("sport/sport-areas.org" . ""))
    (let ((pos-pillars '("sport" "life")))
      (should (equal '("alpha.org" "todo.org"
                       "sport/sport-areas.org" "life/life-areas.org")
                     (mapcar (lambda (file) (file-relative-name file root))
                             (pos-org-files root))))
      (should (equal '("alpha.org" "life/life-areas.org"
                       "sport/sport-areas.org" "todo.org")
                     (pos-test-relative (pos-org-files root) root))))))

(ert-deftest pos/a-pillar-named-with-a-trailing-slash-is-still-a-pillar ()
  "A pillar may be named \"sport/\" as well as \"sport\" in `pos-pillars'.
`expand-file-name' takes either, so a configuration written with
directory names is not silently a configuration of no pillars."
  (pos-test-with-files root
      '(("intray.org" . "") ("sport/sport-areas.org" . ""))
    (let ((pos-pillars '("sport/")))
      (should (equal '("intray.org" "sport/sport-areas.org")
                     (pos-test-relative (pos-org-files root) root))))))

(ert-deftest pos/a-missing-pillar-is-skipped-not-an-error ()
  "A pillar named in `pos-pillars' with no directory contributes nothing.
A fresh repository may not have every area yet; the sweep and lint
still run."
  (pos-test-with-files root '(("intray.org" . ""))
    (let ((pos-pillars '("life" "nowhere")))
      (should (equal '("intray.org")
                     (pos-test-relative (pos-org-files root) root))))))

(ert-deftest pos/a-lock-file-in-a-covered-directory-is-covered-today ()
  "The lock file of an Org file open in Emacs is listed as covered today.
Editing intray.org leaves a dangling symlink .#intray.org beside it,
whose name ends in .org like any other; the walker looks no further
than the name.  This characterises a latent defect: a sweep run while
a covered file is open would try to visit its lock.  A corpus that
leaves lock files out changes this on purpose."
  (pos-test-with-files root '(("intray.org" . ""))
    (make-symbolic-link "someone@somewhere.1234"
                        (expand-file-name ".#intray.org" root))
    (should (equal '(".#intray.org" "intray.org")
                   (pos-test-relative (pos-org-files root) root)))))

(ert-deftest pos/a-hidden-org-file-in-a-covered-directory-is-covered-today ()
  "A hidden Org file at the top of the root or a pillar is covered today.
Only the uncovered walker leaves hidden things out, and only hidden
directories; the covered walker takes every name ending in .org.
Pinned so that a corpus treating hidden files alike everywhere
changes this on purpose."
  (pos-test-with-files root
      '(("intray.org" . "") (".draft.org" . "") ("life/.draft.org" . ""))
    (let ((pos-pillars '("life")))
      (should (equal '(".draft.org" "intray.org" "life/.draft.org")
                     (pos-test-relative (pos-org-files root) root))))))

(ert-deftest pos/an-archive-file-is-never-covered ()
  "A file ending in .org_archive is not covered, at the root or in a pillar.
Archives are what the sweep writes; sweeping them again would archive
the archive.  Only names ending in .org count."
  (pos-test-with-files root
      '(("intray.org" . "") ("intray.org_archive" . "")
        ("life/life-areas.org" . "") ("life/life-areas.org_archive" . ""))
    (let ((pos-pillars '("life")))
      (should (equal '("intray.org" "life/life-areas.org")
                     (pos-test-relative (pos-org-files root) root))))))

(ert-deftest pos/uncovered-files-lie-below-the-covered-directories ()
  "The uncovered files are the Org files under the root that are not covered.
They lie below the root's and the pillars' top levels: a book in a
pillar's resources, a zettel deeper still, a file in a directory that
is no pillar.  Archive files are not Org files here either."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "") ("life/life-areas.org" . "")
          ("life/resources/book.org" . "")
          ("life/resources/book.org_archive" . "")
          ("sport/resources/zettles/a.org" . "") ("notes/plain.org" . ""))
      (should (equal '("life/resources/book.org" "notes/plain.org"
                       "sport/resources/zettles/a.org")
                     (pos-test-relative (pos-uncovered-org-files root)
                                        root))))))

(ert-deftest pos/the-archive-and-prose-directories-are-never-uncovered ()
  "Files in `pos-archive-directory' and `pos-prose-directories' are left out.
An archive holds swept tasks, which keep their keywords; prose, such
as a journal, may use a keyword as a word.  Neither is a place a task
is stranded.  A prose directory's sibling is not excluded with it."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "")
          ("archive/orgmode/2026-W35/intray.org_archive" . "")
          ("archive/orgmode/2026-W35/notes.org" . "")
          ("meta/journal/review.org" . "") ("meta/specs/spec.org" . "")
          ("meta/tools/notes.org" . ""))
      (should (equal '("meta/tools/notes.org")
                     (pos-test-relative (pos-uncovered-org-files root)
                                        root))))))

(ert-deftest pos/hidden-directories-are-not-walked ()
  "The uncovered walker does not enter a hidden directory.
A virtual environment or .git may hold Org files that are nobody's
tasks.  A hidden file, or a lock file, in a directory it does enter is
still listed: only the directory's name is looked at.  That much is a
characterisation, pinned for the corpus to change on purpose."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "") ("life/resources/book.org" . "")
          ("life/resources/.draft.org" . "")
          (".venv/lib/site-packages/x.org" . "") (".git/x.org" . ""))
      (make-symbolic-link "someone@somewhere.1234"
                          (expand-file-name "life/resources/.#book.org" root))
      (should (equal '("life/resources/.#book.org"
                       "life/resources/.draft.org"
                       "life/resources/book.org")
                     (pos-test-relative (pos-uncovered-org-files root)
                                        root))))))

(ert-deftest pos/a-file-archives-under-the-week-by-its-path-from-the-root ()
  "A file archives under the week by its path relative to the root.
With _archive appended: intray.org and life/life-areas.org archive
apart, and a week's directory mirrors the tree it was swept from."
  (should (equal "/repo/archive/orgmode/2026-W36/intray.org_archive"
                 (pos-archive-file "/repo" "/repo/intray.org" "2026-W36")))
  (should (equal "/repo/archive/orgmode/2026-W36/life/life-areas.org_archive"
                 (pos-archive-file "/repo" "/repo/life/life-areas.org"
                                   "2026-W36"))))

(ert-deftest pos/the-root-may-be-named-with-a-trailing-slash ()
  "The root may be given as \"/repo/\" or \"/repo\" with the same result.
`pos-directory' is a directory name; the sweep passes it with its
slash, a caller at the keyboard may not."
  (should (equal "/repo/archive/orgmode/2026-W36/intray.org_archive"
                 (pos-archive-file "/repo/" "/repo/intray.org" "2026-W36"))))

(ert-deftest pos/the-archive-directory-is-the-configured-one ()
  "Archives go under `pos-archive-directory', whatever it is set to.
The default is archive/orgmode; a repository that keeps its archive
elsewhere binds the variable, and the week directories follow."
  (let ((pos-archive-directory "attic"))
    (should (equal "/repo/attic/2026-W36/life/life-areas.org_archive"
                   (pos-archive-file "/repo" "/repo/life/life-areas.org"
                                     "2026-W36")))))

(ert-deftest pos/a-file-outside-the-root-escapes-the-week-directory ()
  "A file outside the root is given an archive path outside its week.
This characterises a latent defect.  Such a file's name relative to
the root begins with \"..\", which `expand-file-name' resolves, so the
archive lands in `pos-archive-directory' beside the weeks, not within
one.  Nothing passes such a file today, since the sweep walks under
the root; when the sweep gets its plan, this is to become a refusal."
  (should (equal "/repo/archive/orgmode/elsewhere/notes.org_archive"
                 (pos-archive-file "/repo" "/elsewhere/notes.org" "2026-W36"))))

;;;; Keywords

(ert-deftest pos/the-done-keywords-are-those-after-the-bar ()
  "DONE and CANCELLED, the keywords after the bar, are the done ones.
One sequence serves every file; the sweep archives entries in these
two states and no other."
  (should (equal '("DONE" "CANCELLED") (pos-done-keywords))))

(ert-deftest pos/visiting-a-file-puts-the-one-sequence-in-force ()
  "A buffer `pos-visit' returns knows the one sequence, with no file line.
Org reads `org-todo-keywords' as a buffer enters Org mode, so the
sequence is bound for that moment only; the user's own setting is not
changed."
  (pos-test-with-files root '(("intray.org" . "* WIP busy\n"))
    (let ((before org-todo-keywords))
      (with-current-buffer (pos-visit (expand-file-name "intray.org" root))
        (should (equal '("BACKLOG" "TODO" "NEXT" "WIP" "BLOCKED"
                         "DONE" "CANCELLED")
                       org-todo-keywords-1))
        (should (equal '("DONE" "CANCELLED") org-done-keywords)))
      (should (eq before org-todo-keywords)))))

(ert-deftest pos/the-one-sequence-is-in-force-without-a-file-line ()
  "A file with no #+TODO line still archives by the one sequence.
CANCELLED is done, so it goes; WIP is open under the sequence, though
no line in the file says so, so it stays."
  (pos-test-with-files root
      '(("intray.org" . "* CANCELLED dropped\n* WIP busy\n* TODO open\n"))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-archive-file root file pos-test-week))
           (result (pos-archive-done-in-file root file pos-test-week
                                             pos-test-sunday)))
      (should (equal 1 (plist-get result :archived)))
      (should (string-search "* CANCELLED dropped"
                             (pos-test-file-string archive)))
      (should (string-search "* WIP busy" (pos-test-file-string file))))))

;;;; Archiving

(ert-deftest pos/done-entries-move-to-the-archive-and-open-ones-stay ()
  "Archiving a file moves its DONE and CANCELLED entries, however nested.
An open entry stays, and so does a done entry with an open task below
it, which is reported as skipped rather than taking the open task
with it.  Each archived entry is stamped with the sweep's time, and a
nested one records the outline path it came from."
  (pos-test-with-files root `(("intray.org" . ,pos-test-intray))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-archive-file root file pos-test-week))
           (result (pos-archive-done-in-file root file pos-test-week
                                             pos-test-sunday))
           (archived (pos-test-file-string archive)))
      (should (equal 3 (plist-get result :archived)))
      (should (equal '("parent with open child") (plist-get result :skipped)))
      (should (equal (pos-test-lines "#+TODO: TODO | DONE CANCELLED"
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

(ert-deftest pos/nothing-done-means-no-archive-file ()
  "A file with nothing done leaves no archive behind.
An empty week directory, or a header with no entries under it, would
be noise in the archive tree."
  (pos-test-with-files root '(("todo.org" . "* TODO only open\n"))
    (let* ((file (expand-file-name "todo.org" root))
           (result (pos-archive-done-in-file root file pos-test-week
                                             pos-test-sunday)))
      (should (equal 0 (plist-get result :archived)))
      (should-not (file-exists-p (pos-archive-file root file pos-test-week))))))

(ert-deftest pos/the-archive-names-its-source-relative-to-the-root ()
  "An archive names its source file relative to the root, never absolutely.
Org would record the absolute path and the wall clock; both are
replaced, so that any checkout on any machine writes the same bytes."
  (pos-test-with-files root '(("life/life-areas.org" . "* DONE finished\n"))
    (let ((file (expand-file-name "life/life-areas.org" root)))
      (pos-archive-done-in-file root file pos-test-week pos-test-sunday)
      (let ((archived (pos-test-file-string
                       (pos-archive-file root file pos-test-week))))
        (should (string-search ":ARCHIVE_FILE: life/life-areas.org" archived))
        (should (string-search "Archived entries from file life/life-areas.org"
                               archived))
        (should-not (string-search root archived))))))

(ert-deftest pos/the-stamp-goes-on-the-archived-entry-not-its-last-child ()
  "ARCHIVE_FILE and ARCHIVE_TIME are set on the entry archived, not below it.
Org leaves point at the end of the pasted subtree, in its last child;
the stamp climbs back to the entry itself, so a parent with done
children is stamped once, on the parent."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-test-lines "* DONE parent"
                                         "** DONE child one"
                                         "** DONE child two")))
    (let ((file (expand-file-name "intray.org" root)))
      (pos-archive-done-in-file root file pos-test-week pos-test-sunday)
      (with-temp-buffer
        (insert-file-contents (pos-archive-file root file pos-test-week))
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\* DONE parent")
        (should (equal "2026-09-06 Sun 23:00"
                       (org-entry-get (point) "ARCHIVE_TIME")))
        (should (equal "intray.org" (org-entry-get (point) "ARCHIVE_FILE")))
        (re-search-forward "^\\*\\* DONE child two")
        (should-not (org-entry-get (point) "ARCHIVE_TIME"))
        (should-not (org-entry-get (point) "ARCHIVE_FILE"))))))

(ert-deftest pos/archiving-writes-no-backup-files ()
  "Archiving saves the source and the archive without backup files.
The repository is the history; a stray intray.org~ beside the file is
clutter for Git to ignore or a person to delete."
  (pos-test-with-files root `(("intray.org" . ,pos-test-intray))
    (let ((file (expand-file-name "intray.org" root))
          (backup-enable-predicate (lambda (_name) t)))
      (pos-archive-done-in-file root file pos-test-week pos-test-sunday)
      (should-not (directory-files-recursively root "~\\'")))))

;;;; The sweep

(ert-deftest pos/a-repository-s-pos-config-el-sets-what-it-names ()
  "Loading a root's pos-config.el sets the settings it names and says so.
The test directory holds one of the shape a repository keeps, and
`pos-load-config' returns non-nil for it.  A root without one is no
error: the tools run with the defaults, and the result is nil."
  (let (pos-pillars pos-prose-directories pos-refile-rules)
    (should (pos-load-config pos-test-directory))
    (should (equal '("life" "sport" "people" "work" "body" "meta")
                   pos-pillars))
    (should (equal '("meta/journal" "meta/specs") pos-prose-directories))
    (should (equal '(("invoice\\|client" . "work")
                     ("dentist\\|checkup" . "body"))
                   pos-refile-rules)))
  (should-not (pos-load-config "/nonexistent/")))

(ert-deftest pos/a-sweep-archives-every-covered-file-and-no-other ()
  "A sweep archives the done entries of every covered file, and only those.
Each file's archive is written under the week by the file's own path;
a done entry in an uncovered file is left where it is, for lint to
report as stranded.  The counts and skipped headings are totalled."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . ,pos-test-intray)
          ("life/life-projects.org" . "* DONE shipped\n* TODO next\n")
          ("life/resources/book.org" . "* DONE not covered\n"))
      (let* ((pos-directory root)
             (result (pos-sweep pos-test-week pos-test-sunday)))
        (should (equal 4 (plist-get result :archived)))
        (should (equal '("parent with open child")
                       (plist-get result :skipped)))
        (should (file-exists-p
                 (expand-file-name
                  "archive/orgmode/2026-W36/intray.org_archive" root)))
        (should (file-exists-p
                 (expand-file-name
                  "archive/orgmode/2026-W36/life/life-projects.org_archive"
                  root)))
        (should (equal "* DONE not covered\n"
                       (pos-test-text root "life/resources/book.org")))))))

(ert-deftest pos/a-sweep-without-arguments-finds-its-own-week ()
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
               (pos-test-text root archive))))))

(ert-deftest pos/the-report-names-each-skipped-entry ()
  "The sweep's report gives the counts, then each skipped heading by name.
A done entry with open children is the one thing the sweep leaves for
a person to resolve, so the report says which."
  (let ((result '(:archived 194 :skipped ("call the plumber" "renew licence"))))
    (should (equal (pos-test-lines "Sweep 2026-W36: archived 194, skipped 2"
                                   "  skipped (done, but has open children):"
                                   "    call the plumber"
                                   "    renew licence")
                   (concat (pos-report "2026-W36" result) "\n")))))

(ert-deftest pos/the-report-says-nothing-of-skipping-when-there-is-none ()
  "With nothing skipped, the report is its one line of counts."
  (should (equal "Sweep 2026-W36: archived 3, skipped 0"
                 (pos-report "2026-W36" '(:archived 3 :skipped nil)))))

;;;; Normalising keywords

(ert-deftest pos/normalising-strips-todo-lines-and-respells-canceled ()
  "Normalising removes a file's #+TODO lines and respells CANCELED headings.
A per-file sequence drifts from the one sequence, so the line goes;
CANCELED is the retired spelling of CANCELLED.  The word is respelled
only as a heading's keyword, never inside a heading's text."
  (pos-test-with-files root
      `(("life/life-projects.org"
         . ,(pos-test-lines "#+TITLE: Projects"
                            "#+TODO: BACKLOG TODO | DONE CANCELED"
                            "* CANCELED dropped"
                            "* TODO cancel the CANCELED thing")))
    (let ((file (expand-file-name "life/life-projects.org" root)))
      (should (equal '(:lines-removed 1 :respelled 1)
                     (pos-normalise-keywords-in-file file)))
      (should (equal (pos-test-lines "#+TITLE: Projects"
                                     "* CANCELLED dropped"
                                     "* TODO cancel the CANCELED thing")
                     (pos-test-file-string file))))))

(ert-deftest pos/normalising-leaves-a-clean-file-untouched ()
  "A file with no #+TODO line and no CANCELED is not rewritten.
Nothing to count, nothing saved: Git sees no change."
  (pos-test-with-files root '(("intray.org" . "* TODO open\n"))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal '(:lines-removed 0 :respelled 0)
                     (pos-normalise-keywords-in-file file)))
      (should (equal "* TODO open\n" (pos-test-file-string file))))))

(ert-deftest pos/normalising-covers-every-covered-file-and-no-other ()
  "Normalising runs over the covered files and leaves uncovered ones alone.
A lower-case #+todo: line counts too.  A book's own keyword line, below
a pillar, is not the sweep's business."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "#+TODO: TODO | DONE\n* TODO a\n")
          ("life/life-areas.org" . "#+todo: TODO WIP | DONE\n* WIP b\n")
          ("life/resources/book.org" . "#+TODO: TODO | DONE\n"))
      (let ((pos-directory root))
        (should (equal '(:lines-removed 2 :respelled 0)
                       (pos-normalise-keywords)))
        (should (equal "#+TODO: TODO | DONE\n"
                       (pos-test-text root "life/resources/book.org")))))))

;;;; Lint

(ert-deftest pos/a-done-entry-with-an-open-child-is-a-finding ()
  "Lint reports a done entry that has an open task below it.
It is the shape the sweep skips.  A finding is (FILE LINE MESSAGE),
the line being the done entry's; a done entry with nothing open below
is not reported."
  (pos-test-with-files root
      '(("intray.org" . "* DONE parent\n** TODO child\n* DONE fine\n"))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (list file 1 "done entry has open children"))
                     (pos-lint-done-with-open-children file))))))

(ert-deftest pos/a-clean-tree-lints-silent ()
  "A tree with nothing to report lints to no findings and empty text.
Open and done tasks in the right places are the normal state."
  (pos-test-with-files root '(("intray.org" . "* TODO open\n* DONE fine\n"))
    (let ((pos-directory root))
      (should-not (pos-lint))
      (should (equal "" (pos-lint-format nil root))))))

(ert-deftest pos/findings-print-file-line-message-relative-to-the-root ()
  "Findings print as file:line: message, relative to the root, in order.
The order is by file then line, so a compilation buffer walks the
tree in one order and a diff of two runs is meaningful."
  (pos-test-configured
    (pos-test-with-files root
        '(("life/life-areas.org" . "* DONE other parent\n** TODO other child\n")
          ("intray.org" . "* DONE parent\n** TODO child\n"))
      (let ((pos-directory root))
        (should (equal (pos-test-lines
                        "intray.org:1: done entry has open children"
                        "life/life-areas.org:1: done entry has open children")
                       (pos-lint-format (pos-lint) root)))))))

(ert-deftest pos/a-heading-beginning-with-a-retired-keyword-is-a-finding ()
  "A heading beginning with a retired keyword is plain text, and reported.
CANCELED and CLARIFY were once in the sequence; Org now reads them as
the first word of a title, so the task is open and invisible.  A word
never in any sequence, like LATER, is just a word."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-test-lines "* CANCELED old"
                                         "* CLARIFY me"
                                         "* LATER timesheets"
                                         "* TODO fine")))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (list file 1
                                 "stale keyword CANCELED (not in the sequence)")
                           (list file 2
                                 "stale keyword CLARIFY (not in the sequence)"))
                     (pos-lint-stale-keyword file))))))

(ert-deftest pos/a-todo-line-in-a-file-is-a-finding ()
  "A #+TODO line in a covered file is reported, with the fix to run.
It overrides the one sequence for that file; `pos-normalise-keywords'
removes it.  The match is case-insensitive, as Org's is."
  (pos-test-with-files root
      '(("life/life-areas.org" . "#+TITLE: x\n#+todo: TODO | DONE\n* TODO a\n"))
    (let ((file (expand-file-name "life/life-areas.org" root))
          (text (concat "#+TODO line overrides the one sequence;"
                        " run pos-normalise-keywords")))
      (should (equal (list (list file 2 text))
                     (pos-lint-todo-line file))))))

(ert-deftest pos/a-merged-copy-left-by-dedupe-is-a-finding ()
  "A \"Merged copy from\" heading is reported until a person reconciles it.
Dedupe folds a dropped copy's differing body under the kept one with
that heading; the finding keeps it from being forgotten there."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-test-lines
                          "* TODO fix the gate"
                          "hinge"
                          "** Merged copy from life/life-areas.org:4"
                          "welder")))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (list file 3
                                 (concat "merged copy awaiting reconciliation"
                                         " with its parent")))
                     (pos-lint-merged-copy file))))))

(ert-deftest pos/a-task-keyword-in-an-uncovered-file-is-stranded ()
  "A task keyword in an uncovered file is stranded, and lint reports it.
The sweep never visits the file, so DONE there would never archive
and TODO never reach an agenda.  Archive files are not uncovered, so
the swept tasks in them are not stranded."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "* TODO covered\n")
          ("life/resources/book.org" . "* Chapter\n** TODO write it\n")
          ("sport/resources/zettles/x.org" . "* DONE old\n")
          ("archive/orgmode/2026-W35/intray.org_archive" . "* DONE swept\n")
          ("archive/orgmode/2026-W35/notes.org" . "* TODO ignored\n"))
      (let ((book (expand-file-name "life/resources/book.org" root))
            (zettel (expand-file-name "sport/resources/zettles/x.org" root))
            (outside "task keyword outside the agenda files (%s)"))
        (should (equal (list (list book 2 (format outside "TODO"))
                             (list zettel 1 (format outside "DONE")))
                       (pos-lint-stranded-tasks root)))))))

(ert-deftest pos/copies-of-one-task-in-two-covered-files-name-each-other ()
  "Copies of one task heading in two covered files each report the other.
Headings compare lower-cased and trimmed, so Fix the gate and fix the
gate are one task.  A plain heading, Finances, is no task and is not
compared."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "* TODO fix the gate\n* Finances\n")
          ("life/life-areas.org" . "* Finances\n** BACKLOG Fix the gate\n"))
      (should (equal (list (list (expand-file-name "intray.org" root) 1
                                 "duplicate task (also life/life-areas.org:2)")
                           (list (expand-file-name "life/life-areas.org" root) 2
                                 "duplicate task (also intray.org:1)"))
                     (pos-lint-duplicate-tasks root))))))

;;;; Stranded tasks

(ert-deftest pos/only-the-topmost-open-task-of-a-nest-is-rescued ()
  "A stranded open task is listed once, at the top of its nest.
Rescue lists open tasks only: a done one stays where it is, for lint
to report, while an open one's subtasks will move with it and are not
listed again."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . "* Unsorted\n")
          ("life/resources/notes.org" . ,(pos-test-lines "* Seminar"
                                                         "** TODO follow up"
                                                         "*** TODO nested"
                                                         "** DONE said hello")))
      (should (equal (list (list (expand-file-name "life/resources/notes.org"
                                                   root)
                                 2 "follow up"))
                     (pos-stranded-open-tasks root))))))

(ert-deftest pos/a-rescued-task-lands-under-unsorted-linked-back ()
  "A rescued task is moved under the intray's Unsorted, with a link back.
The link names the file and the parent heading it came from, relative
to the root, and goes after the planning line so Org still reads the
schedule.  Nested open tasks move with it; the file keeps the rest."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . ,(pos-test-lines "* Zettles"
                                           "* Unsorted"
                                           "** TODO already here"
                                           "* Sorted"))
          ("life/resources/notes.org"
           . ,(pos-test-lines "* Seminar"
                              "** TODO follow up"
                              "SCHEDULED: <2026-09-14 Mon>"
                              "some notes"
                              "*** TODO nested"
                              "** DONE said hello")))
      (let ((pos-directory root))
        (pos-refile-stranded)
        (should (equal "* Seminar\n** DONE said hello\n"
                       (pos-test-text root "life/resources/notes.org")))
        (should (equal (pos-test-lines
                        "* Zettles"
                        "* Unsorted"
                        "** TODO already here"
                        "** TODO follow up"
                        "SCHEDULED: <2026-09-14 Mon>"
                        (concat "From "
                                "[[file:life/resources/notes.org::*Seminar]"
                                "[life/resources/notes.org: Seminar]]")
                        "some notes"
                        "*** TODO nested"
                        "* Sorted")
                       (pos-test-text root "intray.org")))))))

(ert-deftest pos/rescue-makes-unsorted-when-the-intray-has-none ()
  "An intray with no Unsorted heading gains one at its end for the rescue.
A task from a file's top level has no parent to name, so its link
names the file alone."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "* Sorted\n")
          ("life/resources/notes.org" . "* TODO top level task\n"))
      (let ((pos-directory root))
        (pos-refile-stranded)
        (should (equal (pos-test-lines
                        "* Sorted"
                        "* Unsorted"
                        "** TODO top level task"
                        (concat "From [[file:life/resources/notes.org]"
                                "[life/resources/notes.org]]"))
                       (pos-test-text root "intray.org")))))))

(ert-deftest pos/a-dry-run-rescue-lists-the-tasks-and-moves-none ()
  "A dry run returns what it would move and changes neither file."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "* Unsorted\n")
          ("life/resources/notes.org" . "* TODO a task\n"))
      (let ((pos-directory root))
        (should (equal 1 (length (pos-refile-stranded t))))
        (should (equal "* TODO a task\n"
                       (pos-test-text root "life/resources/notes.org")))
        (should (equal "* Unsorted\n" (pos-test-text root "intray.org")))))))

(ert-deftest pos/unsorted-ends-before-the-next-top-heading ()
  "The end of Unsorted is before the next top-level heading, not the file's.
Anything inserted there lands inside Unsorted."
  (with-temp-buffer
    (org-mode)
    (insert "* Unsorted\n** TODO here\n* Sorted\n")
    (pos-goto-unsorted-end)
    (insert "** TODO new\n")
    (should (equal "* Unsorted\n** TODO here\n** TODO new\n* Sorted\n"
                   (buffer-string)))))

(ert-deftest pos/unsorted-is-made-at-the-end-when-missing ()
  "Without an Unsorted heading, one is appended on a line of its own.
A file not ending in a newline gets one first, so that the heading is
a heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Sorted")
    (pos-goto-unsorted-end)
    (insert "** TODO new\n")
    (should (equal "* Sorted\n* Unsorted\n** TODO new\n" (buffer-string)))))

;;;; Dedupe

(defun pos-test-copy (file line &optional body parent)
  "Return the `pos--task-entries' entry for a copy of \"Fix the gate\".
The copy is in FILE under /repo/, at LINE, with BODY, default empty,
under PARENT, default none, and has no ID."
  (list :key "fix the gate" :heading "Fix the gate"
        :file (expand-file-name file "/repo/") :line line
        :parent parent :body (or body "") :id nil))

(ert-deftest pos/the-intray-copy-is-dropped-for-the-pillar-copy ()
  "Of a copy in the intray and one in a pillar, the pillar's is kept.
The intray is where a task waits to be filed; a copy already in its
pillar is the task filed.  The suggestion pairs an action with each
entry, in the group's order, whichever way round the group comes."
  (let ((intray (pos-test-copy "intray.org" 1))
        (pillar (pos-test-copy "life/life-areas.org" 2
                               "hinge is bent" "Household")))
    (should (equal (list (cons "drop" intray) (cons "keep" pillar))
                   (pos-dedupe-suggest (list intray pillar) "/repo/")))
    (should (equal '("keep" "drop")
                   (mapcar #'car (pos-dedupe-suggest (list pillar intray)
                                                     "/repo/"))))))

(ert-deftest pos/two-pillar-copies-are-a-question ()
  "Copies in two pillar files are both marked ?, for a person to decide.
Neither pillar outranks the other.  An intray copy in the same group
does not settle it, and is a question too."
  (let ((life (pos-test-copy "life/life-areas.org" 2))
        (people (pos-test-copy "people/people-areas.org" 5))
        (intray (pos-test-copy "intray.org" 1)))
    (should (equal '("?" "?")
                   (mapcar #'car (pos-dedupe-suggest (list life people)
                                                     "/repo/"))))
    (should (equal '("?" "?" "?")
                   (mapcar #'car (pos-dedupe-suggest (list intray life people)
                                                     "/repo/"))))))

(ert-deftest pos/in-one-file-the-longer-body-is-kept ()
  "Of two copies in one file, the one with the longer body is kept.
More said is more to lose; the other is dropped and, if its body
differs, merged under the kept one when the plan is applied.  Bodies
of equal length keep the first."
  (let ((bare (pos-test-copy "intray.org" 1))
        (said (pos-test-copy "intray.org" 2 "hinge is bent"))
        (later (pos-test-copy "intray.org" 3)))
    (should (equal '("drop" "keep")
                   (mapcar #'car (pos-dedupe-suggest (list bare said)
                                                     "/repo/"))))
    (should (equal '("keep" "drop")
                   (mapcar #'car (pos-dedupe-suggest (list said bare)
                                                     "/repo/"))))
    (should (equal '("keep" "drop")
                   (mapcar #'car (pos-dedupe-suggest (list bare later)
                                                     "/repo/"))))))

(ert-deftest pos/an-excerpt-is-the-first-line-of-the-body-proper ()
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

(ert-deftest pos/an-excerpt-fits-a-table-cell ()
  "An excerpt has its bars made slashes and is cut to sixty columns.
A bar would end the plan table's cell; a long line would push the
other columns off the screen.  The cut is marked with an ellipsis."
  (should (equal "hinge / latch" (pos--excerpt "* TODO x\nhinge | latch")))
  (should (equal (concat (make-string 59 ?a) "…")
                 (pos--excerpt (concat "* TODO x\n" (make-string 70 ?a))))))

(ert-deftest pos/a-dedupe-plan-round-trips-through-its-reader ()
  "A written dedupe plan reads back as the groups and rows it was made from.
The plan is Org a person edits: a heading per duplicate group, and a
table row per copy with the suggested action, file, line, parent
heading, body lines and ID.  The reader takes the action, file and
line, and lower-cases each heading into its group's key."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "* TODO fix the gate\n")
          ("life/life-areas.org" . "* Household\n** TODO Fix the gate\n"))
      (let ((plan (expand-file-name "dedupe.org" root)))
        (with-temp-file plan (insert (pos-dedupe-plan root)))
        (should (equal '(("fix the gate"
                          ("drop" "intray.org" 1)
                          ("keep" "life/life-areas.org" 2)))
                       (pos-dedupe-read-plan plan)))
        (should (string-match-p "^\\* fix the gate$"
                                (pos-test-file-string plan)))
        (should (string-match-p "| Household " (pos-test-file-string plan)))))))

(ert-deftest pos/a-dropped-copy-with-nothing-to-say-is-cut ()
  "A dropped copy whose body is empty is cut, and the kept copy is unchanged.
There is nothing to merge; the group counts as resolved, and both
files are saved.  The table's header row is not a copy."
  (pos-test-with-files root
      `(("intray.org" . "* Unsorted\n** TODO fix the gate\n** TODO other\n")
        ("life/life-areas.org"
         . "* Household\n** TODO Fix the gate\nhinge is bent\n")
        ("dedupe.org"
         . ,(pos-test-lines
             "* fix the gate"
             "| act | file | line | under | body lines |"
             "| drop | intray.org | 2 | Unsorted | 0 |"
             "| keep | life/life-areas.org | 2 | Household | 1 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (equal (list :resolved 1 :merged 0 :relinked 0
                           :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "* Unsorted\n** TODO other\n"
                     (pos-test-text root "intray.org")))
      (should (equal "* Household\n** TODO Fix the gate\nhinge is bent\n"
                     (pos-test-text root "life/life-areas.org"))))))

(ert-deftest pos/a-dropped-body-that-differs-merges-under-the-kept-copy ()
  "A dropped copy whose body differs is folded under the kept copy, then cut.
The fold is a child of the kept copy headed Merged copy from FILE:LINE,
without a keyword, holding the dropped body and its subtasks; lint
reports it until a person reconciles it.  The intray is left empty."
  (pos-test-with-files root
      `(("intray.org"
         . "* TODO fix the gate\ncall the welder\n** TODO get a quote\n")
        ("life/life-areas.org" . ,(pos-test-lines "* Household"
                                                  "** TODO Fix the gate"
                                                  "hinge is bent"
                                                  "** TODO next"))
        ("dedupe.org"
         . ,(pos-test-lines
             "* fix the gate"
             "| drop | intray.org | 1 | | 2 |"
             "| keep | life/life-areas.org | 2 | Household | 1 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (equal (list :resolved 1 :merged 1 :relinked 0
                           :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "" (pos-test-text root "intray.org")))
      (should (equal (pos-test-lines "* Household"
                                     "** TODO Fix the gate"
                                     "hinge is bent"
                                     "*** Merged copy from intray.org:1"
                                     "call the welder"
                                     "**** TODO get a quote"
                                     "** TODO next")
                     (pos-test-text root "life/life-areas.org"))))))

(ert-deftest pos/an-undecided-group-is-skipped-and-a-dry-run-cuts-nothing ()
  "A group left as questions is skipped, and a dry run changes no file.
A group is acted on only with one keep and at least one drop; with ?
rows it counts as skipped.  The decided group counts as resolved, as
it would be, and the files are as they were."
  (pos-test-with-files root
      `(("intray.org" . "* TODO fix the gate\n* TODO paint\n")
        ("life/life-areas.org" . "* TODO fix the gate\n* TODO paint\n")
        ("dedupe.org"
         . ,(pos-test-lines "* fix the gate"
                            "| ? | intray.org | 1 | | 0 |"
                            "| ? | life/life-areas.org | 1 | | 0 |"
                            "* paint"
                            "| drop | intray.org | 2 | | 0 |"
                            "| keep | life/life-areas.org | 2 | | 0 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (equal (list :resolved 1 :merged 0 :relinked 0
                           :skipped 1 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan t)))
      (should (equal "* TODO fix the gate\n* TODO paint\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos/applying-a-plan-saves-every-file-it-touched ()
  "Every file a plan cuts from is saved, not only the last group's.
Two groups across four files: each dropped copy's file is written
back empty."
  (pos-test-with-files root
      `(("intray.org" . "* TODO fix the gate\n")
        ("life/life-areas.org" . "* TODO fix the gate\n")
        ("people/people-areas.org" . "* TODO paint\n")
        ("sport/sport-areas.org" . "* TODO paint\n")
        ("dedupe.org"
         . ,(pos-test-lines "* fix the gate"
                            "| drop | intray.org | 1 | | 0 |"
                            "| keep | life/life-areas.org | 1 | | 0 |"
                            "* paint"
                            "| drop | people/people-areas.org | 1 | | 0 |"
                            "| keep | sport/sport-areas.org | 1 | | 0 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (equal (list :resolved 2 :merged 0 :relinked 0
                           :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "" (pos-test-text root "intray.org")))
      (should (equal "" (pos-test-text root "people/people-areas.org"))))))

(ert-deftest pos/a-copy-cut-with-its-parent-is-counted-vanished ()
  "A copy already cut inside an earlier group's subtree is counted vanished.
Markers are placed before any edit; a marker whose heading is no
longer there is passed over, rather than cutting whatever text has
moved into its place."
  (pos-test-with-files root
      `(("intray.org" . "* TODO shed\n** TODO fix the gate\n* TODO unrelated\n")
        ("life/life-areas.org" . "* TODO shed\n** TODO fix the gate\n")
        ("dedupe.org"
         . ,(pos-test-lines "* shed"
                            "| drop | intray.org | 1 | | 1 |"
                            "| keep | life/life-areas.org | 1 | | 1 |"
                            "* fix the gate"
                            "| drop | intray.org | 2 | shed | 0 |"
                            "| keep | life/life-areas.org | 2 | shed | 0 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (equal (list :resolved 2 :merged 0 :relinked 0
                           :skipped 0 :vanished 1 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "* TODO unrelated\n" (pos-test-text root "intray.org"))))))

(ert-deftest pos/links-to-a-dropped-copy-point-at-the-kept-one ()
  "Each id: link to a dropped copy's ID is pointed at the kept copy's ID.
The plan lists the links the index records, one per link, so a person
sees what will change; a dry run counts them and rewrites none; the
merged note under the kept copy does not keep the dropped ID, so no
link lands on the note."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . ,(pos-test-lines ":PROPERTIES:"
                                           ":ID: file-i"
                                           ":END:"
                                           "* TODO fix the gate"
                                           ":PROPERTIES:"
                                           ":ID: dropped"
                                           ":END:"
                                           "call the welder"))
          ("life/life-areas.org" . ,(pos-test-lines "* Household"
                                                    "** TODO Fix the gate"
                                                    ":PROPERTIES:"
                                                    ":ID: kept"
                                                    ":END:"
                                                    "hinge is bent"))
          ("notes.org"
           . ,(pos-test-lines ":PROPERTIES:"
                              ":ID: file-n"
                              ":END:"
                              (concat "See [[id:dropped][the gate]],"
                                      " and [[id:dropped]] again.")))
          ("dedupe.org"
           . ,(pos-test-lines
               "* fix the gate"
               "| drop | intray.org | 4 | | 1 |"
               "| keep | life/life-areas.org | 2 | Household | 1 |")))
      (let ((plan (expand-file-name "dedupe.org" root))
            (counts (list :resolved 1 :merged 1 :relinked 2
                          :skipped 0 :vanished 0 :stale 0)))
        (should (string-match-p
                 "^- Links to intray.org:4 from notes.org:4, notes.org:4$"
                 (pos-dedupe-plan root)))
        (should (string-match-p "| dropped *|" (pos-dedupe-plan root)))
        (should (equal counts (pos-dedupe-apply root plan t)))
        (should (string-match-p "id:dropped" (pos-test-text root "notes.org")))
        (should (equal counts (pos-dedupe-apply root plan)))
        (should (equal (pos-test-lines ":PROPERTIES:"
                                       ":ID: file-n"
                                       ":END:"
                                       (concat "See [[id:kept][the gate]],"
                                               " and [[id:kept]] again."))
                       (pos-test-text root "notes.org")))
        (should (equal (pos-test-lines "* Household"
                                       "** TODO Fix the gate"
                                       ":PROPERTIES:"
                                       ":ID: kept"
                                       ":END:"
                                       "hinge is bent"
                                       "*** Merged copy from intray.org:4"
                                       "call the welder")
                       (pos-test-text root "life/life-areas.org")))))))

(ert-deftest pos/a-kept-copy-without-an-id-takes-the-first-dropped-one-s ()
  "A kept copy with no ID takes the first dropped copy's, so its links hold.
A second dropped ID's links are then pointed at it, and counted.  The
ID property is written as Org writes one, padded to its column."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-test-lines ":PROPERTIES:"
                                         ":ID: file-i"
                                         ":END:"
                                         "* TODO fix the gate"
                                         ":PROPERTIES:"
                                         ":ID: first"
                                         ":END:"
                                         "* TODO Fix the gate"
                                         ":PROPERTIES:"
                                         ":ID: second"
                                         ":END:"))
        ("life/life-areas.org" . "* Household\n** TODO Fix the gate\n")
        ("notes.org" . ,(pos-test-lines ":PROPERTIES:"
                                        ":ID: file-n"
                                        ":END:"
                                        "[[id:first]] [[id:second]]"))
        ("dedupe.org"
         . ,(pos-test-lines
             "* fix the gate"
             "| drop | intray.org | 4 | | 0 |"
             "| drop | intray.org | 8 | | 0 |"
             "| keep | life/life-areas.org | 2 | Household | 0 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (equal (list :resolved 1 :merged 0 :relinked 1
                           :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal (pos-test-lines ":PROPERTIES:"
                                     ":ID: file-n"
                                     ":END:"
                                     "[[id:first]] [[id:first]]")
                     (pos-test-text root "notes.org")))
      (should (equal (pos-test-lines "* Household"
                                     "** TODO Fix the gate"
                                     ":PROPERTIES:"
                                     ":ID:       first"
                                     ":END:")
                     (pos-test-text root "life/life-areas.org")))
      (should (equal ":PROPERTIES:\n:ID: file-i\n:END:\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos/a-plan-older-than-the-file-is-stale-not-fatal ()
  "A plan whose line no longer holds its heading is stale, and nothing is cut.
A file edited since the plan was written has moved its headings; the
whole group counts as stale before any edit, so a group can never be
half applied."
  (pos-test-with-files root
      `(("intray.org" . "* TODO fix the gate\n")
        ("life/life-areas.org" . "* Household\n** TODO fix the gate\n")
        ("dedupe.org"
         . ,(pos-test-lines
             "* fix the gate"
             "| drop | intray.org | 1 | | 0 |"
             "| keep | life/life-areas.org | 3 | Household | 0 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (equal (list :resolved 0 :merged 0 :relinked 0
                           :skipped 0 :vanished 0 :stale 1)
                     (pos-dedupe-apply root plan)))
      (should (equal "* TODO fix the gate\n"
                     (pos-test-text root "intray.org"))))))

;;;; Refile

(ert-deftest pos/the-first-matching-refile-rule-wins ()
  "A heading is suggested the pillar of the first rule it matches, or none.
Rules match without regard to case; a heading no rule matches has no
suggestion, and is left for a person."
  (pos-test-configured
    (should (equal "work" (pos-refile-suggest "Client invoice query")))
    (should (equal "body" (pos-refile-suggest "Dentist checkup")))
    (should-not (pos-refile-suggest "Visitor arrives"))))

(ert-deftest pos/the-refile-plan-has-one-row-per-intray-entry ()
  "The refile plan has a row per level-two intray entry, with a suggestion.
A matched entry is a move to its pillar's projects file; the rest are
questions.  Each entry is quoted whole below the table in an example
block, its headings escaped, so the plan, a covered file while it
sits in the root, is not itself a file of duplicate tasks."
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

(ert-deftest pos/a-move-lands-under-the-named-heading-or-at-the-end ()
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
      (should (equal '(:moved 2 :left 1 :missing 0 :vanished 0)
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

(ert-deftest pos/an-outline-path-targets-the-nested-heading-not-a-namesake ()
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
      (should (equal '(:moved 1 :left 0 :missing 0 :vanished 0)
                     (pos-refile-apply root plan)))
      (should (equal (pos-test-lines "* Operations"
                                     "** Housekeeping"
                                     "*** existing"
                                     "*** TODO Invoicing"
                                     "** Delivery"
                                     "* Housekeeping")
                     (pos-test-text root "work/work-areas.org"))))))

(ert-deftest pos/a-missing-target-or-a-moved-line-is-counted-not-fatal ()
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
      (should (equal '(:moved 0 :left 0 :missing 2 :vanished 1)
                     (pos-refile-apply root plan)))
      (should (equal "* Unsorted\n** TODO a\n** TODO b\n** TODO c\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos/a-dry-run-refile-moves-nothing ()
  "A dry run counts what it would move and changes no file."
  (pos-test-with-files root
      '(("intray.org" . "* Unsorted\n** TODO a\n")
        ("body/body-projects.org" . "* Appointments\n")
        ("refile.org"
         . "| move | 2 | a | body/body-projects.org | Appointments |\n"))
    (let ((plan (expand-file-name "refile.org" root)))
      (should (equal '(:moved 1 :left 0 :missing 0 :vanished 0)
                     (pos-refile-apply root plan t)))
      (should (equal "* Unsorted\n** TODO a\n"
                     (pos-test-text root "intray.org"))))))

(provide 'pos-test)
;;; pos-test.el ends here
