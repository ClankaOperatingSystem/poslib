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

;; Run: make test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pos)

(pos-load-config (file-name-directory (or load-file-name buffer-file-name)))

;; The index of each temporary repository goes to a cache of the tests'
;; own, never to a database of the user's.
(setq pos-roam-cache-directory (make-temp-file "pos-test-roam-" t))

(defun pos-test-time (year month day hour minute)
  "Return the local time YEAR-MONTH-DAY HOUR:MINUTE as a time value."
  (encode-time (list 0 minute hour day month year nil -1 nil)))

(ert-deftest pos-week-name/sunday-night-belongs-to-its-iso-week ()
  "Sunday 2026-09-06 is the last day of ISO week 36."
  (should (equal "2026-W36"
                 (pos-week-name (pos-test-time 2026 9 6 23 0)))))

(ert-deftest pos-week-name/year-boundary-follows-iso-rules ()
  "2026-01-01 is 2026-W01; 2027-01-01 is 2026-W53."
  (should (equal "2026-W01" (pos-week-name (pos-test-time 2026 1 1 12 0))))
  (should (equal "2026-W53" (pos-week-name (pos-test-time 2027 1 1 12 0)))))

(ert-deftest pos-sweep-boundary/on-the-boundary-is-the-boundary ()
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                 (pos-sweep-boundary (pos-test-time 2026 9 6 23 0)))))

(ert-deftest pos-sweep-boundary/a-minute-early-is-last-week ()
  (should (time-equal-p (pos-test-time 2026 8 30 23 0)
                 (pos-sweep-boundary (pos-test-time 2026 9 6 22 59)))))

(ert-deftest pos-sweep-boundary/a-late-run-on-monday-still-finds-sunday ()
  "A late run still sweeps the week just ended."
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                 (pos-sweep-boundary (pos-test-time 2026 9 7 8 15)))))

(ert-deftest pos-sweep-boundary/midweek-finds-the-previous-sunday ()
  (should (time-equal-p (pos-test-time 2026 9 6 23 0)
                 (pos-sweep-boundary (pos-test-time 2026 9 9 12 0)))))

(ert-deftest pos-sweep-boundary/crosses-a-month-boundary ()
  "Tuesday 2026-09-01: the previous Sunday is in August."
  (should (time-equal-p (pos-test-time 2026 8 30 23 0)
                 (pos-sweep-boundary (pos-test-time 2026 9 1 9 0)))))

(ert-deftest pos-archive-file/root-file ()
  (should (equal "/repo/archive/orgmode/2026-W36/intray.org_archive"
                 (pos-archive-file "/repo" "/repo/intray.org" "2026-W36"))))

(ert-deftest pos-archive-file/pillar-file-keeps-its-subpath ()
  (should (equal "/repo/archive/orgmode/2026-W36/life/life-areas.org_archive"
                 (pos-archive-file "/repo" "/repo/life/life-areas.org" "2026-W36"))))

(ert-deftest pos-archive-file/root-may-have-a-trailing-slash ()
  (should (equal "/repo/archive/orgmode/2026-W36/intray.org_archive"
                 (pos-archive-file "/repo/" "/repo/intray.org" "2026-W36"))))

(defmacro pos-test-with-repo (files &rest body)
  "Run BODY with `root' bound to a temporary repository of FILES.
FILES: (RELATIVE-PATH . CONTENT) pairs."
  (declare (indent 1))
  `(let ((root (file-name-as-directory
                (make-temp-file "pos-test-" t))))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (let ((path (expand-file-name (car file) root)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))))
           ,@body)
       (delete-directory root t))))

(defun pos-test-relative (files root)
  "Return FILES as paths relative to ROOT, sorted."
  (sort (mapcar (lambda (f) (file-relative-name f root)) files)
        #'string<))

(ert-deftest pos-org-files/root-and-pillars-non-recursive ()
  (pos-test-with-repo '(("intray.org" . "")
                        ("todo.org" . "")
                        ("life/life-areas.org" . "")
                        ("life/life-projects.org" . "")
                        ("life/resources/book.org" . "")
                        ("sport/resources/zettles/a.org" . "")
                        ("archive/orgmode/2026-W35/intray.org_archive" . "")
                        ("notes.txt" . ""))
    (progn
      (should (equal '("intray.org" "life/life-areas.org" "life/life-projects.org"
                       "todo.org")
                     (pos-test-relative (pos-org-files root) root))))))

(ert-deftest pos-org-files/missing-pillar-directory-is-not-an-error ()
  "Missing pillars are skipped."
  (pos-test-with-repo '(("intray.org" . ""))
    (let ((pos-pillars '("life" "nowhere")))
      (should (equal '("intray.org")
                     (pos-test-relative (pos-org-files root) root))))))

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
")

(defun pos-test-file-string (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst pos-test-sunday (encode-time (list 0 0 23 6 9 2026 nil -1 nil))
  "Sunday 2026-09-06 23:00 local, the boundary used by these tests.")

(ert-deftest pos-archive-done-in-file/moves-done-entries-to-the-archive ()
  (pos-test-with-repo `(("intray.org" . ,pos-test-intray))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-archive-file root file "2026-W36"))
           (result (pos-archive-done-in-file root file "2026-W36" pos-test-sunday))
           (source (pos-test-file-string file))
           (archived (pos-test-file-string archive)))
      (should (equal 3 (plist-get result :archived)))
      (should (equal '("parent with open child") (plist-get result :skipped)))
      (should-not (string-search "finished task" source))
      (should-not (string-search "dropped task" source))
      (should-not (string-search "nested finished" source))
      (should (string-search "* TODO open task" source))
      (should (string-search "* DONE parent with open child" source))
      (should (string-search "** TODO still open" source))
      (should (string-search "** TODO nested open" source))
      (should (string-search "* DONE finished task" archived))
      (should (string-search "* CANCELLED dropped task" archived))
      (should (string-search "* DONE nested finished" archived))
      (should (equal 3 (cl-count-if
                        (lambda (line) (string-search "ARCHIVE_TIME: 2026-09-06 Sun 23:00" line))
                        (split-string archived "\n"))))
      (should (string-search ":ARCHIVE_OLPATH: Container" archived)))))

(ert-deftest pos-archive-done-in-file/nothing-to-do-creates-no-archive ()
  (pos-test-with-repo '(("todo.org" . "* TODO only open\n"))
    (let* ((file (expand-file-name "todo.org" root))
           (archive (pos-archive-file root file "2026-W36"))
           (result (pos-archive-done-in-file root file "2026-W36" pos-test-sunday)))
      (should (equal 0 (plist-get result :archived)))
      (should-not (file-exists-p archive)))))

(ert-deftest pos-archive-done-in-file/record-names-files-relative-to-root ()
  (pos-test-with-repo `(("life/life-areas.org" . "* DONE finished\n"))
    (let* ((file (expand-file-name "life/life-areas.org" root))
           (archive (pos-archive-file root file "2026-W36")))
      (pos-archive-done-in-file root file "2026-W36" pos-test-sunday)
      (let ((archived (pos-test-file-string archive)))
        (should (string-search ":ARCHIVE_FILE: life/life-areas.org" archived))
        (should (string-search "Archived entries from file life/life-areas.org" archived))
        (should-not (string-search root archived))))))

(ert-deftest pos-archive-done-in-file/stamps-the-entry-not-its-last-child ()
  (pos-test-with-repo '(("intray.org" . "* DONE parent\n** DONE child one\n** DONE child two\n"))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-archive-file root file "2026-W36")))
      (pos-archive-done-in-file root file "2026-W36" pos-test-sunday)
      (with-temp-buffer
        (insert-file-contents archive)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\* DONE parent")
        (should (equal "2026-09-06 Sun 23:00" (org-entry-get (point) "ARCHIVE_TIME")))
        (should (equal "intray.org" (org-entry-get (point) "ARCHIVE_FILE")))
        (re-search-forward "^\\*\\* DONE child two")
        (should-not (org-entry-get (point) "ARCHIVE_TIME"))
        (should-not (org-entry-get (point) "ARCHIVE_FILE"))))))

(ert-deftest pos-archive-done-in-file/writes-no-backup-files ()
  (pos-test-with-repo `(("intray.org" . ,pos-test-intray))
    (let ((file (expand-file-name "intray.org" root))
          (backup-enable-predicate (lambda (_name) t)))
      (pos-archive-done-in-file root file "2026-W36" pos-test-sunday)
      (should-not (directory-files-recursively root "~\\'")))))

(ert-deftest pos-sweep/archives-every-covered-file ()
  (pos-test-with-repo `(("intray.org" . ,pos-test-intray)
                        ("life/life-projects.org" . "* DONE shipped\n* TODO next\n")
                        ("life/resources/book.org" . "* DONE not covered\n"))
    (let* ((pos-directory root)
           (report (pos-sweep "2026-W36" pos-test-sunday)))
      (should (equal 4 (plist-get report :archived)))
      (should (equal '("parent with open child") (plist-get report :skipped)))
      (should (file-exists-p
               (expand-file-name "archive/orgmode/2026-W36/intray.org_archive" root)))
      (should (file-exists-p
               (expand-file-name "archive/orgmode/2026-W36/life/life-projects.org_archive" root)))
      (should (string-search "* DONE not covered"
                             (pos-test-file-string
                              (expand-file-name "life/resources/book.org" root)))))))

(ert-deftest pos-sweep/defaults-to-the-most-recent-boundary ()
  "With no arguments the sweep finds its own week."
  (pos-test-with-repo '(("intray.org" . "* DONE finished\n"))
    (let* ((pos-directory root)
           (week (pos-week-name (pos-sweep-boundary (current-time)))))
      (pos-sweep)
      (should (file-exists-p
               (expand-file-name (concat "archive/orgmode/" week "/intray.org_archive")
                                 root))))))

(ert-deftest pos-report/names-the-skipped-entries ()
  (should (equal (concat "Sweep 2026-W36: archived 194, skipped 2\n"
                         "  skipped (done, but has open children):\n"
                         "    call the plumber\n"
                         "    renew licence")
                 (pos-report "2026-W36"
                             '(:archived 194
                               :skipped ("call the plumber" "renew licence"))))))

(ert-deftest pos-report/says-nothing-about-skipping-when-there-is-none ()
  (should (equal "Sweep 2026-W36: archived 3, skipped 0"
                 (pos-report "2026-W36" '(:archived 3 :skipped nil)))))

(ert-deftest pos-done-keywords/are-the-ones-after-the-bar ()
  (should (equal '("DONE" "CANCELLED") (pos-done-keywords))))

(ert-deftest pos-archive-done-in-file/honours-the-universal-set-without-a-file-line ()
  (pos-test-with-repo '(("intray.org" . "* CANCELLED dropped\n* WIP busy\n* TODO open\n"))
    (let* ((file (expand-file-name "intray.org" root))
           (archive (pos-archive-file root file "2026-W36"))
           (result (pos-archive-done-in-file root file "2026-W36" pos-test-sunday)))
      (should (equal 1 (plist-get result :archived)))
      (should (string-search "* CANCELLED dropped" (pos-test-file-string archive)))
      (should (string-search "* WIP busy" (pos-test-file-string file))))))

(ert-deftest pos-normalise-keywords-in-file/strips-todo-lines-and-respells ()
  (pos-test-with-repo '(("life/life-projects.org" .
                         "#+TITLE: Projects\n#+TODO: BACKLOG TODO | DONE CANCELED\n* CANCELED dropped\n* TODO cancel the CANCELED thing\n"))
    (let* ((file (expand-file-name "life/life-projects.org" root))
           (result (pos-normalise-keywords-in-file file))
           (after (pos-test-file-string file)))
      (should (equal '(:lines-removed 1 :respelled 1) result))
      (should (equal "#+TITLE: Projects\n* CANCELLED dropped\n* TODO cancel the CANCELED thing\n"
                     after)))))

(ert-deftest pos-normalise-keywords-in-file/leaves-a-clean-file-alone ()
  (pos-test-with-repo '(("intray.org" . "* TODO open\n"))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal '(:lines-removed 0 :respelled 0)
                     (pos-normalise-keywords-in-file file)))
      (should (equal "* TODO open\n" (pos-test-file-string file))))))

(ert-deftest pos-normalise-keywords/covers-every-agenda-file ()
  (pos-test-with-repo '(("intray.org" . "#+TODO: TODO | DONE\n* TODO a\n")
                        ("life/life-areas.org" . "#+todo: TODO WIP | DONE\n* WIP b\n")
                        ("life/resources/book.org" . "#+TODO: TODO | DONE\n"))
    (let ((pos-directory root)
          )
      (should (equal '(:lines-removed 2 :respelled 0) (pos-normalise-keywords)))
      (should (string-search "#+TODO"
                             (pos-test-file-string
                              (expand-file-name "life/resources/book.org" root)))))))

(ert-deftest pos-lint-done-with-open-children/finds-the-skipped-shape ()
  (pos-test-with-repo '(("intray.org" . "* DONE parent\n** TODO child\n* DONE fine\n"))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (list file 1 "done entry has open children"))
                     (pos-lint-done-with-open-children file))))))

(ert-deftest pos-lint/is-silent-when-clean ()
  (pos-test-with-repo '(("intray.org" . "* TODO open\n* DONE fine\n"))
    (let ((pos-directory root) )
      (should (equal nil (pos-lint)))
      (should (equal "" (pos-lint-format nil root))))))

(ert-deftest pos-lint/formats-findings-relative-to-root-one-per-line ()
  (pos-test-with-repo '(("life/life-areas.org" . "* DONE other parent\n** TODO other child\n")
                        ("intray.org" . "* DONE parent\n** TODO child\n"))
    (let* ((pos-directory root)
           (findings (pos-lint)))
      (should (equal "intray.org:1: done entry has open children\nlife/life-areas.org:1: done entry has open children\n"
                     (pos-lint-format findings root))))))

(ert-deftest pos-lint-stale-keyword/names-the-old-words-only ()
  (pos-test-with-repo '(("intray.org" . "* CANCELED old\n* CLARIFY me\n* LATER timesheets\n* TODO fine\n"))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (list file 1 "stale keyword CANCELED (not in the sequence)")
                           (list file 2 "stale keyword CLARIFY (not in the sequence)"))
                     (pos-lint-stale-keyword file))))))

(ert-deftest pos-lint-todo-line/reports-the-line ()
  (pos-test-with-repo '(("life/life-areas.org" . "#+TITLE: x\n#+todo: TODO | DONE\n* TODO a\n"))
    (let ((file (expand-file-name "life/life-areas.org" root)))
      (should (equal (list (list file 2 "#+TODO line overrides the one sequence; run pos-normalise-keywords"))
                     (pos-lint-todo-line file))))))

(ert-deftest pos-lint-merged-copy/names-what-dedupe-left-to-reconcile ()
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\nhinge\n** Merged copy from life/life-areas.org:4\nwelder\n"))
    (should (equal (list (list (expand-file-name "intray.org" root) 3
                               "merged copy awaiting reconciliation with its parent"))
                   (pos-lint-merged-copy (expand-file-name "intray.org" root))))))

(ert-deftest pos-lint-stranded-tasks/finds-keywords-outside-the-agenda ()
  (pos-test-with-repo '(("intray.org" . "* TODO covered\n")
                        ("life/resources/book.org" . "* Chapter\n** TODO write it\n")
                        ("sport/resources/zettles/x.org" . "* DONE old\n")
                        ("archive/orgmode/2026-W35/intray.org_archive" . "* DONE swept\n")
                        ("archive/orgmode/2026-W35/notes.org" . "* TODO ignored\n"))
    (progn
      (should (equal (list (list (expand-file-name "life/resources/book.org" root) 2
                                 "task keyword outside the agenda files (TODO)")
                           (list (expand-file-name "sport/resources/zettles/x.org" root) 1
                                 "task keyword outside the agenda files (DONE)"))
                     (pos-lint-stranded-tasks root))))))

(ert-deftest pos-uncovered-org-files/skips-the-prose-directories ()
  "Prose directories are not scanned."
  (pos-test-with-repo '(("intray.org" . "* TODO covered\n")
                        ("meta/journal/review.org" . "* DONE learned something\n")
                        ("meta/specs/spec.org" . "* TODO a required feature\n")
                        ("meta/tools/notes.org" . "* TODO still a task\n"))
    (progn
      (should (equal '("meta/tools/notes.org")
                     (pos-test-relative (pos-uncovered-org-files root) root))))))

(ert-deftest pos-uncovered-org-files/skips-hidden-directories ()
  "Hidden directories are not walked."
  (pos-test-with-repo '(("intray.org" . "")
                        ("life/resources/book.org" . "")
                        (".venv/lib/site-packages/x.org" . "")
                        (".git/x.org" . ""))
    (progn
      (should (equal '("life/resources/book.org")
                     (pos-test-relative (pos-uncovered-org-files root) root))))))

(ert-deftest pos-lint-duplicate-tasks/reports-each-copy-naming-the-other ()
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\n* Finances\n")
                        ("life/life-areas.org" . "* Finances\n** BACKLOG Fix the gate\n"))
    (progn
      (should (equal (list (list (expand-file-name "intray.org" root) 1
                                 "duplicate task (also life/life-areas.org:2)")
                           (list (expand-file-name "life/life-areas.org" root) 2
                                 "duplicate task (also intray.org:1)"))
                     (pos-lint-duplicate-tasks root))))))

(ert-deftest pos-stranded-open-tasks/lists-the-topmost-open-task-of-a-nest ()
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n")
                        ("life/resources/notes.org" .
                         "* Seminar\n** TODO follow up\n*** TODO nested\n** DONE said hello\n"))
    (progn
      (should (equal (list (list (expand-file-name "life/resources/notes.org" root) 2 "follow up"))
                     (pos-stranded-open-tasks root))))))

(ert-deftest pos-refile-stranded/moves-the-task-under-unsorted-with-a-backreference ()
  (pos-test-with-repo '(("intray.org" . "* Zettles\n* Unsorted\n** TODO already here\n* Sorted\n")
                        ("life/resources/notes.org" .
                         "* Seminar\n** TODO follow up\nSCHEDULED: <2026-09-14 Mon>\nsome notes\n*** TODO nested\n** DONE said hello\n"))
    (let ((pos-directory root))
      (pos-refile-stranded)
      (should (equal "* Seminar\n** DONE said hello\n"
                     (pos-test-file-string (expand-file-name "life/resources/notes.org" root))))
      (should (equal (concat "* Zettles\n* Unsorted\n** TODO already here\n"
                             "** TODO follow up\nSCHEDULED: <2026-09-14 Mon>\n"
                             "From [[file:life/resources/notes.org::*Seminar][life/resources/notes.org: Seminar]]\n"
                             "some notes\n*** TODO nested\n* Sorted\n")
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-refile-stranded/creates-unsorted-when-the-intray-has-none ()
  (pos-test-with-repo '(("intray.org" . "* Sorted\n")
                        ("life/resources/notes.org" . "* TODO top level task\n"))
    (let ((pos-directory root))
      (pos-refile-stranded)
      (should (equal "* Sorted\n* Unsorted\n** TODO top level task\nFrom [[file:life/resources/notes.org][life/resources/notes.org]]\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-refile-stranded/dry-run-changes-nothing-and-lists-the-tasks ()
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n")
                        ("life/resources/notes.org" . "* TODO a task\n"))
    (let ((pos-directory root))
      (should (equal 1 (length (pos-refile-stranded t))))
      (should (equal "* TODO a task\n"
                     (pos-test-file-string (expand-file-name "life/resources/notes.org" root))))
      (should (equal "* Unsorted\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-goto-unsorted-end/stops-before-the-next-top-heading ()
  (with-temp-buffer
    (org-mode)
    (insert "* Unsorted\n** TODO here\n* Sorted\n")
    (pos-goto-unsorted-end)
    (insert "** TODO new\n")
    (should (equal "* Unsorted\n** TODO here\n** TODO new\n* Sorted\n"
                   (buffer-string)))))

(ert-deftest pos-goto-unsorted-end/creates-unsorted-at-the-end ()
  (with-temp-buffer
    (org-mode)
    (insert "* Sorted")
    (pos-goto-unsorted-end)
    (insert "** TODO new\n")
    (should (equal "* Sorted\n* Unsorted\n** TODO new\n" (buffer-string)))))

(ert-deftest pos-dedupe-suggest/keeps-the-pillar-copy-over-the-intray ()
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\n")
                        ("life/life-areas.org" . "* Household\n** TODO Fix the gate\n"))
    (progn
      (should (equal '("drop" "keep")
                     (mapcar #'car (pos-dedupe-suggest (car (pos-dedupe-groups root)) root)))))))

(ert-deftest pos-dedupe-suggest/two-pillar-files-is-a-question ()
  (pos-test-with-repo '(("life/life-areas.org" . "* TODO fix the gate\n")
                        ("people/people-areas.org" . "* TODO fix the gate\n"))
    (progn
      (should (equal '("?" "?")
                     (mapcar #'car (pos-dedupe-suggest (car (pos-dedupe-groups root)) root)))))))

(ert-deftest pos-dedupe-suggest/same-file-keeps-the-longer-body ()
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\n* TODO fix the gate\nhinge is bent\n"))
    (progn
      (should (equal '("drop" "keep")
                     (mapcar #'car (pos-dedupe-suggest (car (pos-dedupe-groups root)) root)))))))

(ert-deftest pos-dedupe-plan/round-trips-through-the-reader ()
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\n")
                        ("life/life-areas.org" . "* Household\n** TODO Fix the gate\n"))
    (let (
          (plan (expand-file-name "dedupe.org" root)))
      (with-temp-file plan (insert (pos-dedupe-plan root)))
      (should (equal '(("fix the gate" ("drop" "intray.org" 1) ("keep" "life/life-areas.org" 2)))
                     (pos-dedupe-read-plan plan)))
      (should (string-match-p "^\\* fix the gate$" (pos-test-file-string plan)))
      (should (string-match-p "| Household " (pos-test-file-string plan))))))

(ert-deftest pos-dedupe-apply/deletes-a-copy-with-nothing-to-say ()
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n** TODO fix the gate\n** TODO other\n")
                        ("life/life-areas.org" . "* Household\n** TODO Fix the gate\nhinge is bent\n")
                        ("dedupe.org" . "* fix the gate\n| act | file | line | under | body lines |\n| drop | intray.org | 2 | Unsorted | 0 |\n| keep | life/life-areas.org | 2 | Household | 1 |\n"))
    (let ((pos-directory root))
      (should (equal '(:resolved 1 :merged 0 :relinked 0 :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root))))
      (should (equal "* Unsorted\n** TODO other\n"
                     (pos-test-file-string (expand-file-name "intray.org" root))))
      (should (equal "* Household\n** TODO Fix the gate\nhinge is bent\n"
                     (pos-test-file-string (expand-file-name "life/life-areas.org" root)))))))

(ert-deftest pos-dedupe-apply/merges-a-differing-body-under-the-survivor ()
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\ncall the welder\n** TODO get a quote\n")
                        ("life/life-areas.org" . "* Household\n** TODO Fix the gate\nhinge is bent\n** TODO next\n")
                        ("dedupe.org" . "* fix the gate\n| drop | intray.org | 1 | | 2 |\n| keep | life/life-areas.org | 2 | Household | 1 |\n"))
    (let ((pos-directory root))
      (should (equal '(:resolved 1 :merged 1 :relinked 0 :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root))))
      (should (equal "" (pos-test-file-string (expand-file-name "intray.org" root))))
      (should (equal (concat "* Household\n** TODO Fix the gate\nhinge is bent\n"
                             "*** Merged copy from intray.org:1\ncall the welder\n**** TODO get a quote\n"
                             "** TODO next\n")
                     (pos-test-file-string (expand-file-name "life/life-areas.org" root)))))))

(ert-deftest pos-dedupe-apply/skips-undecided-groups-and-dry-run-changes-nothing ()
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\n* TODO paint\n")
                        ("life/life-areas.org" . "* TODO fix the gate\n* TODO paint\n")
                        ("dedupe.org" . "* fix the gate\n| ? | intray.org | 1 | | 0 |\n| ? | life/life-areas.org | 1 | | 0 |\n* paint\n| drop | intray.org | 2 | | 0 |\n| keep | life/life-areas.org | 2 | | 0 |\n"))
    (let ((pos-directory root))
      (should (equal '(:resolved 1 :merged 0 :relinked 0 :skipped 1 :vanished 0 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root) t)))
      (should (equal "* TODO fix the gate\n* TODO paint\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-dedupe-apply/saves-every-file-it-touched ()
  "Every touched file is saved."
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\n")
                        ("life/life-areas.org" . "* TODO fix the gate\n")
                        ("people/people-areas.org" . "* TODO paint\n")
                        ("sport/sport-areas.org" . "* TODO paint\n")
                        ("dedupe.org" . "* fix the gate\n| drop | intray.org | 1 | | 0 |\n| keep | life/life-areas.org | 1 | | 0 |\n* paint\n| drop | people/people-areas.org | 1 | | 0 |\n| keep | sport/sport-areas.org | 1 | | 0 |\n"))
    (let ((pos-directory root))
      (should (equal '(:resolved 2 :merged 0 :relinked 0 :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root))))
      (should (equal "" (pos-test-file-string (expand-file-name "intray.org" root))))
      (should (equal "" (pos-test-file-string (expand-file-name "people/people-areas.org" root)))))))

(ert-deftest pos-dedupe-apply/a-copy-nested-in-an-earlier-cut-is-left-alone ()
  "A copy cut with its parent is not cut again from unrelated text."
  (pos-test-with-repo '(("intray.org" . "* TODO shed\n** TODO fix the gate\n* TODO unrelated\n")
                        ("life/life-areas.org" . "* TODO shed\n** TODO fix the gate\n")
                        ("dedupe.org" . "* shed\n| drop | intray.org | 1 | | 1 |\n| keep | life/life-areas.org | 1 | | 1 |\n* fix the gate\n| drop | intray.org | 2 | shed | 0 |\n| keep | life/life-areas.org | 2 | shed | 0 |\n"))
    (let ((pos-directory root))
      (should (equal '(:resolved 2 :merged 0 :relinked 0 :skipped 0 :vanished 1 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root))))
      (should (equal "* TODO unrelated\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-dedupe-apply/points-links-at-the-kept-copy ()
  "Each id: link to a dropped copy goes to the kept copy's ID; the merged
note does not keep the dropped ID; a plan lists the links."
  (pos-test-with-repo '(("intray.org" . ":PROPERTIES:\n:ID: file-i\n:END:\n* TODO fix the gate\n:PROPERTIES:\n:ID: dropped\n:END:\ncall the welder\n")
                        ("life/life-areas.org" . "* Household\n** TODO Fix the gate\n:PROPERTIES:\n:ID: kept\n:END:\nhinge is bent\n")
                        ("notes.org" . ":PROPERTIES:\n:ID: file-n\n:END:\nSee [[id:dropped][the gate]], and [[id:dropped]] again.\n")
                        ("dedupe.org" . "* fix the gate\n| drop | intray.org | 4 | | 1 |\n| keep | life/life-areas.org | 2 | Household | 1 |\n"))
    (let ((pos-directory root))
      (should (string-match-p "^- Links to intray.org:4 from notes.org:4, notes.org:4$"
                              (pos-dedupe-plan root)))
      (should (string-match-p "| dropped *|" (pos-dedupe-plan root)))
      (should (equal '(:resolved 1 :merged 1 :relinked 2 :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root) t)))
      (should (string-match-p "id:dropped" (pos-test-file-string (expand-file-name "notes.org" root))))
      (should (equal '(:resolved 1 :merged 1 :relinked 2 :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root))))
      (should (equal ":PROPERTIES:\n:ID: file-n\n:END:\nSee [[id:kept][the gate]], and [[id:kept]] again.\n"
                     (pos-test-file-string (expand-file-name "notes.org" root))))
      (should (equal (concat "* Household\n** TODO Fix the gate\n:PROPERTIES:\n:ID: kept\n:END:\nhinge is bent\n"
                             "*** Merged copy from intray.org:4\ncall the welder\n")
                     (pos-test-file-string (expand-file-name "life/life-areas.org" root)))))))

(ert-deftest pos-dedupe-apply/a-kept-copy-without-an-id-takes-the-dropped-ones ()
  "The first dropped ID passes to the kept copy, so its links need no
rewriting; a second dropped ID's links are pointed at it."
  (pos-test-with-repo '(("intray.org" . ":PROPERTIES:\n:ID: file-i\n:END:\n* TODO fix the gate\n:PROPERTIES:\n:ID: first\n:END:\n* TODO Fix the gate\n:PROPERTIES:\n:ID: second\n:END:\n")
                        ("life/life-areas.org" . "* Household\n** TODO Fix the gate\n")
                        ("notes.org" . ":PROPERTIES:\n:ID: file-n\n:END:\n[[id:first]] [[id:second]]\n")
                        ("dedupe.org" . "* fix the gate\n| drop | intray.org | 4 | | 0 |\n| drop | intray.org | 8 | | 0 |\n| keep | life/life-areas.org | 2 | Household | 0 |\n"))
    (let ((pos-directory root))
      (should (equal '(:resolved 1 :merged 0 :relinked 1 :skipped 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root))))
      (should (equal ":PROPERTIES:\n:ID: file-n\n:END:\n[[id:first]] [[id:first]]\n"
                     (pos-test-file-string (expand-file-name "notes.org" root))))
      (should (equal "* Household\n** TODO Fix the gate\n:PROPERTIES:\n:ID:       first\n:END:\n"
                     (pos-test-file-string (expand-file-name "life/life-areas.org" root))))
      (should (equal ":PROPERTIES:\n:ID: file-i\n:END:\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-dedupe-apply/a-plan-that-predates-an-edit-is-stale-not-fatal ()
  "A plan older than the file is stale."
  (pos-test-with-repo '(("intray.org" . "* TODO fix the gate\n")
                        ("life/life-areas.org" . "* Household\n** TODO fix the gate\n")
                        ("dedupe.org" . "* fix the gate\n| drop | intray.org | 1 | | 0 |\n| keep | life/life-areas.org | 3 | Household | 0 |\n"))
    (let ((pos-directory root))
      (should (equal '(:resolved 0 :merged 0 :relinked 0 :skipped 0 :vanished 0 :stale 1)
                     (pos-dedupe-apply root (expand-file-name "dedupe.org" root))))
      (should (equal "* TODO fix the gate\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-refile-suggest/first-matching-rule-wins ()
  (progn
    (should (equal "work" (pos-refile-suggest "Client invoice query")))
    (should (equal "body" (pos-refile-suggest "Dentist checkup")))
    (should (null (pos-refile-suggest "Visitor arrives")))))

(ert-deftest pos-refile-plan/one-row-per-intray-entry-with-a-suggestion ()
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n** TODO Dentist checkup\nSCHEDULED: <2026-09-14 Mon>\nbook it\n*** sub\n** Visitor arrives\n* Sorted\n** TODO Invoice query\n"))
    (let ((plan (expand-file-name "refile.org" root)))
      (with-temp-file plan (insert (pos-refile-plan root)))
      (should (equal '(("move" 2 "Dentist checkup" "body/body-projects.org" "")
                       ("?" 6 "Visitor arrives" "" "")
                       ("move" 8 "Invoice query" "work/work-projects.org" ""))
                     (pos-refile-read-plan plan)))
      (let ((text (pos-test-file-string plan)))
        (should (string-match-p "| book it *|" text))
        (should (string-match-p "^\\*\\* 2: Dentist checkup (under Unsorted)\n#\\+begin_example\n,\\*\\* TODO Dentist checkup\n" text))
        (should (string-match-p ",\\*\\*\\* sub" text))
        (should (null (pos-lint-duplicate-tasks root)))))))

(ert-deftest pos-refile-apply/moves-under-the-named-heading-or-to-the-end ()
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n** TODO Dentist checkup\nbody\n*** sub\n** TODO Invoice query\n** keep me\n")
                        ("body/body-projects.org" . "* Appointments\n** existing\n* Other\n")
                        ("work/work-projects.org" . "* Admin\n")
                        ("refile.org" . "| move | 2 | Dentist checkup | body/body-projects.org | Appointments |\n| move | 5 | Invoice query | work/work-projects.org | |\n| skip | 6 | keep me | | |\n"))
    (let ((pos-directory root))
      (should (equal '(:moved 2 :left 1 :missing 0 :vanished 0)
                     (pos-refile-apply root (expand-file-name "refile.org" root))))
      (should (equal "* Unsorted\n** keep me\n"
                     (pos-test-file-string (expand-file-name "intray.org" root))))
      (should (equal "* Appointments\n** existing\n** TODO Dentist checkup\nbody\n*** sub\n* Other\n"
                     (pos-test-file-string (expand-file-name "body/body-projects.org" root))))
      (should (equal "* Admin\n* TODO Invoice query\n"
                     (pos-test-file-string (expand-file-name "work/work-projects.org" root)))))))

(ert-deftest pos-refile-apply/an-outline-path-files-below-a-nested-heading ()
  "An outline path targets the nested heading, not a namesake."
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n** TODO Invoicing\n")
                        ("work/work-areas.org" . "* Operations\n** Housekeeping\n*** existing\n** Delivery\n* Housekeeping\n")
                        ("refile.org" . "| move | 2 | Invoicing | work/work-areas.org | Operations/Housekeeping |\n"))
    (let ((pos-directory root))
      (should (equal '(:moved 1 :left 0 :missing 0 :vanished 0)
                     (pos-refile-apply root (expand-file-name "refile.org" root))))
      (should (equal "* Operations\n** Housekeeping\n*** existing\n*** TODO Invoicing\n** Delivery\n* Housekeeping\n"
                     (pos-test-file-string (expand-file-name "work/work-areas.org" root)))))))

(ert-deftest pos-refile-apply/missing-targets-and-moved-lines-are-counted-not-fatal ()
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n** TODO a\n** TODO b\n** TODO c\n")
                        ("body/body-projects.org" . "* Appointments\n")
                        ("refile.org" . "| move | 2 | a | nowhere/nowhere-projects.org | |\n| move | 3 | b | body/body-projects.org | Surgery |\n| move | 4 | zzz | body/body-projects.org | |\n"))
    (let ((pos-directory root))
      (should (equal '(:moved 0 :left 0 :missing 2 :vanished 1)
                     (pos-refile-apply root (expand-file-name "refile.org" root))))
      (should (equal "* Unsorted\n** TODO a\n** TODO b\n** TODO c\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(ert-deftest pos-refile-apply/dry-run-changes-nothing ()
  (pos-test-with-repo '(("intray.org" . "* Unsorted\n** TODO a\n")
                        ("body/body-projects.org" . "* Appointments\n")
                        ("refile.org" . "| move | 2 | a | body/body-projects.org | Appointments |\n"))
    (let ((pos-directory root))
      (should (equal '(:moved 1 :left 0 :missing 0 :vanished 0)
                     (pos-refile-apply root (expand-file-name "refile.org" root) t)))
      (should (equal "* Unsorted\n** TODO a\n"
                     (pos-test-file-string (expand-file-name "intray.org" root)))))))

(provide 'pos-test)
;;; pos-test.el ends here
