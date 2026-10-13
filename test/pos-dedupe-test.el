;;; pos-dedupe-test.el --- Tests for pos-dedupe.el  -*- lexical-binding: t -*-

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

;; The specification of pos-dedupe.el, as tests.  Each test is named
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
;; Pure rules, such as dedupe suggestions, are tested on values alone;
;; the rest through files in a temporary root.  Run: make test.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'pos)
(require 'pos-dedupe)
(require 'pos-test-support)

;;;; Dedupe

(defun pos-dedupe-test-copy (file line &optional body parent)
  "Return the `pos--task-entries' entry for a copy of \"Fix the gate\".
The copy is in FILE under /repo/, at LINE, with BODY, default empty,
under PARENT, default none, and has no ID."
  (list :key "fix the gate" :heading "Fix the gate"
        :file (expand-file-name file "/repo/") :line line
        :parent parent :body (or body "") :id nil))

(ert-deftest pos-dedupe/the-intray-copy-is-dropped-for-the-pillar-copy ()
  "Of a copy in the intray and one in a pillar, the pillar's is kept.
The intray is where a task waits to be filed; a copy already in its
pillar is the task filed.  The suggestion pairs an action with each
entry, in the group's order, whichever way round the group comes."
  (let ((intray (pos-dedupe-test-copy "intray.org" 1))
        (pillar (pos-dedupe-test-copy "life/life-areas.org" 2
                               "hinge is bent" "Household")))
    (should (equal (list (cons "drop" intray) (cons "keep" pillar))
                   (pos-dedupe-suggest (list intray pillar) "/repo/")))
    (should (equal '("keep" "drop")
                   (mapcar #'car (pos-dedupe-suggest (list pillar intray)
                                                     "/repo/"))))))

(ert-deftest pos-dedupe/two-pillar-copies-are-a-question ()
  "Copies in two pillar files are both marked ?, for a person to decide.
Neither pillar outranks the other.  An intray copy in the same group
does not settle it, and is a question too."
  (let ((life (pos-dedupe-test-copy "life/life-areas.org" 2))
        (people (pos-dedupe-test-copy "people/people-areas.org" 5))
        (intray (pos-dedupe-test-copy "intray.org" 1)))
    (should (equal '("?" "?")
                   (mapcar #'car (pos-dedupe-suggest (list life people)
                                                     "/repo/"))))
    (should (equal '("?" "?" "?")
                   (mapcar #'car (pos-dedupe-suggest (list intray life people)
                                                     "/repo/"))))))

(ert-deftest pos-dedupe/in-one-file-the-longer-body-is-kept ()
  "Of two copies in one file, the one with the longer body is kept.
More said is more to lose; the other is dropped and, if its body
differs, merged under the kept one when the plan is applied.  Bodies
of equal length keep the first."
  (let ((bare (pos-dedupe-test-copy "intray.org" 1))
        (said (pos-dedupe-test-copy "intray.org" 2 "hinge is bent"))
        (later (pos-dedupe-test-copy "intray.org" 3)))
    (should (equal '("drop" "keep")
                   (mapcar #'car (pos-dedupe-suggest (list bare said)
                                                     "/repo/"))))
    (should (equal '("keep" "drop")
                   (mapcar #'car (pos-dedupe-suggest (list said bare)
                                                     "/repo/"))))
    (should (equal '("keep" "drop")
                   (mapcar #'car (pos-dedupe-suggest (list bare later)
                                                     "/repo/"))))))

(ert-deftest pos-dedupe/a-dedupe-plan-round-trips-through-its-reader ()
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

(ert-deftest pos-dedupe/a-dropped-copy-with-nothing-to-say-is-cut ()
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "* Unsorted\n** TODO other\n"
                     (pos-test-text root "intray.org")))
      (should (equal "* Household\n** TODO Fix the gate\nhinge is bent\n"
                     (pos-test-text root "life/life-areas.org"))))))

(ert-deftest pos-dedupe/a-dropped-body-that-differs-merges-under-the-kept-copy ()
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 0)
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

(ert-deftest pos-dedupe/an-undecided-group-is-skipped-and-a-dry-run-cuts-nothing ()
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
                           :skipped 1 :unwritable 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan t)))
      (should (equal "* TODO fix the gate\n* TODO paint\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos-dedupe/applying-a-plan-saves-every-file-it-touched ()
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "" (pos-test-text root "intray.org")))
      (should (equal "" (pos-test-text root "people/people-areas.org"))))))

(ert-deftest pos-dedupe/a-copy-cut-with-its-parent-is-counted-vanished ()
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
                           :skipped 0 :unwritable 0 :vanished 1 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "* TODO unrelated\n" (pos-test-text root "intray.org"))))))

(ert-deftest pos-dedupe/links-to-a-dropped-copy-point-at-the-kept-one ()
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
                          :skipped 0 :unwritable 0 :vanished 0 :stale 0)))
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

(ert-deftest pos-dedupe/a-kept-copy-without-an-id-takes-the-first-dropped-one-s ()
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 0)
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

(ert-deftest pos-dedupe/a-plan-older-than-the-file-is-stale-not-fatal ()
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 1)
                     (pos-dedupe-apply root plan)))
      (should (equal "* TODO fix the gate\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos-dedupe/a-group-linked-from-a-child-repository-is-unwritable ()
  "A group whose dropped ID is linked from a child repository is left.
Both copies are the root's own, but a file of a configured child
repository links to the dropped copy's ID and the kept copy has its
own.  The root may not rewrite that link, and cutting the copy would
leave it pointing at nothing, so neither copy is cut, no file is
changed and the group is counted unwritable."
  (let ((intray (pos-test-lines "* TODO fix the gate"
                                ":PROPERTIES:"
                                ":ID: dropped"
                                ":END:"))
        (areas (pos-test-lines "* Household"
                               "** TODO Fix the gate"
                               ":PROPERTIES:"
                               ":ID: kept"
                               ":END:"))
        (notes (pos-test-lines ":PROPERTIES:"
                               ":ID: file-c"
                               ":END:"
                               "[[id:dropped]]")))
    (pos-test-with-files root
        `(("intray.org" . ,intray)
          ("life/life-areas.org" . ,areas)
          (".pos/config.yaml" . ,pos-test-root-config)
          ("child/.git/HEAD" . ,pos-test-git-head)
          ("child/.clanka/config.yml" . ,pos-test-child-config)
          ("child/notes.org" . ,notes)
          ("dedupe.org"
           . ,(pos-test-lines "* fix the gate"
                              "| drop | intray.org | 1 | | 0 |"
                              "| keep | life/life-areas.org | 2 | Household | 0 |")))
      (let ((plan (expand-file-name "dedupe.org" root)))
        (should (equal (list :resolved 0 :merged 0 :relinked 0
                             :skipped 0 :unwritable 1 :vanished 0 :stale 0)
                       (pos-dedupe-apply root plan)))
        (should (equal intray (pos-test-text root "intray.org")))
        (should (equal areas (pos-test-text root "life/life-areas.org")))
        (should (equal notes (pos-test-text root "child/notes.org")))))))

(ert-deftest pos-dedupe/a-group-with-a-copy-in-a-child-repository-is-unwritable ()
  "A group with a copy in a configured child repository is left as it is.
The child's files are read, so the copy is a duplicate the plan lists;
but the root may not write them, so neither copy is cut and the group
is counted unwritable rather than resolved."
  (pos-test-with-files root
      `(("intray.org" . "* TODO fix the gate\n")
        (".pos/config.yaml" . ,pos-test-root-config)
        ("child/.git/HEAD" . ,pos-test-git-head)
        ("child/.clanka/config.yml" . ,pos-test-child-config)
        ("child/intray.org" . "* TODO fix the gate\n")
        ("dedupe.org"
         . ,(pos-test-lines "* fix the gate"
                            "| drop | intray.org | 1 | | 0 |"
                            "| keep | child/intray.org | 1 | | 0 |")))
    (let ((plan (expand-file-name "dedupe.org" root)))
      (should (string-match-p "| child/intray.org " (pos-dedupe-plan root)))
      (should (equal (list :resolved 0 :merged 0 :relinked 0
                           :skipped 0 :unwritable 1 :vanished 0 :stale 0)
                     (pos-dedupe-apply root plan)))
      (should (equal "* TODO fix the gate\n" (pos-test-text root "intray.org")))
      (should (equal "* TODO fix the gate\n"
                     (pos-test-text root "child/intray.org"))))))

(provide 'pos-dedupe-test)
;;; pos-dedupe-test.el ends here
