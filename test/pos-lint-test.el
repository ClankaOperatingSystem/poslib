;;; pos-lint-test.el --- Tests for pos-lint.el  -*- lexical-binding: t -*-

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

;; The specification of pos-lint.el, as tests.  Each test is named by
;; the rule it pins, as a sentence.
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
;; The rules are tested through files in a temporary root.  Run: make
;; test.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'pos)
(require 'pos-lint)
(require 'pos-test-support)

;;;; Lint

(ert-deftest pos-lint/a-done-entry-with-an-open-child-is-a-finding ()
  "Lint reports a done entry that has an open task below it.
It is the shape the sweep skips.  A finding is (FILE LINE MESSAGE),
the line being the done entry's; a done entry with nothing open below
is not reported."
  (pos-test-with-files root
      '(("intray.org" . "* DONE parent\n** TODO child\n* DONE fine\n"))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (pos-finding-make :file file :line 1 :message "done entry has open children"))
                     (pos-lint-done-with-open-children file))))))

(ert-deftest pos-lint/a-clean-tree-lints-silent ()
  "A tree with nothing to report lints to no findings and empty text.
Open and done tasks in the right places are the normal state."
  (pos-test-with-files root '(("intray.org" . "* TODO open\n* DONE fine\n"))
    (let ((pos-directory root))
      (should-not (pos-lint))
      (should (equal "" (pos-lint-format nil root))))))

(ert-deftest pos-lint/a-methodologys-checks-are-run-by-the-lint ()
  "Each executable scripts/check-* of a mounted methodology's skills is run.
A FILE:LINE: MESSAGE line is a finding at that file, relative to the
project; another line is a finding at the project; a check that exits
2 is a finding at its skill with its standard error.  A file in
scripts/lib/, and a check-* that is not executable, are not run."
  (pos-test-with-files root
      '((".clanka/config.yml"
         . "pos: 2\nmethodologies: methodologies\nchildren:\n  - path: methodologies/adr\n")
        ("intray.org" . "* TODO open\n")
        ("methodologies/adr/skills/adr-write/SKILL.md"
         . "---\nname: adr-write\ndescription: Write a decision.\n---\n")
        ("methodologies/adr/skills/adr-write/scripts/check-status"
         . "#!/bin/sh\n. \"$(dirname \"$0\")/lib/say\"\nsay 'decisions/0002.md:3: status not known'\necho 'index out of date'\nexit 1\n")
        ("methodologies/adr/skills/adr-write/scripts/lib/say"
         . "say() { echo \"$1\"; }\n")
        ("methodologies/adr/skills/adr-write/scripts/check-idle"
         . "#!/bin/sh\necho 'never printed'\nexit 1\n")
        ("methodologies/adr/skills/adr-write/scripts/check-broken"
         . "#!/bin/sh\necho 'no decisions directory' >&2\nexit 2\n"))
    (dolist (name '("check-status" "check-broken"))
      (set-file-modes (expand-file-name
                       (concat "methodologies/adr/skills/adr-write/scripts/" name) root)
                      #o755))
    (let ((pos-directory root))
      (should (equal (pos-test-lines
                      ".:1: index out of date"
                      "decisions/0002.md:3: status not known"
                      (concat "methodologies/adr/skills/adr-write:1: "
                              "check check-broken could not check: no decisions directory"))
                     (pos-lint-format (pos-lint) root))))))

(ert-deftest pos-lint/checks-a-declaration-names-are-reported-and-not-run ()
  "A methodology.yaml that names checks is a finding at the methodology.
The commands it names in bin/ are not run."
  (pos-test-with-files root
      '((".clanka/config.yml"
         . "pos: 2\nmethodologies: methodologies\nchildren:\n  - path: methodologies/adr\n")
        ("intray.org" . "* TODO open\n")
        ("methodologies/adr/methodology.yaml" . "methodology: 1\nchecks:\n  - adr-check\n")
        ("methodologies/adr/bin/adr-check" . "#!/bin/sh\necho 'ran'\nexit 1\n"))
    (set-file-modes (expand-file-name "methodologies/adr/bin/adr-check" root) #o755)
    (let ((pos-directory root))
      (should (equal (pos-test-lines
                      (concat "methodologies/adr:1: checks in bin/ are not run; move each"
                              " to a skill's scripts/check-NAME: adr-check"))
                     (pos-lint-format (pos-lint) root))))))

(ert-deftest pos-lint/the-checks-of-a-skill-installed-from-a-source-are-run ()
  "The checks of a skill installed from a declared source are run.
The skill is in .agents/skills/ with a lock entry of the source.  A
skill of the repository's own, with no lock entry, has none run."
  (pos-test-with-files root
      '((".clanka/config.yml"
         . "pos: 2\nmethodologies:\n  - source: https://example.org/adr.git\n    ref: v1\n")
        ("intray.org" . "* TODO open\n")
        ("skills-lock.json"
         . "{\"version\": 1, \"skills\": {\"adr-write\": {\"source\": \"https://example.org/adr.git\", \"sourceUrl\": \"https://example.org/adr.git\", \"ref\": \"v1\", \"sourceType\": \"git\", \"computedHash\": \"0\"}}}\n")
        (".agents/skills/adr-write/SKILL.md"
         . "---\nname: adr-write\ndescription: Write a decision.\n---\n")
        (".agents/skills/adr-write/scripts/check-status"
         . "#!/bin/sh\necho 'decisions/0002.md:3: status not known'\nexit 1\n")
        (".agents/skills/own/SKILL.md" . "---\nname: own\ndescription: Own.\n---\n")
        (".agents/skills/own/scripts/check-own" . "#!/bin/sh\necho 'ran'\nexit 1\n"))
    (dolist (path '(".agents/skills/adr-write/scripts/check-status"
                    ".agents/skills/own/scripts/check-own"))
      (set-file-modes (expand-file-name path root) #o755))
    (let ((pos-directory root))
      (should (equal (pos-test-lines "decisions/0002.md:3: status not known")
                     (pos-lint-format (pos-lint) root))))))

(ert-deftest pos-lint/an-undeclared-entry-of-a-lock-file-is-a-finding ()
  "An entry of skills-lock.json of a skill its source does not name is found.
It is found at its line.  An entry of no declared source is
wild-caught, and not found.  A source declared and not installed is
the image's refresh to report, and the lint says nothing of it."
  (pos-test-with-files root
      '((".clanka/config.yml"
         . "pos: 2\nmethodologies:\n  - source: https://example.org/a.git\n    ref: v1\n    skills: [a-one]\n")
        ("intray.org" . "* TODO open\n")
        ("skills-lock.json"
         . "{\n  \"version\": 1,\n  \"skills\": {\n    \"a-two\": {\n      \"source\": \"https://example.org/a.git\",\n      \"ref\": \"v1\",\n      \"sourceType\": \"git\",\n      \"computedHash\": \"0\"\n    },\n    \"grill-me\": {\n      \"source\": \"someone/skills\",\n      \"sourceType\": \"github\",\n      \"computedHash\": \"0\"\n    }\n  }\n}\n"))
    (let ((pos-directory root))
      (should (equal (pos-test-lines
                      (concat "skills-lock.json:4: lock-undeclared: a-two: "
                              "not among the skills of https://example.org/a.git"))
                     (pos-lint-format (pos-lint) root))))))

(ert-deftest pos-lint/an-entry-of-the-images-skills-is-declared ()
  "An entry of the image's repository in a root's skills-lock.json is declared.
auto/ in the configuration directory names the repository and the
commit the image was built from, and holds the skill.  The checks of
the installed skill are run."
  (pos-test-with-files root
      '((".clanka/config.yml" . "pos: 2\n")
        ("intray.org" . "* TODO open\n")
        (".clanka/auto/version" . "1\n")
        (".clanka/auto/repository" . "https://example.org/clankos.git\n")
        (".clanka/auto/commit" . "1111111111111111111111111111111111111111\n")
        (".clanka/auto/skills/clankos-seal/SKILL.md"
         . "---\nname: clankos-seal\ndescription: Seal.\n---\n")
        ("skills-lock.json"
         . "{\"version\": 1, \"skills\": {\"clankos-seal\": {\"source\": \"https://example.org/clankos.git\", \"sourceUrl\": \"https://example.org/clankos.git\", \"ref\": \"1111111111111111111111111111111111111111\", \"sourceType\": \"git\", \"computedHash\": \"0\"}}}\n")
        (".agents/skills/clankos-seal/SKILL.md"
         . "---\nname: clankos-seal\ndescription: Seal.\n---\n")
        (".agents/skills/clankos-seal/scripts/check-seal"
         . "#!/bin/sh\necho 'intray.org:1: not sealed'\nexit 1\n"))
    (set-file-modes (expand-file-name ".agents/skills/clankos-seal/scripts/check-seal" root)
                    #o755)
    (let ((pos-directory root))
      (should (equal (pos-test-lines "intray.org:1: not sealed")
                     (pos-lint-format (pos-lint) root))))))

(ert-deftest pos-lint/findings-print-file-line-message-relative-to-the-root ()
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

(ert-deftest pos-lint/a-heading-beginning-with-a-retired-keyword-is-a-finding ()
  "A heading beginning with a retired keyword is plain text, and reported.
CANCELED, CLARIFY and BACKLOG were once in the sequence; Org now reads
them as the first word of a title, so the task is open and invisible.
A word never in any sequence, like LATER, is just a word."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-test-lines "* CANCELED old"
                                         "* CLARIFY me"
                                         "* LATER timesheets"
                                         "* TODO fine"
                                         "* BACKLOG mend the fence")))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (pos-finding-make :file file :line 1 :message
                                 "stale keyword CANCELED (not in the sequence)")
                           (pos-finding-make :file file :line 2 :message
                                 "stale keyword CLARIFY (not in the sequence)")
                           (pos-finding-make :file file :line 5 :message
                                 "stale keyword BACKLOG (not in the sequence)"))
                     (pos-lint-stale-keyword file))))))

(ert-deftest pos-lint/an-intray-item-that-is-not-todo-is-a-finding ()
  "An item under Unsorted in an intray.org that is not TODO is reported.
NEXT, WAITING and SOMEDAY items have been clarified; DONE and
CANCELLED ones are finished.  A TODO item is what an intray holds.  A
heading with no state, an item under another heading of the intray,
and an item of a file not named intray.org are not reported."
  (pos-test-with-files root
      `(("intray.org" . ,(pos-test-lines "* Unsorted"
                                         "** TODO fine"
                                         "** NEXT call"
                                         "** SOMEDAY weld"
                                         "** DONE paid"
                                         "** CANCELLED dropped"
                                         "** A note"
                                         "* Sorted"
                                         "** NEXT elsewhere"))
        ("tasks.org" . "* Unsorted\n** NEXT not an intray\n"))
    (let ((file (expand-file-name "intray.org" root)))
      (should (equal (list (pos-finding-make :file file :line 3 :message "NEXT item in the intray: clarified, to be placed")
                           (pos-finding-make :file file :line 4 :message "SOMEDAY item in the intray: clarified, to be placed")
                           (pos-finding-make :file file :line 5 :message "DONE item in the intray: finished, to be retired")
                           (pos-finding-make :file file :line 6 :message "CANCELLED item in the intray: finished, to be retired"))
                     (pos-lint-clarified-in-intray file)))
      (should-not (pos-lint-clarified-in-intray
                   (expand-file-name "tasks.org" root))))))

(ert-deftest pos-lint/a-waiting-item-that-does-not-say-who-or-when-is-a-finding ()
  "A WAITING item says who or what it waits on, and since when.
Who or what is a WAITING_ON or DELEGATED_TO property or the item's
own text.  Since
when is the record of its change to WAITING or a date in its own
text.  Here bare says neither; roofer says who and not when; logged
has the record and no text; smith and mason have a property and the
record; letter says both in its text.  A child's text is not the item's."
  (pos-test-with-files root
      `(("tasks.org"
         . ,(pos-test-lines
             "* WAITING bare"
             "** TODO a child"
             "On the roofer since 2026-10-01."
             "* WAITING roofer"
             "On the roofer to ring back."
             "* WAITING logged"
             ":LOGBOOK:"
             "- State \"WAITING\"    from \"TODO\"       [2026-10-01 Thu 09:00]"
             ":END:"
             "* WAITING smith"
             ":PROPERTIES:"
             ":DELEGATED_TO: smith"
             ":END:"
             ":LOGBOOK:"
             "- State \"WAITING\"    from \"NEXT\"       [2026-10-02 Fri 09:00]"
             ":END:"
             "* WAITING mason"
             ":PROPERTIES:"
             ":WAITING_ON: [[id:a-mason][Mason@yard]]"
             ":END:"
             ":LOGBOOK:"
             "- State \"WAITING\"    from \"NEXT\"       [2026-10-02 Fri 09:00]"
             ":END:"
             "* WAITING letter"
             "Asked the council on [2026-10-03 Sat]."
             "* NEXT not waiting")))
    (let ((file (expand-file-name "tasks.org" root)))
      (should (equal (list (pos-finding-make :file file :line 1 :message (concat "WAITING item does not say who or"
                                                " what it waits on, or since when"))
                           (pos-finding-make :file file :line 4 :message "WAITING item does not say since when")
                           (pos-finding-make :file file :line 6 :message (concat "WAITING item does not say who or"
                                                " what it waits on")))
                     (pos-lint-waiting-without-who-or-when file))))))

(ert-deftest pos-lint/a-todo-line-in-a-file-is-a-finding ()
  "A #+TODO line in a file of the corpus is reported, with the fix to run.
It overrides the one sequence for that file; `pos-normalise-keywords'
removes it.  The match is case-insensitive, as Org's is."
  (pos-test-with-files root
      '(("life/life-areas.org" . "#+TITLE: x\n#+todo: TODO | DONE\n* TODO a\n"))
    (let ((file (expand-file-name "life/life-areas.org" root))
          (text (concat "#+TODO line overrides the one sequence;"
                        " run pos-normalise-keywords")))
      (should (equal (list (pos-finding-make :file file :line 2 :message text))
                     (pos-lint-todo-line file))))))

(ert-deftest pos-lint/a-todo-line-inside-a-block-is-text-and-is-left ()
  "A #+TODO line inside an example or a source block sets no keywords.
It is text the document shows.  Lint does not report it and
normalising does not remove it; the line outside the blocks is
reported and removed."
  (let ((text (pos-test-lines "#+TITLE: How keywords work"
                              "#+begin_example"
                              "#+TODO: TODO(t) | DONE(d!)"
                              "#+end_example"
                              "#+begin_src org"
                              "#+SEQ_TODO: OPEN | SHUT"
                              "#+end_src"
                              "#+TODO: BACKLOG | DONE"
                              "* TODO a")))
    (pos-test-with-files root `(("notes.org" . ,text))
      (let ((file (expand-file-name "notes.org" root)))
        (should (equal '(8) (mapcar #'pos-finding-line (pos-lint-todo-line file))))
        (should (equal '(:lines-removed 1 :respelled 0)
                       (pos-normalise-keywords-in-file file)))
        (should (equal (replace-regexp-in-string
                        "#\\+TODO: BACKLOG | DONE\n" "" text)
                       (pos-test-text root "notes.org")))))))

(ert-deftest pos-lint/a-product-s-own-keyword-line-stands-and-is-no-finding ()
  "A #+TODO line in a file the root may not write is that file's own.
Lint reports the line in the root's own file and not in the
product's, which `pos-normalise-keywords' could not change.  The
product's file is read with its own keywords: SPIKE is a state
there, and in the root's file, where the one sequence holds, it is
the first word of a title."
  (pos-test-with-files root
      '((".pos/config.yaml" . "pos: 2\nchildren:\n  - path: kit\n")
        ("notes.org" . "#+TODO: SPIKE | SHIPPED\n* SPIKE try it here\n")
        ("kit/todo.org" . "#+TODO: SPIKE | SHIPPED\n* SPIKE try it there\n"))
    (let ((pos-directory root))
      (should (equal (concat "notes.org:1: #+TODO line overrides the one sequence;"
                             " run pos-normalise-keywords\n")
                     (pos-lint-format
                      (seq-filter (lambda (finding)
                                    (string-prefix-p "#+TODO" (pos-finding-message finding)))
                                  (pos-lint))
                      root))))
    (let ((pos-visit-corpus (pos-corpus root)))
      (with-current-buffer (pos-visit (expand-file-name "kit/todo.org" root))
        (goto-char (point-max))
        (should (equal "SPIKE" (org-get-todo-state)))))))

(ert-deftest pos-lint/a-merged-copy-left-by-dedupe-is-a-finding ()
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
      (should (equal (list (pos-finding-make :file file :line 3 :message
                                 (concat "merged copy awaiting reconciliation"
                                         " with its parent")))
                     (pos-lint-merged-copy file))))))

(ert-deftest pos-lint/copies-of-one-task-in-two-files-name-each-other ()
  "Copies of one task heading in two corpus files each report the other.
Headings compare lower-cased and trimmed, so Fix the gate and fix the
gate are one task.  A copy nested below the top level is seen like
any other.  A plain heading, Finances, is no task and is not compared."
  (pos-test-with-files root
      '(("intray.org" . "* TODO fix the gate\n* Finances\n")
        ("life/home/areas.org" . "* Finances\n** NEXT Fix the gate\n"))
    (should (equal (list (pos-finding-make
                          :file (expand-file-name "intray.org" root) :line 1
                          :message "duplicate task (also life/home/areas.org:2)")
                         (pos-finding-make
                          :file (expand-file-name "life/home/areas.org" root) :line 2
                          :message "duplicate task (also intray.org:1)"))
                   (pos-lint-duplicate-tasks root)))))

(provide 'pos-lint-test)
;;; pos-lint-test.el ends here
