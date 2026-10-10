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
;; work on.  The "corpus" is the Org files beneath the root that its
;; configurations allow, at any depth, as pos-corpus.el walks them:
;; the files every command reads.  The commands that write, the sweep
;; and the fixers, write only the corpus files of the root's own
;; repository, never those of a repository mounted within it.  The
;; "uncovered" files are the Org files beneath the root the corpus
;; does not read: in an attic or archives directory, a lock file, a
;; product's.  The "intray" is the root's intray.org, where new and
;; rescued tasks wait to be filed.  The "sweep" archives the DONE and
;; CANCELLED entries of the writable files into the week's archive; it
;; is pos-sweep.el's, specified in pos-sweep-test.el.  A "stranded"
;; task is a task keyword in an uncovered file, which no sweep will
;; ever reach.
;;
;; Pure rules, such as dedupe suggestions and excerpts, are tested on
;; values alone; the rest through files in a temporary root.  Run:
;; make test.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'pos)
(require 'pos-test-support)

;;;; Fixtures

(defmacro pos-test-configured (&rest body)
  "Evaluate BODY with the configuration of test/pos-config.el in force.
Prose directories under meta; a refile rule each into a work and a
body projects file."
  (declare (indent 0))
  `(let ((pos-prose-directories '("meta/journal" "meta/specs"))
         (pos-refile-rules '(("invoice\\|client" . "work/work-projects.org")
                             ("dentist\\|checkup" . "body/body-projects.org"))))
     ,@body))

(defconst pos-test-git-head "ref: refs/heads/master\n"
  "The HEAD of a repository; a directory holding one in .git is a repository.")

(defconst pos-test-child-config "pos: 2\nprojects: projects/\n"
  "The configuration of a child repository: a node of the root's tree.")

(defconst pos-test-root-config
  "pos: 2\nchildren:\n  - path: child\n    remote: git@example.org:child.git\n"
  "The configuration of a root that declares the child repository.")

(defun pos-test-lines (&rest lines)
  "Return LINES as the text of a file: each ended by a newline."
  (mapconcat (lambda (line) (concat line "\n")) lines ""))

(defun pos-test-text (root file)
  "Return the text of FILE, named relative to ROOT."
  (pos-test-file-string (expand-file-name file root)))

;;;; Files and paths

(ert-deftest pos/every-org-file-beneath-the-root-is-read-and-sorted ()
  "The files are every Org file beneath the root, at any depth, sorted.
A book in a resources directory and a zettel deeper still are read
with the root's own top-level files; a text file is not.  The list
is sorted by path, so every command visits the tree in one order."
  (pos-test-with-files root
      '(("todo.org" . "") ("alpha.org" . "") ("notes.txt" . "")
        ("life/life-areas.org" . "") ("life/resources/book.org" . "")
        ("sport/resources/zettles/a.org" . ""))
    (should (equal '("alpha.org" "life/life-areas.org"
                     "life/resources/book.org"
                     "sport/resources/zettles/a.org" "todo.org")
                   (mapcar (lambda (file) (file-relative-name file root))
                           (pos-files root))))))

(ert-deftest pos/a-lock-hidden-archive-or-text-file-is-not-read ()
  "A lock file, a hidden file, an archive and a text file are not read.
The lock file of an Org file open in Emacs, .#intray.org, is a
dangling symbolic link, and no link is read, dangling or not; a
hidden .draft.org and an .org_archive are not Org files of the
corpus.  Nor is the walk taken into an archives, attic, node_modules,
underscored or hidden directory, at any depth."
  (pos-test-with-files root
      '(("intray.org" . "") (".draft.org" . "") ("intray.org_archive" . "")
        ("notes.txt" . "") ("archives/old.org" . "") ("attic/old.org" . "")
        ("_tmp/a.org" . "") ("node_modules/m/z.org" . "")
        ("life/.hidden/h.org" . "") ("life/archives/old.org" . ""))
    (make-symbolic-link "someone@somewhere.1234"
                        (expand-file-name ".#intray.org" root))
    (make-symbolic-link "intray.org" (expand-file-name "linked.org" root))
    (should (equal '("intray.org") (pos-test-relative (pos-files root) root)))))

(ert-deftest pos/a-repository-within-the-tree-is-a-product-and-not-read ()
  "A repository within the tree with no configuration is a product.
Its Org files are nobody's tasks: a library's README is neither read
nor written."
  (pos-test-with-files root
      `(("intray.org" . "")
        ("vendor/lib/.git/HEAD" . ,pos-test-git-head)
        ("vendor/lib/README.org" . ""))
    (should (equal '("intray.org") (pos-test-relative (pos-files root) root)))
    (should (equal '("intray.org")
                   (pos-test-relative (pos-files root t) root)))))

(ert-deftest pos/a-configured-child-repository-is-read-and-not-written ()
  "A repository within the tree with a configuration is read, not written.
It is a node of the tree, so its tasks are seen by lint and the
plans; but its own configuration governs its files, so the sweep and
the fixers, which ask for the writable files, leave it out."
  (pos-test-with-files root
      `(("intray.org" . "")
        (".pos/config.yaml" . ,pos-test-root-config)
        ("child/.git/HEAD" . ,pos-test-git-head)
        ("child/.clanka/config.yml" . ,pos-test-child-config)
        ("child/intray.org" . "") ("child/projects/p.org" . ""))
    (should (equal '("child/intray.org" "child/projects/p.org" "intray.org")
                   (pos-test-relative (pos-files root) root)))
    (should (equal '("intray.org")
                   (pos-test-relative (pos-files root t) root)))))

(ert-deftest pos/uncovered-files-are-those-the-corpus-does-not-read ()
  "The uncovered files are the Org files under the root the corpus leaves out.
A file in an archives, attic, underscored or node_modules directory,
and a file of a product repository: the places a task keyword would
never be swept from.  A file nested below the root is read now, and
is not uncovered; an archive file is not an Org file; a lock file,
the dangling link Emacs leaves beside a file being edited, is passed
over, since visiting it would wait on a question."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . "") ("life/life-areas.org" . "")
          ("life/resources/book.org" . "")
          ("life/resources/book.org_archive" . "")
          ("archives/old.org" . "") ("attic/notes.org" . "")
          ("_scratch/draft.org" . "") ("node_modules/m/z.org" . "")
          ("vendor/lib/.git/HEAD" . ,pos-test-git-head)
          ("vendor/lib/README.org" . ""))
      (make-symbolic-link "someone@somewhere.1234"
                          (expand-file-name ".#intray.org" root))
      (should (equal '("_scratch/draft.org" "archives/old.org"
                       "attic/notes.org" "node_modules/m/z.org"
                       "vendor/lib/README.org")
                     (pos-test-relative (pos-uncovered-org-files root)
                                        root))))))

(ert-deftest pos/the-archive-and-prose-directories-are-never-uncovered ()
  "Files in the archive directory and `pos-prose-directories' are left out.
An archive holds swept tasks, which keep their keywords; prose, such
as a journal, may use a keyword as a word.  Neither is a place a task
is stranded, so a file there that the corpus does not read, one in an
attic, is not uncovered either.  A prose directory's sibling is not
excluded with it."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "")
          ("archive/orgmode/2026-W35/intray.org_archive" . "")
          ("archive/orgmode/attic/notes.org" . "")
          ("meta/journal/attic/review.org" . "")
          ("meta/specs/attic/spec.org" . "")
          ("meta/tools/attic/notes.org" . ""))
      (should (equal '("meta/tools/attic/notes.org")
                     (pos-test-relative (pos-uncovered-org-files root)
                                        root))))))

(ert-deftest pos/hidden-directories-are-not-walked ()
  "The uncovered walker does not enter a hidden directory, nor read a hidden file.
A virtual environment or .git may hold Org files that are nobody's
tasks; a hidden file or a lock file in a directory it does enter is
left alone, as the corpus leaves it."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "") ("attic/notes.org" . "")
          ("attic/.draft.org" . "")
          (".venv/lib/site-packages/x.org" . "") (".git/x.org" . ""))
      (make-symbolic-link "someone@somewhere.1234"
                          (expand-file-name "attic/.#notes.org" root))
      (should (equal '("attic/notes.org")
                     (pos-test-relative (pos-uncovered-org-files root)
                                        root))))))

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
  (pos-test-with-files root '(("intray.org" . "* WAITING held\n"))
    (let ((before org-todo-keywords))
      (with-current-buffer (pos-visit (expand-file-name "intray.org" root))
        (should (equal '("TODO" "NEXT" "WAITING" "SOMEDAY" "DONE" "CANCELLED")
                       org-todo-keywords-1))
        (should (equal '("DONE" "CANCELLED") org-done-keywords)))
      (should (eq before org-todo-keywords)))))

(ert-deftest pos/the-one-sequence-is-in-force-without-a-file-line ()
  "A file with no #+TODO line is read by the one sequence.
CANCELLED is done, so an entry in that state is done and the sweep
would take it; WAITING is open under the sequence, though no line in
the file says so, so an entry in that state is not."
  (pos-test-with-files root
      '(("intray.org" . "* CANCELLED dropped\n* WAITING held\n* TODO open\n"))
    (with-current-buffer (pos-visit (expand-file-name "intray.org" root))
      (should (equal '(t nil nil)
                     (org-map-entries (lambda () (and (org-entry-is-done-p) t))
                                      nil 'file))))))

;;;; The root and its settings

(ert-deftest pos/a-repository-s-pos-config-el-sets-what-it-names ()
  "Loading a root's pos-config.el sets the settings it names and says so.
The test directory holds one of the shape a repository keeps, and
`pos-load-config' returns non-nil for it.  A root without one is no
error: the tools run with the defaults, and the result is nil."
  (let (pos-prose-directories pos-refile-rules)
    (should (pos-load-config pos-test-directory))
    (should (equal '("meta/journal" "meta/specs") pos-prose-directories))
    (should (equal '(("invoice\\|client" . "work/work-projects.org")
                     ("dentist\\|checkup" . "body/body-projects.org"))
                   pos-refile-rules)))
  (should-not (pos-load-config "/nonexistent/")))

(ert-deftest pos/a-retired-setting-in-pos-config-el-is-reported ()
  "A pos-config.el that still sets a retired setting is told so, and loads.
`pos-pillars' once named the directories the sweep covered; the corpus
reads every file the configuration allows, so the setting is no
longer read.  The message names the setting and says as much."
  (pos-test-with-files root
      `(("pos-config.el" . ,(pos-test-lines ";;; -*- lexical-binding: t -*-"
                                            "(setq pos-pillars '(\"life\"))")))
    (unwind-protect
        (ert-with-message-capture messages
          (should (pos-load-config root))
          (should (string-match-p "pos-pillars" messages))
          (should (string-match-p "no longer read" messages)))
      (makunbound 'pos-pillars))))

(ert-deftest pos/the-roam-index-exclusions-in-pos-config-el-are-reported ()
  "A pos-config.el that still sets the roam index's exclusions is told so.
`pos-roam-excluded-directories' once named the directories the index
left out; the index covers the corpus, so the setting is no longer
read.  The message names the setting and says where exclusions go."
  (pos-test-with-files root
      `(("pos-config.el"
         . ,(pos-test-lines ";;; -*- lexical-binding: t -*-"
                            "(setq pos-roam-excluded-directories '(\"attic\"))")))
    (unwind-protect
        (ert-with-message-capture messages
          (should (pos-load-config root))
          (should (string-match-p "pos-roam-excluded-directories" messages))
          (should (string-match-p "under exclude" messages)))
      (makunbound 'pos-roam-excluded-directories))))

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

(ert-deftest pos/normalising-runs-over-every-writable-file-and-no-other ()
  "Normalising runs over the writable files and leaves the rest alone.
A lower-case #+todo: line counts too, and a book's own keyword line,
nested below the top level, is now the sweep's business.  A keyword
line in archives/ or in a product repository is not."
  (pos-test-with-files root
      `(("intray.org" . "#+TODO: TODO | DONE\n* TODO a\n")
        ("life/life-areas.org" . "#+todo: TODO WIP | DONE\n* WIP b\n")
        ("life/resources/book.org" . "#+TODO: TODO | DONE\n")
        ("archives/old.org" . "#+TODO: TODO | DONE\n")
        ("vendor/lib/.git/HEAD" . ,pos-test-git-head)
        ("vendor/lib/README.org" . "#+TODO: TODO | DONE\n"))
    (let ((pos-directory root))
      (should (equal '(:lines-removed 3 :respelled 0)
                     (pos-normalise-keywords)))
      (should (equal "" (pos-test-text root "life/resources/book.org")))
      (should (equal "#+TODO: TODO | DONE\n"
                     (pos-test-text root "archives/old.org")))
      (should (equal "#+TODO: TODO | DONE\n"
                     (pos-test-text root "vendor/lib/README.org"))))))

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

(ert-deftest pos/a-methodologys-checks-are-run-by-the-lint ()
  "A check a methodology declares is run in the project, its lines findings.
A FILE:LINE: MESSAGE line is a finding at that file, relative to the
project; another line is a finding at the project; a check that exits
2 is a finding at the methodology with its standard error; a check
bin/ does not hold is a finding at the methodology."
  (pos-test-with-files root
      '((".clanka/config.yml"
         . "pos: 2\nmethodologies: methodologies\nchildren:\n  - path: methodologies/adr\n")
        ("intray.org" . "* TODO open\n")
        ("methodologies/adr/methodology.yaml"
         . "methodology: 1\nchecks:\n  - adr-check\n  - adr-broken\n  - adr-missing\n")
        ("methodologies/adr/bin/adr-check"
         . "#!/bin/sh\necho 'decisions/0002.md:3: status not known'\necho 'index out of date'\nexit 1\n")
        ("methodologies/adr/bin/adr-broken"
         . "#!/bin/sh\necho 'no decisions directory' >&2\nexit 2\n"))
    (dolist (name '("adr-check" "adr-broken"))
      (set-file-modes (expand-file-name (concat "methodologies/adr/bin/" name) root) #o755))
    (let ((pos-directory root))
      (should (equal (pos-test-lines
                      ".:1: index out of date"
                      "decisions/0002.md:3: status not known"
                      "methodologies/adr:1: check adr-broken could not check: no decisions directory"
                      "methodologies/adr:1: check adr-missing is not in bin/ of adr")
                     (pos-lint-format (pos-lint) root))))))

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
      (should (equal (list (list file 1
                                 "stale keyword CANCELED (not in the sequence)")
                           (list file 2
                                 "stale keyword CLARIFY (not in the sequence)")
                           (list file 5
                                 "stale keyword BACKLOG (not in the sequence)"))
                     (pos-lint-stale-keyword file))))))

(ert-deftest pos/an-intray-item-that-is-not-todo-is-a-finding ()
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
      (should (equal (list (list file 3 "NEXT item in the intray: clarified, to be placed")
                           (list file 4 "SOMEDAY item in the intray: clarified, to be placed")
                           (list file 5 "DONE item in the intray: finished, to be retired")
                           (list file 6 "CANCELLED item in the intray: finished, to be retired"))
                     (pos-lint-clarified-in-intray file)))
      (should-not (pos-lint-clarified-in-intray
                   (expand-file-name "tasks.org" root))))))

(ert-deftest pos/a-waiting-item-that-does-not-say-who-or-when-is-a-finding ()
  "A WAITING item says who or what it waits on, and since when.
Who or what is a DELEGATED_TO property or the item's own text.  Since
when is the record of its change to WAITING or a date in its own
text.  Here bare says neither; roofer says who and not when; logged
has the record and no text; smith has the property and the record;
letter says both in its text.  A child's text is not the item's."
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
             "* WAITING letter"
             "Asked the council on [2026-10-03 Sat]."
             "* NEXT not waiting")))
    (let ((file (expand-file-name "tasks.org" root)))
      (should (equal (list (list file 1 (concat "WAITING item does not say who or"
                                                " what it waits on, or since when"))
                           (list file 4 "WAITING item does not say since when")
                           (list file 6 (concat "WAITING item does not say who or"
                                                " what it waits on")))
                     (pos-lint-waiting-without-who-or-when file))))))

(ert-deftest pos/a-todo-line-in-a-file-is-a-finding ()
  "A #+TODO line in a file of the corpus is reported, with the fix to run.
It overrides the one sequence for that file; `pos-normalise-keywords'
removes it.  The match is case-insensitive, as Org's is."
  (pos-test-with-files root
      '(("life/life-areas.org" . "#+TITLE: x\n#+todo: TODO | DONE\n* TODO a\n"))
    (let ((file (expand-file-name "life/life-areas.org" root))
          (text (concat "#+TODO line overrides the one sequence;"
                        " run pos-normalise-keywords")))
      (should (equal (list (list file 2 text))
                     (pos-lint-todo-line file))))))

(ert-deftest pos/a-product-s-own-keyword-line-stands-and-is-no-finding ()
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
                                    (string-prefix-p "#+TODO" (nth 2 finding)))
                                  (pos-lint))
                      root))))
    (let ((pos-visit-corpus (pos-corpus root)))
      (with-current-buffer (pos-visit (expand-file-name "kit/todo.org" root))
        (goto-char (point-max))
        (should (equal "SPIKE" (org-get-todo-state)))))))

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
  "A task keyword in a file the corpus does not read is stranded.
Lint reports it: the sweep never visits an attic or an underscored
scratch directory, so DONE there would never archive and TODO never
reach an agenda.  Archive files are not uncovered, so the swept tasks
in them are not stranded; and a stray Org file in the archive
directory is read by the corpus, so it is not stranded either."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "* TODO read\n")
          ("_scratch/old.org" . "* DONE old\n")
          ("attic/notes.org" . "* Chapter\n** TODO write it\n")
          ("archive/orgmode/2026-W35/intray.org_archive" . "* DONE swept\n")
          ("archive/orgmode/2026-W35/notes.org" . "* TODO stray\n"))
      (let ((old (expand-file-name "_scratch/old.org" root))
            (notes (expand-file-name "attic/notes.org" root))
            (outside "task keyword outside the agenda files (%s)"))
        (should (equal (list (list old 1 (format outside "DONE"))
                             (list notes 2 (format outside "TODO")))
                       (pos-lint-stranded-tasks root)))))))

(ert-deftest pos/a-task-in-a-methodologys-own-file-is-not-stranded ()
  "A task in a methodology's own file is the method's, not a stranded one.
A methodology a project uses may document itself in Org; the corpus
does not read it, and nor does this check.  A task in an attic
beside it still is stranded."
  (pos-test-configured
    (pos-test-with-files root
        '((".clanka/config.yml"
           . "pos: 2\nprojects: projects/\nchildren:\n  - path: projects/p\n")
          ("projects/p/.clanka/config.yml"
           . "pos: 2\nmethodologies: methodologies\nchildren:\n  - path: methodologies/adr\n")
          ("projects/p/methodologies/adr/README.org" . "* NEXT Inside the methodology\n")
          ("attic/notes.org" . "* TODO write it\n"))
      (should (equal (list (list (expand-file-name "attic/notes.org" root) 1
                                 "task keyword outside the agenda files (TODO)"))
                     (pos-lint-stranded-tasks root))))))

(ert-deftest pos/copies-of-one-task-in-two-files-name-each-other ()
  "Copies of one task heading in two corpus files each report the other.
Headings compare lower-cased and trimmed, so Fix the gate and fix the
gate are one task.  A copy nested below the top level is seen like
any other.  A plain heading, Finances, is no task and is not compared."
  (pos-test-with-files root
      '(("intray.org" . "* TODO fix the gate\n* Finances\n")
        ("life/home/areas.org" . "* Finances\n** NEXT Fix the gate\n"))
    (should (equal (list (list (expand-file-name "intray.org" root) 1
                               "duplicate task (also life/home/areas.org:2)")
                         (list (expand-file-name "life/home/areas.org" root) 2
                               "duplicate task (also intray.org:1)"))
                   (pos-lint-duplicate-tasks root)))))

;;;; Stranded tasks

(ert-deftest pos/only-the-topmost-open-task-of-a-nest-is-rescued ()
  "A stranded open task is listed once, at the top of its nest.
Stranded means in a file the corpus does not read, an attic here.
Rescue lists open tasks only: a done one stays where it is, for lint
to report, while an open one's subtasks will move with it and are not
listed again."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . "* Unsorted\n")
          ("attic/notes.org" . ,(pos-test-lines "* Seminar"
                                                "** TODO follow up"
                                                "*** TODO nested"
                                                "** DONE said hello")))
      (should (equal (list (list (expand-file-name "attic/notes.org" root)
                                 2 "follow up"))
                     (pos-stranded-open-tasks root))))))

(ert-deftest pos/a-rescued-task-lands-under-unsorted-linked-back ()
  "A rescued task is moved under the intray's Unsorted, with a link back.
The task is open in a file the corpus does not read.  The link names
the file and the parent heading it came from, relative to the root,
and goes after the planning line so Org still reads the schedule.
Nested open tasks move with it; the file keeps the rest."
  (pos-test-configured
    (pos-test-with-files root
        `(("intray.org" . ,(pos-test-lines "* Zettles"
                                           "* Unsorted"
                                           "** TODO already here"
                                           "* Sorted"))
          ("attic/notes.org"
           . ,(pos-test-lines "* Seminar"
                              "** TODO follow up"
                              "SCHEDULED: <2026-09-14 Mon>"
                              "some notes"
                              "*** TODO nested"
                              "** DONE said hello")))
      (let ((pos-directory root))
        (pos-refile-stranded)
        (should (equal "* Seminar\n** DONE said hello\n"
                       (pos-test-text root "attic/notes.org")))
        (should (equal (pos-test-lines
                        "* Zettles"
                        "* Unsorted"
                        "** TODO already here"
                        "** TODO follow up"
                        "SCHEDULED: <2026-09-14 Mon>"
                        (concat "From [[file:attic/notes.org::*Seminar]"
                                "[attic/notes.org: Seminar]]")
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
          ("attic/notes.org" . "* TODO top level task\n"))
      (let ((pos-directory root))
        (pos-refile-stranded)
        (should (equal (pos-test-lines
                        "* Sorted"
                        "* Unsorted"
                        "** TODO top level task"
                        "From [[file:attic/notes.org][attic/notes.org]]")
                       (pos-test-text root "intray.org")))))))

(ert-deftest pos/a-dry-run-rescue-lists-the-tasks-and-moves-none ()
  "A dry run returns what it would move and changes neither file."
  (pos-test-configured
    (pos-test-with-files root
        '(("intray.org" . "* Unsorted\n")
          ("attic/notes.org" . "* TODO a task\n"))
      (let ((pos-directory root))
        (should (equal 1 (length (pos-refile-stranded t))))
        (should (equal "* TODO a task\n"
                       (pos-test-text root "attic/notes.org")))
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 0)
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
                           :skipped 1 :unwritable 0 :vanished 0 :stale 0)
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 0)
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
                           :skipped 0 :unwritable 0 :vanished 1 :stale 0)
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
                           :skipped 0 :unwritable 0 :vanished 0 :stale 1)
                     (pos-dedupe-apply root plan)))
      (should (equal "* TODO fix the gate\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos/a-group-linked-from-a-child-repository-is-unwritable ()
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

(ert-deftest pos/a-group-with-a-copy-in-a-child-repository-is-unwritable ()
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

;;;; Refile

(ert-deftest pos/the-first-matching-refile-rule-wins ()
  "A heading is suggested the file of the first rule it matches, or none.
Rules match without regard to case; a heading no rule matches has no
suggestion, and is left for a person."
  (pos-test-configured
    (should (equal "work/work-projects.org"
                   (pos-refile-suggest "Client invoice query")))
    (should (equal "body/body-projects.org"
                   (pos-refile-suggest "Dentist checkup")))
    (should-not (pos-refile-suggest "Visitor arrives"))))

(ert-deftest pos/the-refile-plan-has-one-row-per-intray-entry ()
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
      (should (equal '(:moved 1 :left 0 :missing 0 :unwritable 0 :vanished 0)
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
      (should (equal '(:moved 0 :left 0 :missing 2 :unwritable 0 :vanished 1)
                     (pos-refile-apply root plan)))
      (should (equal "* Unsorted\n** TODO a\n** TODO b\n** TODO c\n"
                     (pos-test-text root "intray.org"))))))

(ert-deftest pos/a-target-in-a-child-repository-is-unwritable-and-left ()
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

(ert-deftest pos/a-dry-run-refile-moves-nothing ()
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

(provide 'pos-test)
;;; pos-test.el ends here
