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
(require 'pos-test-support)

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

(ert-deftest pos/a-new-id-is-lower-case-where-uuidgen-prints-upper-case ()
  "A new ID is lower case where the system's uuidgen prints upper case."
  (let ((org-id-method 'uuid)
        (org-id-prefix nil)
        (org-id-uuid-program "echo 0A1B2C3D-4E5F-4A6B-8C7D-9E0F1A2B3C4D"))
    (should (equal "0a1b2c3d-4e5f-4a6b-8c7d-9e0f1a2b3c4d" (pos-new-id)))))

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
  (let (pos-refile-rules)
    (should (pos-load-config pos-test-directory))
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

;;;; The intray's Unsorted

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

(provide 'pos-test)
;;; pos-test.el ends here
