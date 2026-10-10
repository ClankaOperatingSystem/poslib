;;; pos-salvage-test.el --- Tests for pos-salvage.el  -*- lexical-binding: t -*-

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
;; Each test is named for the rule it pins.  The tree is a garden with
;; an intray and an attic, the item about to be sealed.  The attic's
;; notes hold a task with an ID, a schedule and a task beneath it, a
;; done task, and a task with no ID.  The seals are real.

;;; Code:

(require 'ert)
(require 'pos-salvage)
(require 'pos-relink)
(require 'pos-seal)
(require 'pos-test-support)

(defconst pos-salvage-test-notes
  (concat "#+TITLE: Notes\n\n"
          "* Thoughts\nProse.\n"
          "** NEXT Buy a hinge :shop:\n"
          "SCHEDULED: <2030-01-01 Tue +1w>\n"
          ":PROPERTIES:\n:ID:       hinge-id\n:END:\n"
          "At the ironmonger.\n"
          "*** TODO Measure the gate\n"
          ":PROPERTIES:\n:ID:       gate-id\n:END:\n"
          "** DONE Paint\n"
          "* TODO Call the joiner\n")
  "The attic's notes: two open tasks at the top of a nest, and a done one.")

(defmacro pos-salvage-test-with-garden (&rest body)
  "Evaluate BODY with `root' a garden with an intray and an attic.
`attic' is the attic, `notes' its Org file and `intray' the intray."
  (declare (indent 0))
  `(pos-test-with-files dir
       `(("garden/.git/HEAD" . "ref: refs/heads/master\n")
         ("garden/intray.org" . "* Unsorted\n** TODO Already here\n* Sorted\n")
         ("garden/attic/notes.org" . ,pos-salvage-test-notes)
         ("garden/attic/.draft.org" . "* TODO Hidden\n")
         ("garden/attic/data.txt" . "* TODO Not Org\n"))
     (let* ((root (file-name-as-directory
                   (file-truename (expand-file-name "garden" dir))))
            (attic (expand-file-name "attic" root))
            (notes (expand-file-name "attic/notes.org" root))
            (intray (expand-file-name "intray.org" root)))
       (ignore attic notes intray)
       ,@body)))

(defun pos-salvage-test-new-id (text)
  "Return the one ID in TEXT that is neither hinge-id nor gate-id."
  (let (ids (start 0))
    (while (string-match "^:ID: +\\(\\S-+\\)$" text start)
      (unless (member (match-string 1 text) '("hinge-id" "gate-id"))
        (push (match-string 1 text) ids))
      (setq start (match-end 0)))
    (should (= 1 (length ids)))
    (car ids)))

(ert-deftest pos-salvage/the-open-tasks-are-the-topmost-of-each-nest ()
  "The open tasks of an item are listed once, at the top of each nest.
A task beneath an open task goes with it and is not listed.  A done
task is not listed.  The files are those a corpus reads by name:
here .draft.org is hidden and data.txt is not Org.  A single file is
an item too."
  (pos-salvage-test-with-garden
    (let ((tasks `((,notes 5 "NEXT" "Buy a hinge")
                   (,notes 16 "TODO" "Call the joiner"))))
      (should (equal tasks (pos-salvage-open-tasks attic)))
      (should (equal tasks (pos-salvage-open-tasks notes))))))

(ert-deftest pos-salvage/a-task-is-copied-to-the-intray-and-closed-where-it-stood ()
  "Each open task is copied to the end of Unsorted and closed in its file.
The copy keeps its state, tags, schedule, text and what is beneath
it.  It takes the task's ID, or a new one, and says where it came
from by a file link from the root, to the heading.  The original is
CANCELLED, as is the open task beneath it, and neither keeps an ID;
its repeater is not followed.  It cites the copy by an id link.  The
done task and the prose are as they were.  The tasks are returned as
they stood."
  (pos-salvage-test-with-garden
    (should (equal `((,notes 5 "NEXT" "Buy a hinge")
                     (,notes 16 "TODO" "Call the joiner"))
                   (pos-salvage root attic)))
    (let* ((copied (pos-test-file-string intray))
           (id (pos-salvage-test-new-id copied)))
      (should (equal (concat
                      "* Unsorted\n** TODO Already here\n"
                      "** NEXT Buy a hinge :shop:\n"
                      "SCHEDULED: <2030-01-01 Tue +1w>\n"
                      ":PROPERTIES:\n:ID:       hinge-id\n:END:\n"
                      "Salvaged from [[file:attic/notes.org::*Buy a hinge]"
                      "[attic/notes.org]]; nobody knew where it belonged.\n"
                      "At the ironmonger.\n"
                      "*** TODO Measure the gate\n"
                      ":PROPERTIES:\n:ID:       gate-id\n:END:\n"
                      "** TODO Call the joiner\n"
                      ":PROPERTIES:\n:ID:       " id "\n:END:\n"
                      "Salvaged from [[file:attic/notes.org::*Call the joiner]"
                      "[attic/notes.org]]; nobody knew where it belonged.\n"
                      "* Sorted\n")
                     copied))
      (should (equal (concat
                      "#+TITLE: Notes\n\n"
                      "* Thoughts\nProse.\n"
                      "** CANCELLED Buy a hinge :shop:\n"
                      "SCHEDULED: <2030-01-01 Tue +1w>\n"
                      "Salvaged to the intray: [[id:hinge-id][Buy a hinge]]\n"
                      "At the ironmonger.\n"
                      "*** CANCELLED Measure the gate\n"
                      "** DONE Paint\n"
                      "* CANCELLED Call the joiner\n"
                      "Salvaged to the intray: [[id:" id "][Call the joiner]]\n")
                     (pos-test-file-string notes))))
    ;; Nothing is left to salvage, and a second run writes nothing.
    (let ((before (pos-test-file-string intray)))
      (should-not (pos-salvage root attic))
      (should (equal before (pos-test-file-string intray))))))

(ert-deftest pos-salvage/a-dry-run-lists-the-tasks-and-writes-nothing ()
  "A dry run returns the tasks and changes neither file.
The report gives the count, then each task by file and line from the
root, with its state and title, worded as what would be done."
  (pos-salvage-test-with-garden
    (let ((before (pos-test-file-string intray))
          (tasks (pos-salvage root attic t)))
      (should (equal (concat "Would salvage 2 open tasks to intray.org\n"
                             "attic/notes.org:5: NEXT Buy a hinge\n"
                             "attic/notes.org:16: TODO Call the joiner\n")
                     (pos-salvage-report root tasks t)))
      (should (equal "Salvaged 1 open task to intray.org\nattic/notes.org:5: NEXT Buy a hinge\n"
                     (pos-salvage-report root (list (car tasks)) nil)))
      (should (equal before (pos-test-file-string intray)))
      (should (equal pos-salvage-test-notes (pos-test-file-string notes))))))

(ert-deftest pos-salvage/a-heading-is-cited-by-its-custom-id-or-not-at-all ()
  "The copy cites a heading by its CUSTOM_ID if it has one.
A title that holds a link cannot be written inside a link: the copy
cites the file alone, and the original's link shows the title as it
reads.  An intray with no Unsorted gains one at its end."
  (pos-test-with-files root
      `(("intray.org" . "* Sorted\n")
        ("attic/notes.org"
         . ,(concat "* TODO Oil the lock\n:PROPERTIES:\n:CUSTOM_ID: lock\n:END:\n"
                    "* TODO Read [[https://example.org][the paper]]\n")))
    (pos-salvage root (expand-file-name "attic" root))
    (let ((copied (pos-test-file-string (expand-file-name "intray.org" root))))
      (should (string-prefix-p "* Sorted\n* Unsorted\n** TODO Oil the lock\n" copied))
      (should (string-match-p
               (regexp-quote "Salvaged from [[file:attic/notes.org::#lock][attic/notes.org]];")
               copied))
      (should (string-match-p
               (regexp-quote "Salvaged from [[file:attic/notes.org][attic/notes.org]];")
               copied)))
    (should (string-match-p
             "^Salvaged to the intray: \\[\\[id:[^]]+\\]\\[Read the paper\\]\\]$"
             (pos-test-file-string (expand-file-name "attic/notes.org" root))))))

(ert-deftest pos-salvage/what-cannot-be-salvaged-is-refused-and-nothing-is-written ()
  "Salvage is refused, as a user error, before anything is written.
Refused: a source that is not there, one that is not beneath the
root, the root itself, and one that holds the intray; an intray that
is missing while there is a task to copy.  With no open task, as in
the second tree, a missing intray is no matter."
  (pos-salvage-test-with-garden
    (should-error (pos-salvage root (expand-file-name "nowhere" root))
                  :type 'user-error)
    (should-error (pos-salvage root dir) :type 'user-error)
    (should-error (pos-salvage root root) :type 'user-error)
    (should-error (pos-salvage root intray) :type 'user-error)
    (delete-file intray)
    (should-error (pos-salvage root attic) :type 'user-error)
    (should (equal pos-salvage-test-notes (pos-test-file-string notes)))
    (should-not (file-exists-p intray)))
  (pos-test-with-files root '(("attic/notes.org" . "* DONE Paint\n"))
    (should-not (pos-salvage root (expand-file-name "attic" root)))))

(ert-deftest pos-salvage/after-the-seal-and-the-relink-the-copy-cites-the-item ()
  "The seal and its relink plan finish what salvage began.
The relink plan, made from the seal plan after the salvage, rewrites
the copy's file link to the sealed item's ipfs:// link, with the
path within the item and the heading.  The ID the copy took is held
by the intray alone, so the plan is not refused for an ID two files
hold, and a link to the task by its ID still leads to the copy.  In
the sealed file the task is closed, and its citation of the copy is
no longer an id link: sealing resolved it."
  (pos-salvage-test-with-garden
    (pos-salvage root attic)
    ;; A hidden file is not sealed.
    (delete-file (expand-file-name ".draft.org" attic))
    (make-directory (expand-file-name "archives" root))
    (let* ((sealed (expand-file-name "archives/attic" root))
           (seal (pos-seal-plan attic sealed))
           (relink (pos-relink-plan root seal)))
      (pos-test-approve seal)
      (should (equal '("intray.org")
                     (pos-relink-apply relink (pos-bytes-sha (pos-bytes-json relink)))))
      (let ((item (pos-links-link sealed))
            (copied (pos-test-file-string intray))
            (kept (pos-test-file-string (expand-file-name "notes.org" sealed))))
        (should (string-match-p
                 (regexp-quote (concat "Salvaged from [[" item
                                       "/notes.org::*Buy a hinge][attic/notes.org]];"))
                 copied))
        (should (string-match-p "^:ID: +hinge-id$" copied))
        (should (string-match-p "^\\*\\* CANCELLED Buy a hinge" kept))
        (should (string-match-p "^Salvaged to the intray: \\[\\[ipfs://" kept))
        (should-not (string-match-p "\\[\\[id:" kept))))))

(provide 'pos-salvage-test)
;;; pos-salvage-test.el ends here
