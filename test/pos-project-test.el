;;; pos-project-test.el --- Tests for pos-project.el  -*- lexical-binding: t -*-

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
;; Each test is named for the rule it pins.  The tree is a root whose
;; projects lie in projects/ and one responsibility, home, whose
;; projects lie in work/.

;;; Code:

(require 'ert)
(require 'pos-project)
(require 'pos-startup)
(require 'pos-test-support)

(defmacro pos-project-test-with-tree (&rest body)
  "Evaluate BODY with `root' holding a root and a responsibility, home."
  (declare (indent 0))
  `(pos-test-with-files root
       '((".pos/config.yaml"
          . "pos: 2\nprojects: projects/\nchildren:\n  - path: home\n")
         ("intray.org" . "* Unsorted\n")
         ("projects/taken.org" . "* TODO Here already\n")
         ("projects/held/project.org" . "* TODO Here already\n")
         ("home/.pos/config.yaml" . "pos: 2\nprojects: work/\n")
         ("home/index.org" . "* Tasks\n")
         ("shed/index.org" . "* Tasks\n"))
     ,@body))

(ert-deftest pos-project/the-file-is-made-where-the-scopes-projects-go ()
  "The file is NAME.org in the directory the scope's configuration names.
The root's projects go in projects/ and home's in work/, which is
made when it is not there.  The file's name is returned."
  (pos-project-test-with-tree
    (should (equal (expand-file-name "projects/paint-the-hall.org" root)
                   (pos-project-create root "." "paint-the-hall" "Paint the hall"
                                       "The hall is painted." "2030-02-01")))
    (should (equal (expand-file-name "home/work/mend-roof.org" root)
                   (pos-project-create root "home" "mend-roof" "Mend the roof"
                                       "The roof does not leak." "2030-03-01")))))

(ert-deftest pos-project/the-file-has-what-every-project-has ()
  "The file has an ID, STATUS COMMITTED, CREATED, an outcome and a review.
The outcome is the text given, under a top-level Outcome heading.
The review is a TODO tagged review, scheduled on the date given.  A
next action, when one is given, is a NEXT item before the review.
Titles and the outcome are trimmed."
  (pos-project-test-with-tree
    (let ((file (pos-project-create root "." "paint-the-hall" " Paint the hall "
                                    "The hall is painted.\n\nIn one colour.\n"
                                    "2030-02-01" " Choose the colour ")))
      (should (string-match-p
               (concat "\\`:PROPERTIES:\n"
                       ":ID:       [-[:xdigit:]]\\{36\\}\n"
                       ":STATUS:   COMMITTED\n"
                       ":CREATED:  \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [[:alpha:]]+\\]\n"
                       ":END:\n"
                       "#\\+TITLE: Paint the hall\n\n"
                       "\\* Outcome\n\nThe hall is painted\\.\n\nIn one colour\\.\n\n"
                       "\\* NEXT Choose the colour\n"
                       "\\* TODO Review Paint the hall :review:\n"
                       "SCHEDULED: <2030-02-01 Fri>\n\\'")
               (pos-test-file-string file))))
    (let ((file (pos-project-create root "." "no-next" "No next" "Done." "2030-02-01")))
      (should-not (string-match-p "^\\* NEXT " (pos-test-file-string file))))))

(ert-deftest pos-project/the-views-read-the-project-from-its-file ()
  "A project just made is in the projects view, with what it was made with.
It has a review, so it is not among those with a review to schedule."
  (pos-project-test-with-tree
    (pos-project-create root "home" "mend-roof" "Mend the roof"
                        "The roof does not leak." "2030-03-01" "Call the roofer")
    (should (equal (pos-startup-view-items root "projects")
                   '(((scope . "home/work/mend-roof") (scope_kind . "project")
                      (status . "COMMITTED") (review . "2030-03-01")
                      (next . "Call the roofer")
                      (outcome . "The roof does not leak.")))))
    (should-not (seq-find (lambda (row)
                            (equal "home/work/mend-roof" (alist-get 'scope row)))
                          (pos-startup-view-items root "reviews-to-schedule")))))

(ert-deftest pos-project/what-cannot-be-made-is-refused-and-nothing-written ()
  "A refusal is a `user-error' and makes no file.
Refused: a scope that is not the root or a responsibility, here a
plain directory and a path not in the tree; a name with a capital, a
space or a slash; a name taken by a file or by a directory; a title
or a next action of two lines; a blank outcome, and one with a line
that begins with a star; a review that is not a date."
  (pos-project-test-with-tree
    (dolist (arguments
             '(("shed" "a" "Title" "Outcome." "2030-02-01")
               ("nowhere" "a" "Title" "Outcome." "2030-02-01")
               ("." "Paint" "Title" "Outcome." "2030-02-01")
               ("." "paint hall" "Title" "Outcome." "2030-02-01")
               ("." "paint/hall" "Title" "Outcome." "2030-02-01")
               ("." "taken" "Title" "Outcome." "2030-02-01")
               ("." "held" "Title" "Outcome." "2030-02-01")
               ("." "a" "Two\nlines" "Outcome." "2030-02-01")
               ("." "a" "Title" "Outcome." "2030-02-01" "Two\nlines")
               ("." "a" "Title" "  " "2030-02-01")
               ("." "a" "Title" "Text\n* A heading" "2030-02-01")
               ("." "a" "Title" "Outcome." "next week")))
      (ert-info ((format "%S" arguments))
        (should-error (apply #'pos-project-create root arguments)
                      :type 'user-error)))
    (should (equal '("held" "taken.org")
                   (directory-files (expand-file-name "projects" root) nil
                                    "\\`[^.]")))))

(ert-deftest pos-project/the-command-prints-the-file-made ()
  "The command takes create and the arguments, and prints the file's path.
The path is relative to the root."
  (pos-project-test-with-tree
    (let ((pos-directory root)
          (command-line-args-left
           (list "create" "home" "mend-roof" "Mend the roof"
                 "The roof does not leak." "2030-03-01")))
      (should (equal "home/work/mend-roof.org\n"
                     (with-output-to-string (pos-project-batch))))
      (should-not command-line-args-left))))

(provide 'pos-project-test)
;;; pos-project-test.el ends here
