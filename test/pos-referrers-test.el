;;; pos-referrers-test.el --- Tests for pos-referrers.el  -*- lexical-binding: t -*-

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
;; Each test is named for the rule it pins, and its tree is in the test.

;;; Code:

(require 'ert)
(require 'pos-referrers)
(require 'pos-test-support)

(ert-deftest pos-referrers/links-into-a-path-are-found-by-file-and-by-id ()
  "A link into a path is found whether it names a file or an ID.
Here the path is projects/paint.  The intray links to a file in it,
to the directory itself and to an ID a heading in it holds; notes
links to an ID its file holds, twice on one line.  A link to a file
beside the path, a web link, an ID no file holds and an ID held
outside the path are not links into it.  Links within the path
itself are not listed.  Each link has its file, line, text and the
file it leads to; links on one line are in the order of what they
lead to."
  (pos-test-with-files root
      `((".pos/config.yaml" . "pos: 2\nprojects: projects/\n")
        ("intray.org"
         . ,(concat "* Unsorted\n"
                    "** TODO One [[file:projects/paint/project.org::*Outcome][o]]\n"
                    "See [[file:projects/paint][the project]] and [[id:colour][c]],\n"
                    "[[file:projects/painting.org]], [[https://example.org][web]],\n"
                    "[[id:absent]] and [[id:shelf][s]].\n"))
        ("life/notes.org" . "ü [[id:paint]] and [[id:paint][again]]\n")
        ("projects/painting.org" . ":PROPERTIES:\n:ID: shelf\n:END:\n")
        ("projects/paint/project.org"
         . ,(concat ":PROPERTIES:\n:ID:       paint\n:END:\n"
                    "* Outcome\n[[file:notes.org]] [[id:colour]]\n"))
        ("projects/paint/notes.org"
         . "* Colour\n:PROPERTIES:\n:ID: colour\n:END:\n"))
    (let ((project (expand-file-name "projects/paint/project.org" root))
          (notes (expand-file-name "projects/paint/notes.org" root))
          (intray (expand-file-name "intray.org" root))
          (life (expand-file-name "life/notes.org" root)))
      (should (equal (list (list intray 2 "file:projects/paint/project.org::*Outcome"
                                 project)
                           (list intray 3 "file:projects/paint"
                                 (expand-file-name "projects/paint" root))
                           (list intray 3 "id:colour" notes)
                           (list life 1 "id:paint" project)
                           (list life 1 "id:paint" project))
                     (pos-referrers root "projects/paint")))
      (should (equal (list (list intray 2 "file:projects/paint/project.org::*Outcome"
                                 project)
                           (list life 1 "id:paint" project)
                           (list life 1 "id:paint" project))
                     (pos-referrers root "projects/paint/project.org")))
      (should-not (pos-referrers root "life")))))

(ert-deftest pos-referrers/an-id-two-files-hold-leads-to-both ()
  "An id link leads to each file that holds the ID.
Here two files of the path hold twin, so the link is listed twice,
once for each."
  (pos-test-with-files root
      '(("intray.org" . "[[id:twin]]\n")
        ("old/a.org" . ":PROPERTIES:\n:ID: twin\n:END:\n")
        ("old/b.org" . "* H\n:PROPERTIES:\n:ID: twin\n:END:\n"))
    (should (equal (list (expand-file-name "old/a.org" root)
                         (expand-file-name "old/b.org" root))
                   (mapcar (lambda (link) (nth 3 link))
                           (pos-referrers root "old"))))))

(provide 'pos-referrers-test)
;;; pos-referrers-test.el ends here
