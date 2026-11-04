;;; pos-blocker-test.el --- Tests for pos-blocker.el  -*- lexical-binding: t -*-

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
(require 'pos-blocker)
(require 'pos-test-support)

(defconst pos-blocker-test-files
  '((".pos/config.yaml" . "projects: projects/\n")
    ("intray.org"
     . "* WAITING The server is back
:PROPERTIES:
:ID: server
:END:
* NEXT Deploy the site
:PROPERTIES:
:BLOCKER: [[id:server][The server is back]]
:END:
* TODO Ask the host
:PROPERTIES:
:BLOCKER: server
:END:
* TODO Wait for nothing
:PROPERTIES:
:BLOCKER: [[id:nowhere][Nowhere]]
:END:
* Notes
:PROPERTIES:
:ID: notes
:END:
* TODO Wait on the notes
:PROPERTIES:
:BLOCKER: [[id:notes][Notes]]
:END:
* TODO Release the roofer
:PROPERTIES:
:TRIGGER: [[id:roof]]
:END:
* DONE Answer the letter
:PROPERTIES:
:BLOCKER: [[id:server][The server is back]]
:END:
")
    ("projects/roof.org"
     . "* TODO Mend the roof
:PROPERTIES:
:BLOCKER: [[id:server][The server is back]] [[id:roof]]
:END:
* WAITING The roof is inspected
:PROPERTIES:
:ID: roof
:END:
* TODO Paint the eaves
:PROPERTIES:
:BLOCKER: [[id:roof][The roof is inspected]]
:END:
"))
  "A tree of events and the items they block, for the tests here.")

(ert-deftest pos-blocker/a-property-s-links-are-read-in-order ()
  "A value's links by ID are its IDs, described or not.
Anything else in the value is given as nil, once, at the end."
  (should (equal '("a" "b") (pos-blocker-links "[[id:a][A]] [[id:b]]")))
  (should (equal '("a" nil) (pos-blocker-links "[[id:a][A]], b")))
  (should (equal '(nil) (pos-blocker-links "a")))
  (should (equal '(nil) (pos-blocker-links "[[file:a.org][A]]"))))

(ert-deftest pos-blocker/each-open-event-has-the-open-items-it-blocks ()
  "An event is an item an open item's BLOCKER links to; done, it is no event.
Each is listed with the open items it blocks, a done one left out, in
the order of the files.  A heading with no state blocks nothing."
  (pos-test-with-files root pos-blocker-test-files
    (let ((events (pos-blocker-events (pos-corpus root))))
      (should (equal (mapcar (lambda (event)
                               (mapcar #'pos-blocker-heading-title event))
                             events)
                     '(("The server is back" "Deploy the site" "Mend the roof")
                       ("The roof is inspected" "Mend the roof" "Paint the eaves")))))))

(ert-deftest pos-blocker/the-lint-reports-what-is-not-an-item-s-link ()
  "A blocker or a trigger links by ID to an item of its scope or beneath it.
A value that is not links by ID, a link to no item or to a heading
that is no item, and a link up out of the scope are found.  So is a
NEXT item an open event blocks, which is TODO until the event is
done.  A trigger from the root down to a project is no finding."
  (pos-test-with-files root pos-blocker-test-files
    (should (equal (mapcar (lambda (finding)
                             (list (file-relative-name (pos-finding-file finding) root)
                                   (pos-finding-line finding)
                                   (pos-finding-message finding)))
                           (pos-blocker-lint root))
                   '(("intray.org" 5 "NEXT item blocked by an event not done: The server is back")
                     ("intray.org" 9 "BLOCKER holds something other than links by ID")
                     ("intray.org" 13 "BLOCKER links to no item: nowhere")
                     ("intray.org" 21 "BLOCKER links to no item: notes")
                     ("projects/roof.org" 1 "BLOCKER links to an item of another scope: ."))))))

(provide 'pos-blocker-test)
;;; pos-blocker-test.el ends here
