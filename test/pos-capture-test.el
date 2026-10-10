;;; pos-capture-test.el --- Tests for pos-capture.el  -*- lexical-binding: t -*-

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
(require 'pos-capture)
(require 'pos-test-support)

(defmacro pos-capture-test-with-intray (contents &rest body)
  "Evaluate BODY with `root' a directory with an intray.org of CONTENTS."
  (declare (indent 1))
  `(pos-test-with-files root `(("intray.org" . ,,contents))
     ,@body))

(defun pos-capture-test-intray (root)
  "Return the saved contents of ROOT's intray."
  (pos-test-file-string (expand-file-name "intray.org" root)))

(ert-deftest pos-capture/files-the-task-at-the-end-of-unsorted ()
  "A captured task is filed at the end of Unsorted, trimmed, as a TODO.
Its properties follow it, before the next heading."
  (pos-capture-test-with-intray "* Unsorted\n** TODO here\n* Sorted\n"
    (should (equal 3 (pos-capture root "  Make time for sketching ")))
    (should (string-match-p
             (concat "\\`\\* Unsorted\n\\*\\* TODO here\n"
                     "\\*\\* TODO Make time for sketching\n"
                     ":PROPERTIES:\n\\(?::[A-Z]+:.*\n\\)+:END:\n"
                     "\\* Sorted\n\\'")
             (pos-capture-test-intray root)))))

(ert-deftest pos-capture/gives-the-task-an-id-and-the-time-of-capture ()
  "A captured task has an ID, a new Org ID, and CREATED, an inactive timestamp.
Two captures have different IDs.  No ID is written to
`org-id-locations-file'."
  (pos-capture-test-with-intray "* Unsorted\n"
    (let ((org-id-locations-file (expand-file-name "locations" root))
          ids)
      (dolist (title '("First" "Second"))
        (pos-capture root title))
      (with-current-buffer (pos-visit (expand-file-name "intray.org" root))
        (org-map-entries
         (lambda ()
           (when (org-get-todo-state)
             (push (org-entry-get (point) "ID") ids)
             (should (string-match-p
                      (concat "\\`\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
                              "[[:alpha:]]+ [0-9]\\{2\\}:[0-9]\\{2\\}\\]\\'")
                      (org-entry-get (point) "CREATED")))))
         nil 'file))
      (should (= 2 (length ids)))
      (should (seq-every-p #'org-uuidgen-p ids))
      (should-not (equal (nth 0 ids) (nth 1 ids)))
      (should-not (file-exists-p org-id-locations-file)))))

(ert-deftest pos-capture/refuses-an-empty-or-multi-line-title ()
  "An empty or multi-line title is refused and the intray is unchanged."
  (pos-capture-test-with-intray "* Unsorted\n"
    (should-error (pos-capture root "   ") :type 'user-error)
    (should-error (pos-capture root "two\nlines") :type 'user-error)
    (should (equal "* Unsorted\n" (pos-capture-test-intray root)))))

(ert-deftest pos-capture/refuses-a-root-without-an-intray ()
  "A root without an intray.org is refused."
  (pos-test-with-temp-dir root
    (should-error (pos-capture root "a task") :type 'user-error)))

(ert-deftest pos-capture/refuses-an-intray-with-unsaved-edits ()
  "An intray with unsaved edits in a buffer is refused."
  (pos-capture-test-with-intray "* Unsorted\n"
    (with-current-buffer (find-file-noselect (expand-file-name "intray.org" root))
      (goto-char (point-max))
      (insert "unsaved")
      (should-error (pos-capture root "a task") :type 'user-error)
      (set-buffer-modified-p nil))
    (should (equal "* Unsorted\n" (pos-capture-test-intray root)))))

(provide 'pos-capture-test)
;;; pos-capture-test.el ends here
