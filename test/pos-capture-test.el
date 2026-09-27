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

(defmacro pos-capture-test-with-intray (contents &rest body)
  "Evaluate BODY with `root' bound to a directory whose intray holds CONTENTS."
  (declare (indent 1))
  `(let ((root (file-name-as-directory (make-temp-file "pos-capture" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "intray.org" root) (insert ,contents))
           ,@body)
       (let ((buffer (find-buffer-visiting (expand-file-name "intray.org" root))))
         (when buffer (kill-buffer buffer)))
       (delete-directory root t))))

(defun pos-capture-test-intray (root)
  "Return the saved contents of ROOT's intray."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "intray.org" root))
    (buffer-string)))

(ert-deftest pos-capture/files-the-task-at-the-end-of-unsorted ()
  (pos-capture-test-with-intray "* Unsorted\n** TODO here\n* Sorted\n"
    (should (equal 3 (pos-capture root "  Make time for sketching ")))
    (should (equal "* Unsorted\n** TODO here\n** TODO Make time for sketching\n* Sorted\n"
                   (pos-capture-test-intray root)))))

(ert-deftest pos-capture/refuses-an-empty-or-multi-line-title ()
  (pos-capture-test-with-intray "* Unsorted\n"
    (should-error (pos-capture root "   ") :type 'user-error)
    (should-error (pos-capture root "two\nlines") :type 'user-error)
    (should (equal "* Unsorted\n" (pos-capture-test-intray root)))))

(ert-deftest pos-capture/refuses-a-root-without-an-intray ()
  (let ((root (make-temp-file "pos-capture" t)))
    (unwind-protect
        (should-error (pos-capture root "a task") :type 'user-error)
      (delete-directory root t))))

(ert-deftest pos-capture/refuses-an-intray-with-unsaved-edits ()
  (pos-capture-test-with-intray "* Unsorted\n"
    (with-current-buffer (find-file-noselect (expand-file-name "intray.org" root))
      (goto-char (point-max))
      (insert "unsaved")
      (should-error (pos-capture root "a task") :type 'user-error)
      (set-buffer-modified-p nil))
    (should (equal "* Unsorted\n" (pos-capture-test-intray root)))))

;;; pos-capture-test.el ends here
