;;; pos-views-test.el --- Tests for pos-views.el  -*- lexical-binding: t -*-

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
;; Each test is named for the rule it pins.  The tree is a root with an
;; intray and two projects, one with a next action and one with none.

;;; Code:

(require 'ert)
(require 'pos-views)
(require 'pos-test-support)

(defmacro pos-views-test-with-tree (&rest body)
  "Evaluate BODY with `root' a small tree, and no views buffer left after."
  (declare (indent 0))
  `(pos-test-with-files root
       '((".pos/config.yaml" . "pos: 2\nprojects: projects/\n")
         ("intray.org" . "* Unsorted\n** TODO Sort the shelf\n** NEXT Answer the letter\n")
         ("projects/roof/project.org"
          . ":PROPERTIES:\n:STATUS:   WIP\n:END:\n* NEXT Call the roofer\n")
         ("projects/paint.org"
          . ":PROPERTIES:\n:STATUS:   WIP\n:END:\n* TODO Choose the colour\n"))
     (unwind-protect
         (save-window-excursion ,@body)
       (when (get-buffer "*pos-views*") (kill-buffer "*pos-views*")))))

(defun pos-views-test-lines ()
  "Return the lines of the current buffer that are of something.
Each as (TEXT . ITEM), TEXT trimmed."
  (let (lines)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((item (get-text-property (point) 'pos-item)))
          (when item
            (push (cons (string-trim (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))
                        item)
                  lines)))
        (forward-line 1)))
    (nreverse lines)))

(ert-deftest pos-views/the-buffer-has-the-views-text-and-each-line-its-item ()
  "The buffer holds what `pos-startup-view' prints, and each line its item.
A line that names an item has that item, with its file and line; a
title line and a line that says none have nothing."
  (pos-views-test-with-tree
    (with-current-buffer (pos-views-show "next" root)
      (should (derived-mode-p 'pos-views-mode))
      (should buffer-read-only)
      (should (equal (pos-startup-view root "next")
                     (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal '(("projects/roof/project.org" 4 "Call the roofer")
                       ("intray.org" 3 "Answer the letter"))
                     (mapcar (lambda (line)
                               (let-alist (cdr line) (list .file .line .title)))
                             (pos-views-test-lines))))
      (dolist (line (pos-views-test-lines))
        (should (string-suffix-p (alist-get 'title (cdr line)) (car line)))))))

(ert-deftest pos-views/a-line-leads-to-its-item-or-its-scope ()
  "Visiting a line shows the item's heading; a scope's line, its file.
In the stuck view a line is of a project: paint is one file, which is
visited.  A line of nothing is refused."
  (pos-views-test-with-tree
    (with-current-buffer (pos-views-show "next" root)
      (should-error (pos-views-visit) :type 'user-error)
      (pos-views-next)
      (pos-views-visit))
    (should (equal (expand-file-name "projects/roof/project.org" root)
                   (buffer-file-name (window-buffer))))
    (with-current-buffer (window-buffer)
      (should (looking-at-p "\\* NEXT Call the roofer")))
    (with-current-buffer (pos-views-show "stuck" root)
      (pos-views-next)
      (pos-views-visit))
    (should (equal (expand-file-name "projects/paint.org" root)
                   (buffer-file-name (window-buffer))))))

(ert-deftest pos-views/a-state-is-set-from-a-line-and-the-view-read-again ()
  "Setting a state from a line changes the item and reads the view again.
The change is `pos-set-state''s, so it is recorded.  Here the intray's
TODO becomes NEXT, and the next view then lists it.  A scope's line
has no state to set."
  (pos-views-test-with-tree
    (with-current-buffer (pos-views-show "intray" root)
      (pos-views-next)
      (should (string-match-p "TODO Sort the shelf"
                              (car (car (pos-views-test-lines)))))
      (pos-views-set-state "NEXT" "Today")
      (should (string-match-p "NEXT Sort the shelf"
                              (car (car (pos-views-test-lines))))))
    (should (string-match-p
             "^- State \"NEXT\" +from \"TODO\".*\n  Today$"
             (pos-test-file-string (expand-file-name "intray.org" root))))
    (with-current-buffer (pos-views-show "stuck" root)
      (pos-views-next)
      (should-error (pos-views-set-state "DONE") :type 'user-error))))

(ert-deftest pos-views/the-weekly-views-are-shown-together-and-read-again ()
  "The weekly command shows `pos-startup-weekly-views' in one buffer.
Reverting reads the files again: an item added to a file is then
listed."
  (pos-views-test-with-tree
    (with-current-buffer (pos-views-weekly root)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-prefix-p "Intray, to be placed\n" text))
        (should (string-match-p "^Projects with no next action\n" text))
        (should (string-match-p "^Finished in the last 7 days\n" text)))
      (should-not (seq-find (lambda (line) (string-match-p "Ring the bank" (car line)))
                            (pos-views-test-lines)))
      (with-current-buffer (find-file-noselect (expand-file-name "intray.org" root))
        (goto-char (point-max))
        (insert "** TODO Ring the bank\n")
        (save-buffer))
      (revert-buffer)
      (should (seq-find (lambda (line) (string-match-p "Ring the bank" (car line)))
                        (pos-views-test-lines))))))

(provide 'pos-views-test)
;;; pos-views-test.el ends here
