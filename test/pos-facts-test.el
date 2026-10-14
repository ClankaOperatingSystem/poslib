;;; pos-facts-test.el --- Tests for pos-facts.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The repositories are made on disk, with a bare one
;; as the upstream, so that pushed is told as git tells it.

;;; Code:

(require 'ert)
(require 'pos-facts)
(require 'pos-seal)
(require 'pos-test-support)

(defun pos-facts-test-commit (root subject &rest files)
  "Write FILES, a plist of path and text, in ROOT and commit them as SUBJECT.
Return the commit's hash."
  (while files
    (pos-test-write-files root (list (cons (pop files) (pop files)))))
  (pos-test-git root "add" "-A")
  (pos-test-git root "commit" "-q" "--allow-empty" "-m" subject)
  (pos-test-git root "rev-parse" "HEAD"))

(defun pos-facts-test-seal (root rel text)
  "Seal TEXT as REL in the archive at ROOT's archives/."
  (make-directory (expand-file-name "archives" root) t)
  (pos-test-approve (pos-seal-stage text (expand-file-name rel (expand-file-name "archives" root)))))

(ert-deftest pos-facts/commits-and-events-after-a-commit-are-listed ()
  "The report lists the commits after the one named, oldest first, then the events.
A commit the upstream has is pushed; one it lacks is not.  An event
whose file the repository tracks is committed; one it does not is not.
Here one seal is committed and pushed, a note is committed and not
pushed, and a second seal is not committed."
  (pos-test-with-temp-dir dir
    (let* ((root (file-name-as-directory (file-truename (expand-file-name "root" dir))))
           (origin (expand-file-name "origin.git" dir)))
      (make-directory root)
      (pos-test-git-init root)
      (let ((since (pos-facts-test-commit root "Begin" "README" "A root.\n")))
        (pos-facts-test-seal root "notes/one.org" "one\n")
        (pos-facts-test-commit root "Seal one")
        (pos-test-git dir "init" "-q" "--bare" origin)
        (pos-test-git root "remote" "add" "origin" origin)
        (pos-test-git root "push" "-q" "-u" "origin" "master")
        (pos-facts-test-commit root "Add a note" "notes.txt" "later\n")
        (pos-facts-test-seal root "notes/two.org" "two\n")
        (should (string-match-p
                 (concat "\\`Commits after [0-9a-f]+ Begin\n"
                         "  [0-9a-f]+ Seal one (pushed)\n"
                         "  [0-9a-f]+ Add a note (not pushed)\n"
                         "\nSealed after it\n"
                         "  \\. event 1: notes/one.org (committed)\n"
                         "  \\. event 2: notes/two.org (not committed)\n\\'")
                 (pos-facts-report root since)))
        (should (equal '("Seal one" "Add a note")
                       (mapcar (lambda (commit) (alist-get 'subject commit))
                               (pos-facts-commits root since))))
        (should (equal '(1 2)
                       (mapcar (lambda (event) (alist-get 'number event))
                               (pos-facts-events root since))))))))

(ert-deftest pos-facts/nothing-after-the-commit-says-none ()
  "With no commit and no event after the one named, each list says (none).
With no upstream, the report says that pushed is not known."
  (pos-test-with-temp-dir dir
    (let ((root (file-name-as-directory (file-truename dir))))
      (pos-test-git-init root)
      (let ((since (pos-facts-test-commit root "Begin" "README" "A root.\n")))
        (should (string-match-p
                 "\\`Commits after [0-9a-f]+ Begin\n  (none)\n\nSealed after it\n  (none)\n\\'"
                 (pos-facts-report root since)))
        (pos-facts-test-commit root "Go on")
        (should (string-match-p
                 (concat "\\`Commits after [0-9a-f]+ Begin\n  [0-9a-f]+ Go on\n"
                         "  HEAD has no upstream: whether a commit is pushed is not known\n")
                 (pos-facts-report root since)))
        (should (eq :null (alist-get 'pushed (car (pos-facts-commits root since)))))))))

(ert-deftest pos-facts/the-command-takes-one-commit-the-repository-has ()
  "The command prints the report for the commit named, and exits 2 otherwise."
  (pos-test-with-temp-dir dir
    (let ((root (file-name-as-directory (file-truename dir)))
          (status nil))
      (pos-test-git-init root)
      (let ((since (pos-facts-test-commit root "Begin" "README" "A root.\n"))
            (pos-directory root))
        (let ((command-line-args-left (list since)))
          (should (equal (pos-facts-report root since)
                         (with-output-to-string (pos-facts-batch))))
          (should-not command-line-args-left))
        (cl-letf (((symbol-function 'kill-emacs)
                   (lambda (&optional code) (throw 'pos-facts-test-exit code))))
          (let ((command-line-args-left (list "nothing-of-the-kind")))
            (setq status (catch 'pos-facts-test-exit (pos-facts-batch) :returned))))
        (should (eql 2 status))))))

(provide 'pos-facts-test)
;;; pos-facts-test.el ends here
