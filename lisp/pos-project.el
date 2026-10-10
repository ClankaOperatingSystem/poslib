;;; pos-project.el --- Make a project file -*- lexical-binding: t; -*-

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

;; A project is committed to by making its file, as zettles/project.org
;; has it: one Org file where its responsibility's configuration says
;; its projects go.  The file is made with what every project has: an
;; ID, the STATUS COMMITTED, the date it was made, its outcome, a
;; scheduled review, and its next action when one is known.  The
;; start-up views read each of these from the file.
;;
;; - `pos-project-create': command.
;; - `pos-project-batch': shell entry; create WITHIN NAME TITLE OUTCOME
;;   REVIEW [NEXT].

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-corpus)

(defun pos-project--one-line-p (text)
  "Return non-nil if TEXT is a string of one line that is not blank."
  (and (stringp text)
       (not (string-empty-p (string-trim text)))
       (not (string-match-p "[[:cntrl:]]" text))))

(defun pos-project--text (id title outcome review next created)
  "Return the text of a project file.
ID, TITLE, OUTCOME and NEXT are strings, NEXT or nil; REVIEW and
CREATED are times."
  (concat ":PROPERTIES:\n"
          ":ID:       " id "\n"
          ":STATUS:   COMMITTED\n"
          ":CREATED:  " (format-time-string "[%Y-%m-%d %a]" created) "\n"
          ":END:\n"
          "#+TITLE: " title "\n\n"
          "* Outcome\n\n" outcome "\n\n"
          (if next (concat "* NEXT " next "\n") "")
          "* TODO Review " title " :review:\n"
          "SCHEDULED: " (format-time-string "<%Y-%m-%d %a>" review) "\n"))

(defun pos-project-create (root within name title outcome review &optional next)
  "Make the file of a new project of the scope WITHIN in the tree at ROOT.
WITHIN is the path of the root or a responsibility, \".\" for the
root; the file is NAME.org in the directory its configuration gives
for projects.  NAME is lower-case letters, digits and hyphens.
TITLE is one line.  OUTCOME is the text under the Outcome heading
and may have several paragraphs.  REVIEW is the date of the first
review, YYYY-MM-DD.  NEXT, if not nil, is the title of the next
action.  The file has a new Org ID, the STATUS COMMITTED and the
date in CREATED.  Return the file's name.  Refuse a scope that is
not in the tree, has no place for projects or is another
repository's; a NAME that is taken, as a file or a directory; and an
OUTCOME with a line that begins with a star."
  (unless (and (stringp name)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name)))
    (user-error "A name is lower-case letters, digits and hyphens: %s" name))
  (unless (pos-project--one-line-p title)
    (user-error "A title is one nonempty line"))
  (when (and next (not (pos-project--one-line-p next)))
    (user-error "A next action is one nonempty line"))
  (unless (and (stringp outcome) (not (string-empty-p (string-trim outcome))))
    (user-error "A project has an outcome"))
  (when (string-match-p "^\\*" outcome)
    (user-error "A line of the outcome begins with a star"))
  (unless (and (stringp review)
               (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" review))
    (user-error "A review date is YYYY-MM-DD: %s" review))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (corpus (pos-corpus root))
         (scope (seq-find (lambda (scope)
                            (and (memq (pos-scope-kind scope) '(root responsibility))
                                 (equal within (pos-scope-path scope))))
                          (pos-corpus-scopes corpus)))
         (projects (and scope (pos-corpus--projects-dir scope))))
    (unless scope
      (user-error "Not the root or a responsibility of the tree: %s" within))
    (unless projects
      (user-error "The configuration of %s names no place for projects" within))
    (unless (pos-corpus-scope-writable-p corpus scope)
      (user-error "Not a scope this root may write: %s" within))
    (let* ((directory (expand-file-name projects root))
           (file (expand-file-name (concat name ".org") directory))
           (make-backup-files nil)
           (coding-system-for-write 'utf-8-unix))
      (when (or (file-exists-p file)
                (file-exists-p (expand-file-name name directory)))
        (user-error "There is already a project named %s in %s" name within))
      (make-directory directory t)
      (write-region (pos-project--text
                     (org-id-new) (string-trim title) (string-trim outcome)
                     (date-to-time (concat review " 00:00:00"))
                     (and next (string-trim next)) (current-time))
                    nil file nil 'silent nil 'excl)
      file)))

(defun pos-project-batch ()
  "Run a project command from `command-line-args-left' on `pos-directory'.
The one command is create WITHIN NAME TITLE OUTCOME REVIEW [NEXT], as
`pos-project-create' takes them; it prints the file made, relative
to the root.  Exit 2 on any other arguments."
  (let ((root (file-name-as-directory (expand-file-name pos-directory)))
        (arguments (prog1 command-line-args-left
                     (setq command-line-args-left nil))))
    (if (and (equal (car arguments) "create")
             (memq (length arguments) '(6 7)))
        (princ (format "%s\n"
                       (file-relative-name
                        (apply #'pos-project-create root (cdr arguments))
                        root)))
      (message "Usage: create WITHIN NAME TITLE OUTCOME REVIEW [NEXT]")
      (kill-emacs 2))))

(provide 'pos-project)
;;; pos-project.el ends here
