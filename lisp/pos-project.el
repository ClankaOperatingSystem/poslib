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
;; A project's STATUS is changed in its file, with the change and its
;; time recorded in a LOGBOOK drawer beneath the file's properties.
;;
;; A project that outgrows one file is promoted: NAME.org becomes
;; NAME/project.org, and the path links that the move would break are
;; rewritten, those in the file and those to it.  A link by ID needs
;; no rewriting.
;;
;; - `pos-project-create': command.
;; - `pos-project-set-status': command.
;; - `pos-project-promote': command.
;; - `pos-project-batch': shell entry; create WITHIN NAME TITLE OUTCOME
;;   REVIEW [NEXT], status PROJECT STATUS [NOTE], or promote PROJECT.

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-corpus)
(require 'pos-ledger)
(require 'pos-links)
(require 'pos-place)

(defcustom pos-project-statuses '("COMMITTED" "WIP" "DONE")
  "The values a project file's STATUS property takes.
COMMITTED is a project not begun, WIP one begun, DONE one closed."
  :type '(repeat string)
  :group 'pos)

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
          ":OUTCOME:  " outcome "\n"
          ":CREATED:  " (format-time-string "[%Y-%m-%d %a]" created) "\n"
          ":END:\n"
          "#+TITLE: " title "\n\n"
          (if next (concat "* NEXT " next "\n") "")
          "* TODO Review " title " :review:\n"
          "SCHEDULED: " (format-time-string "<%Y-%m-%d %a>" review) "\n"))

(defun pos-project-create (root within name title outcome review &optional next)
  "Make the file of a new project of the scope WITHIN in the tree at ROOT.
WITHIN is the path of the root or a responsibility, \".\" for the
root; the file is NAME.org in the directory its configuration gives
for projects.  NAME is lower-case letters, digits and hyphens.
TITLE is one line.  OUTCOME is one line, the value of the file's
OUTCOME property.  REVIEW is the date of the first review,
YYYY-MM-DD.  NEXT, if not nil, is the title of the next action.  The
file has a new Org ID, the STATUS COMMITTED and the date in CREATED.
Return the file's name.  Refuse a scope that is not in the tree, has
no place for projects or is another repository's; and a NAME that is
taken, as a file or a directory."
  (unless (and (stringp name)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name)))
    (user-error "A name is lower-case letters, digits and hyphens: %s" name))
  (unless (pos-project--one-line-p title)
    (user-error "A title is one nonempty line"))
  (when (and next (not (pos-project--one-line-p next)))
    (user-error "A next action is one nonempty line"))
  (unless (pos-project--one-line-p outcome)
    (user-error "An outcome is one nonempty line"))
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
                     (pos-new-id) (string-trim title) (string-trim outcome)
                     (date-to-time (concat review " 00:00:00"))
                     (and next (string-trim next)) (current-time))
                    nil file nil 'silent nil 'excl)
      file)))

(defconst pos-project--status-line "^:STATUS:[ \t]+\\(\\S-+\\)[ \t]*$"
  "A regexp matching a STATUS property's line, its value in group 1.")

(defun pos-project--status-file (corpus project)
  "Return the file of PROJECT, a scope's path, in CORPUS that has its STATUS.
The file is one of the project's with a STATUS property before its
first heading.  Refuse a path that is not a project's, and a project
with no such file."
  (let ((files (mapcar #'car
                       (seq-filter
                        (lambda (entry)
                          (and (eq 'project (pos-scope-kind (cdr entry)))
                               (equal project (pos-scope-path (cdr entry)))))
                        (pos-corpus-entries corpus)))))
    (unless files
      (user-error "Not a project of the tree: %s" project))
    (or (seq-find
         (lambda (file)
           (with-current-buffer (pos-visit file)
             (org-with-wide-buffer
              (goto-char (point-min))
              (re-search-forward
               pos-project--status-line
               (save-excursion
                 (or (re-search-forward "^\\*+ " nil t) (point-max)))
               t))))
         files)
        (user-error "No file of %s has a STATUS" project))))

(defun pos-project-set-status (root project status &optional note)
  "Set the STATUS of PROJECT in the tree at ROOT to STATUS, with NOTE.
PROJECT is the project's path as the views print it, as
\"projects/paint\".  STATUS is one of `pos-project-statuses'.  NOTE
is one line, or nil.  The change is recorded, with its time, at the
head of a LOGBOOK drawer beneath the file's properties.  Return (FILE
PREVIOUS STATUS).  Refuse the status the project has, and what
`pos-set-state' refuses of the file."
  (unless (member status pos-project-statuses)
    (user-error "Unknown status: %s (one of %s)"
                status (string-join pos-project-statuses ", ")))
  (when (and note (not (pos-project--one-line-p note)))
    (user-error "A note is one nonempty line"))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (corpus (pos-corpus root))
         (file (pos-project--status-file corpus project))
         (enable-local-variables nil)
         (enable-local-eval nil)
         (make-backup-files nil)
         (auto-save-default nil)
         (create-lockfiles t)
         (vc-handled-backends nil))
    (with-current-buffer (pos-place--buffer root corpus file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (re-search-forward pos-project--status-line)
       (let ((previous (match-string-no-properties 1)))
         (when (equal previous status)
           (user-error "The project is already %s" status))
         (unwind-protect
             (progn
               (lock-buffer)
               (atomic-change-group
                 (replace-match status t t nil 1)
                 (re-search-forward "^:END:[ \t]*\n")
                 (if (looking-at-p ":LOGBOOK:[ \t]*\n")
                     (forward-line 1)
                   (insert ":LOGBOOK:\n:END:\n")
                   (forward-line -1))
                 (insert (format "- Status %-12s from %-12s %s%s\n"
                                 (format "\"%s\"" status)
                                 (format "\"%s\"" previous)
                                 (format-time-string
                                  (org-time-stamp-format t t))
                                 (if note
                                     (concat " \\\\\n  " (string-trim note))
                                   "")))
                 (save-buffer)
                 (list file previous status)))
           (unlock-buffer)))))))

(defun pos-project--relink (file rewrite)
  "Return (BYTES . COUNT) for FILE with its path links rewritten.
REWRITE is called with the path of each link whose path is relative
and returns the path to write in its place, or nil to leave it.
BYTES is the file's bytes afterwards and COUNT the links changed."
  (let (rewrites)
    (dolist (link (pos-links-in-file file))
      (pcase-let ((`(,offset ,text ,path) link))
        (unless (or (file-name-absolute-p path) (string-prefix-p "~" path))
          (let ((new (funcall rewrite path))
                (at (string-search path text)))
            (when (and new at (not (equal new path)))
              (push (list offset text
                          (concat (substring text 0 at) new
                                  (substring text (+ at (length path)))))
                    rewrites))))))
    (cons (pos-links-rewrite (pos-ledger-read file) rewrites)
          (length rewrites))))

(defun pos-project--write (file bytes)
  "Write BYTES to FILE and bring a buffer visiting it up to date."
  (let ((coding-system-for-write 'no-conversion)
        (make-backup-files nil))
    (write-region bytes nil file nil 'silent))
  (let ((buffer (get-file-buffer file)))
    (when buffer
      (with-current-buffer buffer (revert-buffer t t)))))

(defun pos-project-promote (root project)
  "Promote PROJECT, a one-file project of the tree at ROOT, to a directory.
PROJECT is the project's path as the views print it, as
\"projects/paint\": the file projects/paint.org becomes
projects/paint/project.org, so the project's path stays as it was.
Each relative path link in the file is rewritten to lead where it
led, and each path link to the file, in a file the root may write,
is rewritten to the new place.  Return (FILE . REWRITTEN): the new
file, and (FILE . COUNT) for each file with links rewritten, the new
file among them.  Refuse a path that is not a one-file project's, a
project the root may not write, and an unsaved buffer of any file to
be written."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (corpus (pos-corpus root))
         (old (expand-file-name (concat project ".org") root))
         (owner (pos-corpus-owner corpus old))
         (directory (file-name-as-directory (expand-file-name project root)))
         (new (expand-file-name "project.org" directory))
         (others (remove old (pos-files root t)))
         rewritten)
    (unless (and owner (eq 'project (pos-scope-kind owner))
                 (equal project (pos-scope-path owner)))
      (user-error "Not a one-file project of the tree: %s" project))
    (unless (pos-corpus-writable-p corpus old)
      (user-error "Not a project this root may write: %s" project))
    (when (file-exists-p directory)
      (user-error "There is already a directory %s" project))
    (dolist (file (cons old others))
      (let ((buffer (get-file-buffer file)))
        (when (and buffer (buffer-modified-p buffer))
          (user-error "Save the modified buffer of %s first"
                      (file-relative-name file root)))))
    ;; The file's own links, read where they were written and written
    ;; for where the file will be.
    (let ((moved (pos-project--relink
                  old
                  (lambda (path)
                    (file-relative-name
                     (expand-file-name path (file-name-directory old))
                     directory)))))
      (make-directory directory)
      (pos-project--write new (car moved))
      (let ((buffer (get-file-buffer old)))
        (when buffer (kill-buffer buffer)))
      (delete-file old)
      (when (> (cdr moved) 0) (push (cons new (cdr moved)) rewritten)))
    (dolist (file others)
      (let* ((here (file-name-directory file))
             (changed (pos-project--relink
                       file
                       (lambda (path)
                         (and (equal old (expand-file-name path here))
                              (file-relative-name new here))))))
        (when (> (cdr changed) 0)
          (pos-project--write file (car changed))
          (push (cons file (cdr changed)) rewritten))))
    (cons new (nreverse rewritten))))

(defun pos-project-batch ()
  "Run a project command from `command-line-args-left' on `pos-directory'.
create WITHIN NAME TITLE OUTCOME REVIEW [NEXT], as
`pos-project-create' takes them, prints the file made, relative to
the root.  status PROJECT STATUS [NOTE], as `pos-project-set-status'
takes them, prints the file, the status it had and the one it has.
promote PROJECT, as `pos-project-promote' takes it, prints the new
file, then \"Links rewritten: FILE: COUNT\" for each file changed.
Exit 2 on any other arguments."
  (let ((root (file-name-as-directory (expand-file-name pos-directory)))
        (arguments (prog1 command-line-args-left
                     (setq command-line-args-left nil))))
    (pcase arguments
      ((and `("create" . ,rest) (guard (memq (length rest) '(5 6))))
       (princ (format "%s\n"
                      (file-relative-name
                       (apply #'pos-project-create root rest) root))))
      ((and `("status" . ,rest) (guard (memq (length rest) '(2 3))))
       (pcase-let ((`(,file ,previous ,status)
                    (apply #'pos-project-set-status root rest)))
         (princ (format "%s: STATUS %s, was %s\n"
                        (file-relative-name file root) status previous))))
      (`("promote" ,project)
       (pcase-let ((`(,file . ,rewritten) (pos-project-promote root project)))
         (princ (format "%s\n" (file-relative-name file root)))
         (pcase-dolist (`(,changed . ,count) rewritten)
           (princ (format "Links rewritten: %s: %d\n"
                          (file-relative-name changed root) count)))))
      (_ (message "%s\n%s\n%s"
                  "Usage: create WITHIN NAME TITLE OUTCOME REVIEW [NEXT]"
                  (format "       status PROJECT %s [NOTE]"
                          (string-join pos-project-statuses "|"))
                  "       promote PROJECT")
         (kill-emacs 2)))))

(provide 'pos-project)
;;; pos-project.el ends here
