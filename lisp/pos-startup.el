;;; pos-startup.el --- Prompts and agenda views for opening a session -*- lexical-binding: t; -*-

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

;; What a session opens with: prompts for the people opening it, then
;; views of the saved Org files under a root, so that nobody has to
;; open each file to find what is next, scheduled, due or to be
;; reviewed.
;;
;; The files are the corpus, pos-corpus.el: every Org file the tree's
;; configurations allow, each belonging to a scope.  A scope is the
;; root; a responsibility, a directory whose configuration says where
;; its projects belong, as doc/pos-directory.txt has it; or a project,
;; what lies in that place.  A directory's name alone makes nothing a
;; scope.  The tree is walked once for a report.
;;
;; - `pos-startup-report': the prompts and the views, as text.
;; - `pos-startup-view': one view, as text.
;; - `pos-startup-view-items': one view, as data: what each line is of.
;; - `pos-startup-data': the report as data, for `json-serialize'.
;; - `pos-startup-batch': shell entry; --view NAME, repeated, picks views;
;;   --weekly picks `pos-startup-weekly-views'; --json prints the data.
;;
;; The data of a view is gathered while its text is made, an item for
;; each line, so the two forms cannot differ in what they list.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'seq)
(require 'pos)
(require 'pos-corpus)
(require 'pos-roam)

(defcustom pos-startup-scheduled-days 14
  "Days, from today, that the scheduled view covers."
  :type 'integer
  :group 'pos)

(defcustom pos-startup-review-days 7
  "Days, from today, that the reviews view covers."
  :type 'integer
  :group 'pos)

(defcustom pos-startup-finished-days 7
  "Days, to today, that the finished view covers."
  :type 'integer
  :group 'pos)

(defcustom pos-startup-review-tag "review"
  "The tag of a heading that schedules a review of its scope."
  :type 'string
  :group 'pos)

(defcustom pos-startup-review-covers-property "COVERS"
  "The property of a review heading that names other scopes it covers.
Its value is paths apart by spaces, each from the directory of the
heading's file to a scope.  A space within a path is written %20."
  :type 'string
  :group 'pos)

(defcustom pos-startup-active-statuses '("COMMITTED" "WIP")
  "Values of a project file's STATUS property that make it active.
An active project is expected to have a review scheduled."
  :type '(repeat string)
  :group 'pos)

(defcustom pos-startup-stuck-statuses '("WIP")
  "Values of a project file's STATUS property that make it begun.
A begun project is expected to have a next action."
  :type '(repeat string)
  :group 'pos)

(defcustom pos-startup-next-action-states '("NEXT" "WAITING")
  "The states of an item that is a project's next action.
A WAITING item is one: the project's next move is someone else's."
  :type '(repeat string)
  :group 'pos)

(defconst pos-startup-prompts
  "START-UP PROMPTS
Purpose: What are we here to do in this session?
Circumstances: What time, attention, energy, and fixed commitments constrain the choice?
Choice: Which established work, new concern, or clarification should begin?
Trust: Treat the views below as a bounded view of saved files, not a complete review.
"
  "The questions a session opens with.")

(defconst pos-startup-views
  '("next" "waiting" "someday" "scheduled" "deadlines" "reviews"
    "reviews-to-schedule" "stuck" "projects" "intray" "finished" "all")
  "The names of the views, in the order they are printed.")

(defconst pos-startup-default-views
  '("next" "waiting" "scheduled" "deadlines" "reviews" "reviews-to-schedule"
    "stuck" "intray")
  "The views `pos-startup-report' prints when none is named.")

(defconst pos-startup-weekly-views
  '("intray" "next" "waiting" "scheduled" "deadlines" "reviews" "stuck"
    "reviews-to-schedule" "someday" "finished")
  "The views of a weekly review, in the order the review reads them.
What is to be placed; what is next, waited for, dated and to be
reviewed; what is set aside; and what was finished in the week.")

(defvar pos-startup--root nil
  "The root being read, while a view is made.")

(defvar pos-startup--corpus nil
  "The corpus of `pos-startup--root', bound while a view or report is made.
The tree is walked once for it; `pos-startup-view' walks when nothing
has bound this.")

(defvar pos-startup--gathering nil
  "Non-nil while a view's lines are also gathered as data.")

(defvar pos-startup--items nil
  "What is gathered while `pos-startup--gathering', latest first.")

;;;; Files and scopes

(defun pos-startup--corpus (root)
  "Return the corpus of ROOT: the one bound for this view, else a fresh walk."
  (or pos-startup--corpus (pos-corpus root)))

(defun pos-startup--files-of-kind (kind)
  "Return the files of `pos-startup--corpus' that belong to a scope of KIND."
  (mapcar #'car
          (seq-filter (lambda (entry) (eq (pos-scope-kind (cdr entry)) kind))
                      (pos-corpus-entries pos-startup--corpus))))

(defun pos-startup--label ()
  "Return a label naming the scope of the current Org file.
The file's directory relative to the root, without the segments named
projects.  A file not named project.org adds its own base name."
  (let* ((file (file-relative-name (or buffer-file-name "") pos-startup--root))
         (scope (remove "projects"
                        (split-string (or (file-name-directory file) "") "/" t)))
         (base (file-name-base file)))
    (mapconcat #'identity
               (append scope (unless (string= base "project") (list base)))
               "/")))

(defun pos-startup--file-status (file)
  "Return the STATUS property FILE has before its first heading, or nil."
  (with-current-buffer (pos-visit file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((end (save-excursion
                  (or (re-search-forward "^\\*+ " nil t) (point-max)))))
       (when (re-search-forward "^:STATUS:[ \t]+\\(\\S-+\\)" end t)
         (match-string-no-properties 1))))))

(defun pos-startup--scope-named (name file)
  "Return the scope of `pos-startup--corpus' that NAME, written in FILE, names.
NAME is a path from FILE's directory.  Nil if it is no scope's."
  (let ((path (directory-file-name
               (file-relative-name
                (expand-file-name name (file-name-directory file))
                pos-startup--root))))
    (seq-find (lambda (scope) (string= path (pos-scope-path scope)))
              (pos-corpus-scopes pos-startup--corpus))))

(defun pos-startup--reviews-of-scopes ()
  "Return (COVERED . UNKNOWN), what the reviews of `pos-startup--corpus' cover.
A review is an open, scheduled heading tagged
`pos-startup-review-tag', in a file of a project, a responsibility
or the root.  It covers the scope its file belongs to and each scope
its `pos-startup-review-covers-property' names, and no other: a
review of a scope is not one of the scopes beneath it.

COVERED is an alist of (PATH . DATE): each scope covered, with the
earliest date, as YYYY-MM-DD, a review of it is scheduled on.
UNKNOWN is a list of (LINE SCOPE . MORE) for each path named that is
no scope's, in the order found: the line of text that reports it,
the scope of the review's file, and the path, file, line and title
as an alist."
  (let (covered unknown)
    (pcase-dolist (`(,file . ,scope) (pos-corpus-entries pos-startup--corpus))
      (when (memq (pos-scope-kind scope) '(project responsibility root))
        (with-current-buffer (pos-visit file)
          (org-map-entries
           (lambda ()
             (when-let* (((member pos-startup-review-tag (org-get-tags nil t)))
                         ((org-entry-is-todo-p))
                         (time (org-get-scheduled-time (point)))
                         (date (format-time-string "%Y-%m-%d" time)))
               (let ((paths (list (pos-scope-path scope))))
                 (dolist (name (org-entry-get-multivalued-property
                                nil pos-startup-review-covers-property))
                   (let ((named (pos-startup--scope-named name file))
                         (title (org-link-display-format
                                 (substring-no-properties
                                  (org-get-heading t t t t)))))
                     (if named
                         (push (pos-scope-path named) paths)
                       (push `(,(format "%-54s %s  %s %s" (pos-startup--label)
                                        name (org-get-todo-state) title)
                               ,scope
                               (covers . ,name)
                               (file . ,(file-relative-name file pos-startup--root))
                               (line . ,(line-number-at-pos))
                               (title . ,title))
                             unknown))))
                 (dolist (path paths)
                   (let ((had (assoc path covered)))
                     (cond ((not had) (push (cons path date) covered))
                           ((string< date (cdr had)) (setcdr had date))))))))
           nil 'file))))
    (cons covered (nreverse unknown))))

(defun pos-startup--responsibilities ()
  "Return the path of each responsibility of `pos-startup--corpus', sorted."
  (sort (mapcar #'pos-scope-path
                (pos-corpus-scopes-of-kind pos-startup--corpus 'responsibility))
        #'string<))

;;;; Items

(defun pos-startup--scope-row (scope &optional status more)
  "Gather SCOPE, a `pos-scope', as a line of a view of scopes.
STATUS is its project file's STATUS, or nil.  MORE is an alist of
what else the view says of the scope."
  (when pos-startup--gathering
    (push `((scope . ,(pos-scope-path scope))
            (scope_kind . ,(symbol-name (pos-scope-kind scope)))
            (status . ,(or status :null))
            ,@more)
          pos-startup--items)))

(defun pos-startup--item-at-point ()
  "Return the heading at point as data.
An alist for `json-serialize': the scope and its kind, the file
relative to the root, the heading's line, its ID, state, title and
tags, its SCHEDULED, DEADLINE and CLOSED as written, the properties
of its drawer by name, named in capitals, and its body, the text of
the entry itself without its planning line and drawers.  What an
item lacks is :null."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((file (buffer-file-name))
          (scope (pos-corpus-owner pos-startup--corpus file))
          (end (save-excursion (outline-next-heading) (point)))
          (text (lambda (value)
                  (if value (substring-no-properties value) :null))))
     `((scope . ,(pos-scope-path scope))
       (scope_kind . ,(symbol-name (pos-scope-kind scope)))
       (file . ,(file-relative-name file pos-startup--root))
       (line . ,(line-number-at-pos))
       (id . ,(funcall text (org-entry-get nil "ID")))
       (state . ,(funcall text (org-get-todo-state)))
       (title . ,(funcall text (org-get-heading t t t t)))
       (tags . ,(vconcat (mapcar #'substring-no-properties
                                 (org-get-tags nil t))))
       (scheduled . ,(funcall text (org-entry-get nil "SCHEDULED")))
       (deadline . ,(funcall text (org-entry-get nil "DEADLINE")))
       (closed . ,(funcall text (org-entry-get nil "CLOSED")))
       ;; Org adds CATEGORY, which it works out, to what the drawer has.
       (properties . ,(mapcar (lambda (property)
                                (cons (intern (car property))
                                      (substring-no-properties (cdr property))))
                              (sort (assoc-delete-all
                                     "CATEGORY"
                                     (org-entry-properties nil 'standard))
                                    (lambda (a b) (string< (car a) (car b))))))
       (body . ,(save-excursion
                  (org-end-of-meta-data t)
                  (string-trim (buffer-substring-no-properties
                                (min (point) end) end))))))))

(defun pos-startup--gather-agenda ()
  "Gather the heading of each line of the agenda in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((marker (get-text-property (point) 'org-hd-marker)))
        (when marker
          (push (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (pos-startup--item-at-point)))
                pos-startup--items)))
      (forward-line 1))))

;;;; Views

(defun pos-startup--agenda (function)
  "Return what the agenda command FUNCTION displays, as text.
A link is given as its description.  While `pos-startup--gathering',
gather the heading of each line too."
  (let ((buffer (generate-new-buffer " *pos-startup*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (org-mode))
          (let ((org-agenda-buffer-name (buffer-name buffer))
                (org-agenda-sticky nil)
                (org-agenda-window-setup 'current-window))
            (funcall function)
            (with-current-buffer (get-buffer org-agenda-buffer-name)
              (when pos-startup--gathering (pos-startup--gather-agenda))
              (org-link-display-format
               (buffer-substring-no-properties (point-min) (point-max))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (get-buffer "*Org Agenda*") (kill-buffer "*Org Agenda*")))))

(defun pos-startup--or-none (text)
  "Return TEXT, a title and its lines, saying so if it has no lines."
  (if (string-match-p "^  " text) text (concat text "  (none)\n")))

(defun pos-startup--list (title lines)
  "Return TITLE and LINES as text, or TITLE and a note that there are none."
  (pos-startup--or-none
   (concat title "\n" (mapconcat (lambda (line) (concat "  " line "\n")) lines ""))))

(defun pos-startup--next ()
  "Return the next view of `pos-startup--corpus'.
Each NEXT item: those of projects, of responsibilities and of the
root apart.  An item of a scope yet to be configured, which is of no
kind, is in a fourth list, printed only when there is such an item."
  (mapconcat
   (lambda (pair)
     (let* ((org-agenda-files (pos-startup--files-of-kind (car pair)))
            (title (format "NEXT items of %s" (cdr pair)))
            (org-agenda-overriding-header title))
       (pos-startup--or-none
        (if org-agenda-files
            (pos-startup--agenda (lambda () (org-todo-list "NEXT")))
          (concat title "\n")))))
   (append '((project . "projects") (responsibility . "responsibilities")
             (root . "the root"))
           (when (seq-some #'pos-startup--has-next-p
                           (pos-startup--files-of-kind nil))
             '((nil . "scopes yet to be configured"))))
   "\n"))

(defun pos-startup--reviews ()
  "Return the reviews view of `pos-startup--corpus'.
Headings tagged `pos-startup-review-tag' that are late or scheduled
within `pos-startup-review-days': projects', responsibilities' and
the root's apart.  The root's reviews are of the whole tree."
  (mapconcat
   (lambda (pair)
     (let* ((org-agenda-files (pos-startup--files-of-kind (car pair)))
            (title (format "%s reviews, late or due in the next %d days"
                           (cdr pair) pos-startup-review-days))
            (org-agenda-overriding-header title)
            (org-agenda-entry-types '(:scheduled))
            (org-agenda-remove-tags t)
            (org-agenda-skip-function
             `(org-agenda-skip-entry-if
               'notregexp ,(format ":%s:" (regexp-quote pos-startup-review-tag))))
            (org-agenda-scheduled-leaders '("Scheduled:    " "%2d days late: ")))
       (pos-startup--or-none
        (if org-agenda-files
            (pos-startup--agenda
             (lambda () (org-agenda-list nil nil pos-startup-review-days)))
          (concat title "\n")))))
   '((project . "Project") (responsibility . "Responsibility") (root . "Root"))
   "\n"))

(defun pos-startup--reviews-to-schedule ()
  "Return the view of the scopes of `pos-startup--corpus' with no review scheduled.
Active projects, by `pos-startup-active-statuses', responsibilities,
and the root, named \".\" as its configuration names it: each that no
review covers, by `pos-startup--reviews-of-scopes'.  A fourth list,
printed only when it has a line, gives each path a review names that
is no scope's."
  (let* ((reviews (pos-startup--reviews-of-scopes))
         (reviewed (mapcar #'car (car reviews)))
         projects)
    (dolist (file (pos-startup--files-of-kind 'project))
      (let* ((status (pos-startup--file-status file))
             (owner (pos-corpus-owner pos-startup--corpus file))
             (scope (pos-scope-path owner)))
        (when (and (member status pos-startup-active-statuses)
                   (not (member scope reviewed)))
          (pos-startup--scope-row owner status)
          (push (format "%-54s %s" scope status) projects))))
    (let ((responsibilities
           (seq-remove (lambda (scope) (member scope reviewed))
                       (pos-startup--responsibilities))))
      (dolist (kind '(responsibility root))
        (dolist (scope (sort (pos-corpus-scopes-of-kind pos-startup--corpus kind)
                             (lambda (a b)
                               (string< (pos-scope-path a) (pos-scope-path b)))))
          (unless (member (pos-scope-path scope) reviewed)
            (pos-startup--scope-row scope))))
      (concat
       (pos-startup--list "Projects with a review to be scheduled"
                          (nreverse projects))
       "\n"
       (pos-startup--list "Responsibilities with a review to be scheduled"
                          responsibilities)
       "\n"
       (pos-startup--list "Root with a review to be scheduled"
                          (unless (member "." reviewed) '(".")))
       (when (cdr reviews)
         (concat
          "\n"
          (pos-startup--list
           "Reviews that name a path that is no scope's"
           (mapcar (pcase-lambda (`(,line ,scope . ,more))
                     (pos-startup--scope-row scope nil more)
                     line)
                   (cdr reviews)))))))))

(defun pos-startup--has-next-p (file)
  "Return non-nil if FILE has a NEXT item."
  (let ((pos-startup-next-action-states '("NEXT")))
    (pos-startup--has-next-action-p file)))

(defun pos-startup--has-next-action-p (file)
  "Return non-nil if FILE has an item that is a next action.
Its state is one of `pos-startup-next-action-states'."
  (with-current-buffer (pos-visit file)
    (catch 'found
      (org-map-entries
       (lambda ()
         (when (member (org-get-todo-state) pos-startup-next-action-states)
           (throw 'found t)))
       nil 'file)
      nil)))

(defun pos-startup--stuck ()
  "Return the view of the projects of `pos-startup--corpus' with no next action.
Begun projects, by `pos-startup-stuck-statuses', none of whose files
has an item in one of `pos-startup-next-action-states'."
  (let (projects)
    (dolist (file (pos-startup--files-of-kind 'project))
      (let* ((owner (pos-corpus-owner pos-startup--corpus file))
             (path (pos-scope-path owner))
             (project (or (assoc path projects)
                          (car (push (list path nil nil owner) projects)))))
        (setf (nth 1 project) (or (nth 1 project) (pos-startup--file-status file)))
        (setf (nth 2 project) (or (nth 2 project)
                                  (pos-startup--has-next-action-p file)))))
    (pos-startup--list
     "Projects with no next action"
     (mapcar (lambda (project)
               (pos-startup--scope-row (nth 3 project) (nth 1 project))
               (format "%-54s %s" (nth 0 project) (nth 1 project)))
             (sort (seq-filter (lambda (project)
                                 (and (member (nth 1 project) pos-startup-stuck-statuses)
                                      (not (nth 2 project))))
                               projects)
                   (lambda (a b) (string< (car a) (car b))))))))

(defun pos-startup--outcome ()
  "Return the first paragraph under the current buffer's Outcome heading.
The heading is a top-level one titled Outcome.  One line, or nil if
there is no such heading or no text under it."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (re-search-forward "^\\* Outcome[ \t]*$" nil t)
     (let ((end (save-excursion (outline-next-heading) (point))))
       (org-end-of-meta-data t)
       (when (< (point) end)
         (let ((paragraph (car (split-string
                                (buffer-substring-no-properties (point) end)
                                "\n[ \t]*\n" t))))
           (and paragraph
                (org-link-display-format
                 (string-join (split-string paragraph "[ \t\n]+" t) " ")))))))))

(defun pos-startup--project-facts (file)
  "Return (STATUS OUTCOME NEXT REVIEW) as FILE, a project's file, has them.
STATUS is the file's; OUTCOME is `pos-startup--outcome'; NEXT is the
title of its first item in one of `pos-startup-next-action-states';
REVIEW is the earliest date, as YYYY-MM-DD, an open heading tagged
`pos-startup-review-tag' is scheduled on.  Each is nil if FILE has
none."
  (let ((status (pos-startup--file-status file))
        next review)
    (with-current-buffer (pos-visit file)
      (org-map-entries
       (lambda ()
         (when (and (not next)
                    (member (org-get-todo-state) pos-startup-next-action-states))
           (setq next (org-link-display-format
                       (substring-no-properties (org-get-heading t t t t)))))
         (when (and (member pos-startup-review-tag (org-get-tags nil t))
                    (org-entry-is-todo-p))
           (let ((time (org-get-scheduled-time (point))))
             (when time
               (let ((date (format-time-string "%Y-%m-%d" time)))
                 (when (or (not review) (string< date review))
                   (setq review date)))))))
       nil 'file)
      (list status (pos-startup--outcome) next review))))

(defun pos-startup--projects ()
  "Return the projects view of `pos-startup--corpus'.
Each active project, by `pos-startup-active-statuses': its STATUS,
the date of its next review, its next action and its outcome, each
read from the project's files and none written anywhere else, but
that a review elsewhere that names the project is a review of it,
by `pos-startup--reviews-of-scopes'.  What a project lacks is shown
as a dash."
  (let ((covered (car (pos-startup--reviews-of-scopes)))
        projects)
    (dolist (file (pos-startup--files-of-kind 'project))
      (let* ((owner (pos-corpus-owner pos-startup--corpus file))
             (path (pos-scope-path owner))
             (project (or (assoc path projects)
                          (car (push (list path owner nil nil nil nil) projects))))
             (facts (pos-startup--project-facts file)))
        (dotimes (i 4)
          (let ((fact (nth i facts)) (had (nth (+ 2 i) project)))
            (setf (nth (+ 2 i) project)
                  ;; The earliest review of the project's files.
                  (if (and (= i 3) fact had)
                      (if (string< fact had) fact had)
                    (or had fact)))))))
    (pos-startup--list
     "Projects"
     (mapcan
      (lambda (project)
        (pcase-let* ((`(,path ,owner ,status ,outcome ,next ,own) project)
                     (named (cdr (assoc path covered)))
                     (review (if (and own named (string< own named)) own
                               (or named own))))
          (pos-startup--scope-row
           owner status
           `((review . ,(or review :null)) (next . ,(or next :null))
             (outcome . ,(or outcome :null))))
          (list (format "%-54s %-9s review %-10s" path status (or review "-"))
                (format "  next: %s" (or next "-"))
                (format "  outcome: %s" (or outcome "-")))))
      (sort (seq-filter (lambda (project)
                          (member (nth 2 project) pos-startup-active-statuses))
                        projects)
            (lambda (a b) (string< (car a) (car b))))))))

(defun pos-startup--intray ()
  "Return the intray view of `pos-startup--corpus'.
Each open item under Unsorted in a file named intray.org: what has
been captured and not yet placed."
  (let (lines)
    (dolist (file (pos-corpus-files pos-startup--corpus))
      (when (string= "intray.org" (file-name-nondirectory file))
        (with-current-buffer (pos-visit file)
          (org-map-entries
           (lambda ()
             (when (and (org-entry-is-todo-p)
                        (string= "Unsorted" (car (org-get-outline-path))))
               (when pos-startup--gathering
                 (push (pos-startup--item-at-point) pos-startup--items))
               (push (format "%-54s %s %s" (pos-startup--label)
                             (org-get-todo-state)
                             (org-link-display-format
                              (org-get-heading t t t t)))
                     lines)))
           nil 'file))))
    (pos-startup--list "Intray, to be placed" (nreverse lines))))

(defun pos-startup--finished ()
  "Return the finished view of `pos-startup--corpus'.
Each item in a done state whose CLOSED date is today or within the
`pos-startup-finished-days' days before it, with that date.  An item
with no CLOSED date is not listed."
  (let ((since (- (org-today) pos-startup-finished-days))
        lines)
    (dolist (file (pos-corpus-files pos-startup--corpus))
      (with-current-buffer (pos-visit file)
        (org-map-entries
         (lambda ()
           (let ((closed (org-entry-get nil "CLOSED")))
             (when (and closed (org-entry-is-done-p)
                        (>= (org-time-string-to-absolute closed) since))
               (when pos-startup--gathering
                 (push (pos-startup--item-at-point) pos-startup--items))
               (push (format "%-54s %s %s %s" (pos-startup--label)
                             (substring closed 1 11)
                             (org-get-todo-state)
                             (org-link-display-format
                              (org-get-heading t t t t)))
                     lines))))
         nil 'file)))
    (pos-startup--list (format "Finished in the last %d days"
                               pos-startup-finished-days)
                       (nreverse lines))))

(defun pos-startup-view (root view)
  "Return VIEW, one of `pos-startup-views', of the Org files under ROOT.
Text: a title, then one line for each item, labelled by its scope.
A view of more than one list has a title for each."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (pos-startup--root root)
         (pos-startup--corpus (pos-startup--corpus root))
         (org-agenda-files (pos-corpus-files pos-startup--corpus))
         (org-todo-keywords pos-todo-keywords)
         ;; Label each line by its scope: the default is the file's
         ;; name, and most are project.org.
         (org-agenda-prefix-format
          '((agenda . "  %-54(pos-startup--label) %?-12t% s")
            (todo . "  %-54(pos-startup--label) ")))
         (org-agenda-include-diary nil)
         ;; A done item is not listed, whatever date it still carries.
         (org-agenda-skip-scheduled-if-done t)
         (org-agenda-skip-deadline-if-done t)
         (org-agenda-skip-timestamp-if-done t)
         ;; A date line only for a day that has an item.
         (org-agenda-show-all-dates nil)
         ;; From today, not from the week's Monday.
         (org-agenda-start-on-weekday nil)
         (org-agenda-inhibit-startup t)
         ;; Opening a file otherwise runs git to find its state.
         (vc-handled-backends nil)
         (org-element-cache-persistent nil))
    (pcase view
      ("next" (pos-startup--next))
      ("waiting"
       (let ((org-agenda-overriding-header "WAITING items"))
         (pos-startup--or-none
          (pos-startup--agenda (lambda () (org-todo-list "WAITING"))))))
      ("someday"
       ;; Not committed to; read when a review asks what might be taken up.
       (let ((org-agenda-overriding-header "SOMEDAY items"))
         (pos-startup--or-none
          (pos-startup--agenda (lambda () (org-todo-list "SOMEDAY"))))))
      ("scheduled"
       ;; Deadlines and reviews have views of their own.
       (let ((org-agenda-overriding-header
              (format "Scheduled items, next %d days" pos-startup-scheduled-days))
             (org-agenda-entry-types '(:scheduled :timestamp :sexp))
             (org-agenda-skip-function
              `(org-agenda-skip-entry-if
                'regexp ,(format ":%s:" (regexp-quote pos-startup-review-tag))))
             (org-agenda-scheduled-leaders '("Scheduled:    " "%2d days late: ")))
         (pos-startup--or-none
          (pos-startup--agenda
           (lambda () (org-agenda-list nil nil pos-startup-scheduled-days))))))
      ("deadlines"
       ;; Every open deadline, once, under today: a one-day view whose
       ;; warning period reaches any date.  A negative period overrides
       ;; an item's own, such as -3d, which would otherwise hide it.
       (let ((org-agenda-overriding-header "Deadlines, all open")
             (org-agenda-entry-types '(:deadline))
             (org-deadline-warning-days -36500)
             (org-agenda-deadline-leaders
              '("Due today:        " "Due in %3d days:  " "%3d days overdue: ")))
         (pos-startup--or-none
          (pos-startup--agenda (lambda () (org-agenda-list nil nil 1))))))
      ("reviews" (pos-startup--reviews))
      ("reviews-to-schedule" (pos-startup--reviews-to-schedule))
      ("stuck" (pos-startup--stuck))
      ("projects" (pos-startup--projects))
      ("intray" (pos-startup--intray))
      ("finished" (pos-startup--finished))
      ("all"
       (let ((org-agenda-overriding-header "All TODO items"))
         (pos-startup--or-none (pos-startup--agenda #'org-todo-list))))
      (_ (user-error "Unknown view: %s (one of %s)"
                     view (string-join pos-startup-views ", "))))))

(defun pos-startup-view-items (root view)
  "Return what VIEW of the Org files under ROOT lists, as data.
A list with an element for each line `pos-startup-view' prints, in
its order.  In a view of items each is an alist as
`pos-startup--item-at-point' gives it.  In reviews-to-schedule,
stuck and projects, which list scopes, each has scope, scope_kind
and status; in projects, review, next and outcome too.  A line of
reviews-to-schedule for a path that is no scope's has the scope of
the review's file, and covers, file, line and title."
  (let ((pos-startup--gathering t)
        (pos-startup--items nil))
    (pos-startup-view root view)
    (nreverse pos-startup--items)))

(defun pos-startup--table (rows)
  "Return ROWS, lists of strings, as lines with their columns aligned."
  (let ((widths nil))
    (dolist (row rows)
      (seq-map-indexed (lambda (cell i)
                         (setf (alist-get i widths)
                               (max (or (alist-get i widths) 0) (length cell))))
                       row))
    (mapconcat (lambda (row)
                 (concat "  "
                         (string-trim-right
                          (mapconcat #'identity
                                     (seq-map-indexed
                                      (lambda (cell i) (string-pad cell (alist-get i widths)))
                                      row)
                                     "  "))
                         "\n"))
               rows "")))

(defun pos-startup--other-kinds ()
  "Return the section naming the kinds of canon methodologies declare, or \"\".
One line for each kind a project's methodology declares, as
doc/pos-methodology.txt has it: the project, the methodology, the kind,
where it is and where to start reading.  A methodology whose declaration
is refused is one line naming the refusal."
  (let (rows)
    (pcase-dolist (`(,scope ,name . ,dir)
                   (pos-corpus-methodologies pos-startup--corpus))
      (condition-case err
          (seq-doseq (kind (alist-get 'canon (or (pos-tree-read-methodology-file dir)
                                                 '((canon . [])))))
            (push (list (pos-scope-path scope) name (alist-get 'kind kind)
                        (alist-get 'at kind)
                        (concat "start at " (alist-get 'entrance kind)))
                  rows))
        (pos-tree-refused
         (push (list (pos-scope-path scope) name
                     (format "(refused: %s: %s)" (nth 1 err) (nth 2 err)))
               rows))))
    (if rows
        (concat "\nCanon of other kinds, not read:\n" (pos-startup--table (nreverse rows)))
      "")))

(defun pos-startup-report (root &optional views)
  "Return the opening questions, then VIEWS of ROOT's Org files, as text.
VIEWS defaults to `pos-startup-default-views'."
  (let ((views (or views pos-startup-default-views)))
    (dolist (view views)
      (unless (member view pos-startup-views)
        (user-error "Unknown view: %s (one of %s)"
                    view (string-join pos-startup-views ", "))))
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (pos-startup--corpus (pos-corpus root)))
      (concat pos-startup-prompts
              (format "Files read: %d\n" (length (pos-corpus-files pos-startup--corpus)))
              (mapconcat (lambda (finding)
                           (format "Not read: %s (%s)\n" (car finding) (cdr finding)))
                         (pos-corpus-findings pos-startup--corpus) "")
              (pos-startup--other-kinds)
              (mapconcat (lambda (view) (concat "\n" (pos-startup-view root view)))
                         views "")))))

(defun pos-startup-data (root &optional views)
  "Return VIEWS of ROOT's Org files as data, for `json-serialize'.
VIEWS defaults to `pos-startup-default-views'.  An alist: files_read,
the count of files; not_read, a vector with path and reason for each
configuration refused; views, each view's name with the vector of
what `pos-startup-view-items' gives for it.  A view named twice is
given once."
  (let ((views (or views pos-startup-default-views)))
    (dolist (view views)
      (unless (member view pos-startup-views)
        (user-error "Unknown view: %s (one of %s)"
                    view (string-join pos-startup-views ", "))))
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (pos-startup--corpus (pos-corpus root)))
      `((files_read . ,(length (pos-corpus-files pos-startup--corpus)))
        (not_read . ,(vconcat
                      (mapcar (lambda (finding)
                                `((path . ,(format "%s" (car finding)))
                                  (reason . ,(format "%s" (cdr finding)))))
                              (pos-corpus-findings pos-startup--corpus))))
        (views . ,(mapcar (lambda (view)
                            (cons (intern view)
                                  (vconcat (pos-startup-view-items root view))))
                          (seq-uniq views)))))))

(defun pos-startup-batch ()
  "Print the start-up report of `pos-directory'.
Each --view NAME in `command-line-args-left' names a view to print in
place of the default ones, and --weekly names those of
`pos-startup-weekly-views'.  With --json, print `pos-startup-data' as
JSON and a newline in place of the text.  Exit 2 on any other
argument."
  (let (views json)
    (while command-line-args-left
      (let ((argument (pop command-line-args-left)))
        (cond ((equal argument "--json") (setq json t))
              ((equal argument "--weekly")
               (setq views (append (reverse pos-startup-weekly-views) views)))
              ((and (equal argument "--view") command-line-args-left
                    (member (car command-line-args-left) pos-startup-views))
               (push (pop command-line-args-left) views))
              (t (message "Usage: [--json] [--weekly] [--view %s] ..."
                          (string-join pos-startup-views "|"))
                 (kill-emacs 2)))))
    ;; Each session opens here: the index is kept current as a matter
    ;; of course, so that a later command finds it ready.
    (pos-roam-sync pos-directory)
    (if json
        (princ (concat (json-serialize
                        (pos-startup-data pos-directory (nreverse views)))
                       "\n"))
      (princ (pos-startup-report pos-directory (nreverse views))))))

(provide 'pos-startup)
;;; pos-startup.el ends here
