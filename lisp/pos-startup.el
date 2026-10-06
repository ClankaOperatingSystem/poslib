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
;; The files are every Org file under the root outside archives, attics
;; and directories that are hidden or begin with an underscore.  A file
;; belongs to the deepest scope on its path, else the root.  A scope is
;; a responsibility, which is a directory whose configuration says
;; where its projects belong, as doc/pos-directory.txt has it, or a
;; directory within one named responsibilities; or a project, which is
;; what is directly within a directory named projects.
;;
;; - `pos-startup-report': the prompts and the views, as text.
;; - `pos-startup-view': one view, as text.
;; - `pos-startup-batch': shell entry; --view NAME, repeated, picks views.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'seq)
(require 'pos)
(require 'pos-tree)

(defcustom pos-startup-excluded-directories '("archives" "attic")
  "Names of directories whose Org files are not read.
Hidden directories and those beginning with an underscore are never
read."
  :type '(repeat string)
  :group 'org-archive)

(defcustom pos-startup-scheduled-days 14
  "Days, from today, that the scheduled view covers."
  :type 'integer
  :group 'org-archive)

(defcustom pos-startup-review-days 7
  "Days, from today, that the reviews view covers."
  :type 'integer
  :group 'org-archive)

(defcustom pos-startup-review-tag "review"
  "The tag of a heading that schedules a review of its scope."
  :type 'string
  :group 'org-archive)

(defcustom pos-startup-active-statuses '("COMMITTED" "WIP")
  "Values of a project file's STATUS property that make it active.
An active project is expected to have a review scheduled."
  :type '(repeat string)
  :group 'org-archive)

(defconst pos-startup-prompts
  "START-UP PROMPTS
Purpose: What are we here to do in this session?
Circumstances: What time, attention, energy, and fixed commitments constrain the choice?
Choice: Which established work, new concern, or clarification should begin?
Trust: Treat the views below as a bounded view of saved files, not a complete review.
"
  "The questions a session opens with.")

(defconst pos-startup-views
  '("next" "scheduled" "deadlines" "reviews" "reviews-to-schedule" "intray"
    "all")
  "The names of the views, in the order they are printed.")

(defconst pos-startup-default-views
  '("next" "scheduled" "deadlines" "reviews" "reviews-to-schedule" "intray")
  "The views `pos-startup-report' prints when none is named.")

(defvar pos-startup--root nil
  "The root being read, while a view is made.")

(defvar pos-startup--configured nil
  "The configured responsibilities found, as (ROOT . PATHS), or nil.
Bound while a view is made, so that the tree is walked once for it.")

;;;; Files and scopes

(defun pos-startup--excluded-p (directory)
  "Return non-nil if the Org files in DIRECTORY are not read."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (or (member name pos-startup-excluded-directories)
        (string-prefix-p "_" name)
        (string-prefix-p "." name))))

(defun pos-startup-files (root)
  "Return the Org files read under ROOT, sorted.
Excluded directories are not entered."
  (sort (directory-files-recursively
         root "\\`[^.#].*\\.org\\'" nil
         (lambda (directory) (not (pos-startup--excluded-p directory)))
         t)
        #'string<))

(defun pos-startup--directories (root)
  "Return the directories under ROOT whose Org files are read."
  (when (file-directory-p root)
    (seq-filter (lambda (entry)
                  (and (file-directory-p entry)
                       (not (pos-startup--excluded-p entry))))
                (directory-files-recursively
                 root "" t
                 (lambda (directory) (not (pos-startup--excluded-p directory)))
                 t))))

(defun pos-startup--configured (root)
  "Return the path of each configured responsibility beneath ROOT.
A directory whose configuration says where its projects belong, as
doc/pos-directory.txt has it.  Relative to ROOT, which is not one of
them, sorted.  A configuration that is refused makes nothing a
responsibility."
  (if (equal (car pos-startup--configured) root)
      (cdr pos-startup--configured)
    (let (found)
      (dolist (directory (pos-startup--directories root))
        (when (equal "responsibility"
                     (ignore-errors
                       (when-let* ((file (pos-tree-config-file directory)))
                         (alist-get
                          'kind
                          (pos-tree-read-config
                           (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name file directory))
                             (buffer-string)))))))
          (push (file-relative-name directory root) found)))
      (setq found (sort found #'string<))
      ;; Kept for the length of a view only: the tree may change after.
      (when pos-startup--root
        (setq pos-startup--configured (cons root found)))
      found)))

(defun pos-startup--owner (file root)
  "Return (KIND . PATH) for the scope under ROOT that FILE belongs to.
KIND is the symbol project or responsibility, and PATH is relative to
ROOT.  The scope is the deepest on FILE's path: a configured
responsibility, or what a directory named projects or responsibilities
holds.  Nil for a file that belongs to ROOT itself."
  (let* ((parts (split-string (file-relative-name file root) "/" t))
         (configured (pos-startup--configured root))
         (owner nil) (index 0))
    (while (< (1+ index) (length parts))
      (let ((here (mapconcat #'identity (seq-take parts (1+ index)) "/"))
            (kind (pcase (nth index parts)
                    ("projects" 'project)
                    ("responsibilities" 'responsibility))))
        (when (member here configured)
          (setq owner (cons 'responsibility here)))
        (when kind
          (setq owner (cons kind (mapconcat #'identity
                                            (seq-take parts (+ index 2)) "/")))))
      (setq index (1+ index)))
    (when (and owner (string-suffix-p ".org" (cdr owner)))
      (setcdr owner (file-name-sans-extension (cdr owner))))
    owner))

(defun pos-startup--files-of-kind (kind root)
  "Return the files under ROOT that belong to a scope of KIND."
  (seq-filter (lambda (file) (eq (car (pos-startup--owner file root)) kind))
              (pos-startup-files root)))

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
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((end (save-excursion
                  (or (re-search-forward "^\\*+ " nil t) (point-max)))))
       (when (re-search-forward "^:STATUS:[ \t]+\\(\\S-+\\)" end t)
         (match-string-no-properties 1))))))

(defun pos-startup--reviewed-scopes (root)
  "Return the paths of the scopes under ROOT with a review scheduled.
That is, holding an open, scheduled heading tagged
`pos-startup-review-tag'."
  (let (scopes)
    (dolist (file (pos-startup-files root))
      (let ((owner (pos-startup--owner file root)))
        (when owner
          (with-current-buffer (find-file-noselect file)
            (org-map-entries
             (lambda ()
               (when (and (member pos-startup-review-tag (org-get-tags nil t))
                          (org-entry-is-todo-p)
                          (org-get-scheduled-time (point)))
                 (cl-pushnew (cdr owner) scopes :test #'string=)))
             nil 'file)))))
    scopes))

(defun pos-startup--responsibilities (root)
  "Return the path of each responsibility beneath ROOT.
Each configured one, and each directory directly within a
responsibilities/.  Relative to ROOT, sorted."
  (let ((found (copy-sequence (pos-startup--configured root))))
    (dolist (entry (pos-startup--directories root))
      (when (string= "responsibilities"
                     (file-name-nondirectory
                      (directory-file-name (file-name-directory entry))))
        (cl-pushnew (file-relative-name entry root) found :test #'string=)))
    (sort found #'string<)))

;;;; Views

(defun pos-startup--agenda (function)
  "Return what the agenda command FUNCTION displays, as text.
A link is given as its description."
  (let ((buffer (generate-new-buffer " *pos-startup*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (org-mode))
          (let ((org-agenda-buffer-name (buffer-name buffer))
                (org-agenda-sticky nil)
                (org-agenda-window-setup 'current-window))
            (funcall function)
            (with-current-buffer (get-buffer org-agenda-buffer-name)
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

(defun pos-startup--reviews (root)
  "Return the reviews view of ROOT.
Headings tagged `pos-startup-review-tag' that are late or scheduled
within `pos-startup-review-days', projects' and responsibilities'
apart."
  (mapconcat
   (lambda (pair)
     (let* ((org-agenda-files (pos-startup--files-of-kind (car pair) root))
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
   '((project . "Project") (responsibility . "Responsibility"))
   "\n"))

(defun pos-startup--reviews-to-schedule (root)
  "Return the view of ROOT's scopes that have no review scheduled.
Active projects, by `pos-startup-active-statuses', and responsibilities."
  (let ((reviewed (pos-startup--reviewed-scopes root))
        projects)
    (dolist (file (pos-startup--files-of-kind 'project root))
      (let ((status (pos-startup--file-status file))
            (scope (cdr (pos-startup--owner file root))))
        (when (and (member status pos-startup-active-statuses)
                   (not (member scope reviewed)))
          (push (format "%-54s %s" scope status) projects))))
    (concat
     (pos-startup--list "Projects with a review to be scheduled"
                        (nreverse projects))
     "\n"
     (pos-startup--list "Responsibilities with a review to be scheduled"
                        (seq-remove (lambda (scope) (member scope reviewed))
                                    (pos-startup--responsibilities root))))))

(defun pos-startup--intray (root)
  "Return the intray view of ROOT.
Each open item under Unsorted in a file named intray.org: what has
been captured and not yet placed."
  (let (lines)
    (dolist (file (pos-startup-files root))
      (when (string= "intray.org" (file-name-nondirectory file))
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when (and (org-entry-is-todo-p)
                        (string= "Unsorted" (car (org-get-outline-path))))
               (push (format "%-54s %s %s" (pos-startup--label)
                             (org-get-todo-state)
                             (org-link-display-format
                              (org-get-heading t t t t)))
                     lines)))
           nil 'file))))
    (pos-startup--list "Intray, to be placed" (nreverse lines))))

(defun pos-startup-view (root view)
  "Return VIEW, one of `pos-startup-views', of the Org files under ROOT.
Text: a title, then one line for each item, labelled by its scope."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (pos-startup--root root)
         (pos-startup--configured nil)
         (org-agenda-files (pos-startup-files root))
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
      ("next"
       (let ((org-agenda-overriding-header "NEXT items"))
         (pos-startup--or-none
          (pos-startup--agenda (lambda () (org-todo-list "NEXT"))))))
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
      ("reviews" (pos-startup--reviews root))
      ("reviews-to-schedule" (pos-startup--reviews-to-schedule root))
      ("intray" (pos-startup--intray root))
      ("all"
       (let ((org-agenda-overriding-header "All TODO items"))
         (pos-startup--or-none (pos-startup--agenda #'org-todo-list))))
      (_ (user-error "Unknown view: %s (one of %s)"
                     view (string-join pos-startup-views ", "))))))

(defun pos-startup-report (root &optional views)
  "Return the opening questions, then VIEWS of ROOT's Org files, as text.
VIEWS defaults to `pos-startup-default-views'."
  (let ((views (or views pos-startup-default-views)))
    (dolist (view views)
      (unless (member view pos-startup-views)
        (user-error "Unknown view: %s (one of %s)"
                    view (string-join pos-startup-views ", "))))
    (concat pos-startup-prompts
            (format "Files read: %d\n" (length (pos-startup-files root)))
            (mapconcat (lambda (view) (concat "\n" (pos-startup-view root view)))
                       views ""))))

(defun pos-startup-batch ()
  "Print the start-up report of `pos-directory'.
Each --view NAME in `command-line-args-left' names a view to print in
place of the default ones.  Exit 2 on any other argument."
  (let (views)
    (while command-line-args-left
      (let ((argument (pop command-line-args-left)))
        (if (and (equal argument "--view") command-line-args-left
                 (member (car command-line-args-left) pos-startup-views))
            (push (pop command-line-args-left) views)
          (message "Usage: [--view %s] ..." (string-join pos-startup-views "|"))
          (kill-emacs 2))))
    (princ (pos-startup-report pos-directory (nreverse views)))))

(provide 'pos-startup)
;;; pos-startup.el ends here
