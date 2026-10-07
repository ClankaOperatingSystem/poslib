;;; pos.el --- Personal OS tools for an Org repository  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Chris Gough

;; Author: Chris Gough
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.6") (yaml "1.2.4") (org-roam "2.3.1"))
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

;; - Weekly sweep: done items to dated archives.
;; - Lint: stranded, duplicate, malformed tasks.
;; - Plans: refile the intray, resolve duplicates.
;; - Same from M-x and batch Emacs.

;;; Code:

;;;; Weeks

(defun pos-week-name (time)
  "Return the ISO 8601 week name of TIME, e.g. \"2026-W36\"."
  (format-time-string "%G-W%V" time))

(defconst pos-sweep-weekday 0
  "Sweep weekday; 0 is Sunday.")

(defconst pos-sweep-hour 23
  "Sweep hour, local time.")

(defun pos-sweep-boundary (now)
  "Return the latest sweep boundary at or before NOW."
  (let* ((d (decode-time now))
         (days-since (mod (- (decoded-time-weekday d) pos-sweep-weekday) 7))
         ;; Boundary day, before the hour: last week's.
         (too-early (and (= days-since 0)
                         (< (decoded-time-hour d) pos-sweep-hour))))
    (encode-time
     (list 0 0 pos-sweep-hour
           (- (decoded-time-day d) days-since (if too-early 7 0))
           (decoded-time-month d)
           (decoded-time-year d)
           nil -1 nil))))

(defgroup pos nil
  "A personal operating system in Org files."
  :group 'org
  :prefix "pos-")

;;;; Files and paths

(defcustom pos-pillars nil
  "Root subdirectories, one per area."
  :type '(repeat string)
  :group 'pos)

(defun pos-org-files (root)
  "Return the Org files the sweep covers under ROOT.
Top level of ROOT and of each of `pos-pillars'; missing ones skipped."
  (let ((dirs (cons root
                    (mapcar (lambda (p) (expand-file-name p root))
                            pos-pillars))))
    (mapcan (lambda (dir)
              (when (file-directory-p dir)
                (directory-files dir t "\\.org\\'")))
            dirs)))

(defcustom pos-archive-directory "archive/orgmode"
  "Archive directory, relative to the root; one subdirectory per week."
  :type 'string
  :group 'pos)

(defun pos-archive-file (root file week)
  "Return the WEEK archive for FILE under ROOT.
Keeps FILE's path relative to ROOT; suffix _archive."
  (expand-file-name
   (concat (file-relative-name file root) "_archive")
   (expand-file-name week (expand-file-name pos-archive-directory root))))

;;;; Keywords

;; One sequence for every file; per-file #+TODO lines drift.
(defconst pos-todo-keywords
  '((sequence "BACKLOG" "TODO" "NEXT" "WIP" "BLOCKED"
              "|" "DONE" "CANCELLED"))
  "The TODO sequence, shaped as `org-todo-keywords'.")

(defun pos-done-keywords ()
  "Return the done keywords of `pos-todo-keywords'."
  (cdr (member "|" (cdr (car pos-todo-keywords)))))

;;;; Archiving

(require 'org)
(require 'org-archive)
(require 'seq)
(require 'cl-lib)
(require 'pos-roam)

(setq org-todo-keywords pos-todo-keywords)

(defun pos--open-descendant-p ()
  "Return non-nil if the entry at point has an open TODO below it."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil))
      (re-search-forward org-not-done-heading-regexp end t))))

(defun pos--fix-last-archived (archive-buffer relative-file time)
  "Stamp the last entry in ARCHIVE-BUFFER with RELATIVE-FILE and TIME.
Replaces the absolute path and wall clock, so any checkout writes
the same bytes."
  (with-current-buffer archive-buffer
    (save-excursion
      (goto-char (point-max))
      (org-back-to-heading t)
      (while (org-up-heading-safe))
      (org-entry-put (point) "ARCHIVE_FILE" relative-file)
      (org-entry-put (point) "ARCHIVE_TIME"
                     (format-time-string
                      (org-time-stamp-format 'with-time 'no-brackets)
                      time)))))

(defun pos-archive-done-in-file (root file week time)
  "Archive done entries in FILE under ROOT to WEEK's archive, stamped TIME.
Skip done entries with open children.
Return (:archived COUNT :skipped HEADINGS)."
  (let* ((archive-file (pos-archive-file root file week))
         (relative-file (file-relative-name file root))
         (archived 0)
         (skipped nil)
         (org-archive-location (concat archive-file "::"))
         (make-backup-files nil)
         (org-archive-file-header-format
          (format "\nArchived entries from file %s\n\n" relative-file)))
    (make-directory (file-name-directory archive-file) t)
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
         (when (org-entry-is-done-p)
           (if (pos--open-descendant-p)
               (push (org-get-heading t t t t) skipped)
             ;; Subtree gone; resume here.
             (setq org-map-continue-from (point))
             (org-archive-subtree)
             (pos--fix-last-archived
              (find-file-noselect archive-file) relative-file time)
             (setq archived (1+ archived)))))
       nil 'file)
      (save-buffer))
    ;; org-archive-subtree saves the archive only from the agenda.
    (when (> archived 0)
      (with-current-buffer (find-file-noselect archive-file)
        (save-buffer)))
    (list :archived archived :skipped (nreverse skipped))))

;;;; The sweep

;; Default: parent of lisp/.
(defcustom pos-directory
  (expand-file-name ".." (file-name-directory (or load-file-name
                                                  buffer-file-name
                                                  default-directory)))
  "Root of the repository the tools operate on."
  :type 'directory
  :group 'pos)

(defcustom pos-config-file "pos-config.el"
  "Per-repository configuration, relative to `pos-directory'."
  :type 'string
  :group 'pos)

(defun pos-load-config (&optional root)
  "Load the `pos-config-file' of ROOT, default `pos-directory', if present.
Return non-nil if loaded."
  (load (expand-file-name pos-config-file (or root pos-directory)) t t t))

(defun pos-report (week result)
  "Return a report of RESULT, a sweep of WEEK, naming skipped entries."
  (let ((skipped (plist-get result :skipped)))
    (concat
     (format "Sweep %s: archived %d, skipped %d"
             week (plist-get result :archived) (length skipped))
     (when skipped
       (concat "\n  skipped (done, but has open children):\n"
               (mapconcat (lambda (heading) (concat "    " heading))
                          skipped "\n"))))))

(defun pos-sweep (&optional week time)
  "Archive done entries in covered files to WEEK's archive, stamped TIME.
Both default to the latest sweep boundary.
Return (:archived COUNT :skipped HEADINGS)."
  (interactive)
  (let* ((time (or time (pos-sweep-boundary (current-time))))
         (week (or week (pos-week-name time)))
         (root (file-name-as-directory pos-directory))
         (archived 0)
         (skipped nil))
    (dolist (file (pos-org-files root))
      (let ((result (pos-archive-done-in-file root file week time)))
        (setq archived (+ archived (plist-get result :archived)))
        (setq skipped (append skipped (plist-get result :skipped)))
        (message "%s: archived %d, skipped %d"
                 (file-relative-name file root)
                 (plist-get result :archived)
                 (length (plist-get result :skipped)))))
    (let ((result (list :archived archived :skipped skipped)))
      (message "%s" (pos-report week result))
      result)))

;;;; Normalising keywords

(defun pos-normalise-keywords-in-file (file)
  "Remove #+TODO lines from FILE; respell CANCELED as CANCELLED.
Return (:lines-removed N :respelled N)."
  (let ((lines-removed 0)
        (respelled 0)
        (make-backup-files nil))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward "^#\\+\\(SEQ_\\|TYP_\\)?TODO:" nil t)
            (delete-region (line-beginning-position) (line-beginning-position 2))
            (setq lines-removed (1+ lines-removed))))
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward "^\\*+ \\(CANCELED\\)\\b" nil t)
            (replace-match "CANCELLED" t t nil 1)
            (setq respelled (1+ respelled)))))
      (when (buffer-modified-p)
        (save-buffer)))
    (list :lines-removed lines-removed :respelled respelled)))

(defun pos-normalise-keywords ()
  "Run `pos-normalise-keywords-in-file' on covered files; return totals."
  (interactive)
  (let ((lines-removed 0)
        (respelled 0))
    (dolist (file (pos-org-files (file-name-as-directory pos-directory)))
      (let ((result (pos-normalise-keywords-in-file file)))
        (setq lines-removed (+ lines-removed (plist-get result :lines-removed))
              respelled (+ respelled (plist-get result :respelled)))))
    (message "Normalised keywords: %d #+TODO lines removed, %d CANCELED respelled"
             lines-removed respelled)
    (list :lines-removed lines-removed :respelled respelled)))

;;;; Lint

(defun pos--map-headings (file function)
  "Call FUNCTION at each heading of FILE; collect non-nil results."
  (with-current-buffer (find-file-noselect file)
    (delq nil (org-map-entries function nil 'file))))

(defun pos--finding (file message)
  "Return a finding for the heading at point in FILE with MESSAGE."
  (list file (line-number-at-pos) message))

(defun pos-lint-done-with-open-children (file)
  "Report done entries in FILE with open children."
  (pos--map-headings
   file
   (lambda ()
     (when (and (org-entry-is-done-p) (pos--open-descendant-p))
       (pos--finding file "done entry has open children")))))

(defconst pos-lint-stale-keywords
  '("CANCELED" "CLARIFY" "DELEGATE" "DELEGATED" "WAITING" "SOMEDAY" "MAYBE")
  "Retired keywords; headings starting with one are plain text.")

(defun pos-lint-stale-keyword (file)
  "Report headings in FILE that start with a retired keyword."
  (pos--map-headings
   file
   (lambda ()
     (let ((word (car (split-string (org-get-heading t t t t)))))
       (when (member word pos-lint-stale-keywords)
         (pos--finding file (format "stale keyword %s (not in the sequence)" word)))))))

(defun pos-lint-todo-line (file)
  "Report #+TODO lines in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (findings nil))
        (while (re-search-forward "^#\\+\\(SEQ_\\|TYP_\\)?TODO:" nil t)
          (push (pos--finding file "#+TODO line overrides the one sequence; run pos-normalise-keywords")
                findings))
        (nreverse findings)))))

(defun pos-lint-merged-copy (file)
  "Report merged copies in FILE awaiting reconciliation."
  (pos--map-headings
   file
   (lambda ()
     (when (string-prefix-p "Merged copy from " (org-get-heading t t t t))
       (pos--finding file "merged copy awaiting reconciliation with its parent")))))

(defvar pos-lint-checks
  '(pos-lint-done-with-open-children
    pos-lint-stale-keyword
    pos-lint-todo-line
    pos-lint-merged-copy)
  "Per-file checks: file to findings.")

(defcustom pos-prose-directories nil
  "Root subdirectories of prose; not scanned for stranded tasks."
  :type '(repeat string)
  :group 'pos)

(defun pos-uncovered-org-files (root)
  "Return Org files under ROOT the sweep does not cover.
Excludes archive, prose and hidden directories."
  (let ((covered (pos-org-files root))
        (excluded (mapcar (lambda (dir)
                            (file-name-as-directory (expand-file-name dir root)))
                          (cons pos-archive-directory pos-prose-directories))))
    (seq-remove (lambda (file)
                  (seq-some (lambda (dir) (string-prefix-p dir file)) excluded))
                (seq-difference
                 (directory-files-recursively
                  root "\\.org\\'" nil
                  (lambda (dir)
                    (not (string-prefix-p "." (file-name-nondirectory dir)))))
                 covered))))

(defun pos-lint-stranded-tasks (root)
  "Report task keywords in Org files under ROOT outside the sweep."
  (mapcan (lambda (file)
            (pos--map-headings
             file
             (lambda ()
               (let ((keyword (org-get-todo-state)))
                 (when keyword
                   (pos--finding
                    file (format "task keyword outside the agenda files (%s)" keyword)))))))
          (pos-uncovered-org-files root)))

(defun pos--task-headings (root)
  "Return (KEY FILE LINE) for keyword headings in covered files under ROOT.
KEY is the lower-cased heading text."
  (mapcan (lambda (file)
            (pos--map-headings
             file
             (lambda ()
               (when (org-get-todo-state)
                 (list (downcase (string-trim (org-get-heading t t t t)))
                       file (line-number-at-pos))))))
          (pos-org-files root)))

(defun pos-lint-duplicate-tasks (root)
  "Report task headings in more than one covered file under ROOT.
Each copy names the others."
  (let ((findings nil))
    (dolist (group (seq-group-by #'car (pos--task-headings root)))
      (let ((copies (cdr group)))
        (when (> (length copies) 1)
          (dolist (copy copies)
            (pcase-let ((`(,_ ,file ,line) copy))
              (push (list file line
                          (format "duplicate task (also %s)"
                                  (mapconcat
                                   (lambda (other)
                                     (pcase-let ((`(,_ ,ofile ,oline) other))
                                       (format "%s:%d"
                                               (file-relative-name ofile root) oline)))
                                   (remove copy copies) ", ")))
                    findings))))))
    (nreverse findings)))

(defvar pos-lint-repo-checks
  '(pos-lint-stranded-tasks
    pos-lint-duplicate-tasks)
  "Whole-tree checks: root to findings.")

(defun pos-lint-format (findings root)
  "Render FINDINGS as \"file:line: message\" lines relative to ROOT."
  (mapconcat (lambda (finding)
               (pcase-let ((`(,file ,line ,message) finding))
                 (format "%s:%d: %s\n"
                         (file-relative-name file root) line message)))
             findings ""))

(defun pos-lint ()
  "Lint covered files; return findings sorted by file and line.
Interactively, show them in a compilation buffer."
  (interactive)
  (let* ((root (file-name-as-directory pos-directory))
         (findings
          (sort (append
                 (mapcan (lambda (file)
                           (mapcan (lambda (check) (funcall check file))
                                   pos-lint-checks))
                         (pos-org-files root))
                 (mapcan (lambda (check) (funcall check root))
                         pos-lint-repo-checks))
                (lambda (a b)
                  (or (string< (car a) (car b))
                      (and (string= (car a) (car b))
                           (< (cadr a) (cadr b))))))))
    (when (called-interactively-p 'any)
      (with-current-buffer (get-buffer-create "*pos-lint*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq default-directory root)
          (insert (if findings (pos-lint-format findings root) "clean\n"))
          (compilation-mode))
        (pop-to-buffer (current-buffer))))
    findings))

(defun pos-lint-batch ()
  "Print lint findings; exit 1 if any."
  (let ((findings (pos-lint)))
    (princ (pos-lint-format findings (file-name-as-directory pos-directory)))
    (kill-emacs (if findings 1 0))))

;;;; Rescuing stranded tasks

(defun pos-stranded-open-tasks (root)
  "Return (FILE LINE HEADING) for open tasks under ROOT outside the sweep.
Topmost of a nest only."
  (mapcan
   (lambda (file)
     (pos--map-headings
      file
      (lambda ()
        (when (and (org-get-todo-state) (not (org-entry-is-done-p)))
          (setq org-map-continue-from
                (save-excursion (org-end-of-subtree t) (point)))
          (list file (line-number-at-pos) (org-get-heading t t t t))))))
   (pos-uncovered-org-files root)))

(defun pos--backreference (root file)
  "Return a link line to the parent of the heading at point in FILE.
The link is relative to ROOT."
  (let* ((relative (file-relative-name file root))
         (parent (car (last (org-get-outline-path)))))
    (if parent
        (format "From [[file:%s::*%s][%s: %s]]" relative parent relative parent)
      (format "From [[file:%s][%s]]" relative relative))))

(defun pos-goto-unsorted-end ()
  "Move point to the end of the Unsorted subtree; create it if missing."
  (goto-char (point-min))
  (if (re-search-forward "^\\* Unsorted\\b" nil t)
      (org-end-of-subtree t t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* Unsorted\n"))
  (unless (bolp) (insert "\n")))

(defun pos-refile-stranded-in-file (root file intray)
  "Move open tasks in FILE under Unsorted in INTRAY, linked back.
Links are relative to ROOT.  Return the headings moved."
  (let ((moved nil)
        (make-backup-files nil))
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
         (when (and (org-get-todo-state) (not (org-entry-is-done-p)))
           (let ((backreference (pos--backreference root file))
                 (heading (org-get-heading t t t t)))
             (setq org-map-continue-from (point))
             (org-cut-subtree)
             (with-current-buffer intray
               (pos-goto-unsorted-end)
               (let ((start (point)))
                 (org-paste-subtree 2)
                 (goto-char start)
                 (org-end-of-meta-data t)
                 (insert backreference "\n")))
             (push heading moved))))
       nil 'file)
      (save-buffer))
    (nreverse moved)))

(defun pos-refile-stranded-report (tasks dry-run)
  "Return a report of TASKS, (FILE LINE HEADING) each; DRY-RUN words it."
  (concat
   (format "%s %d stranded task%s into intray.org"
           (if dry-run "Would refile" "Refiled")
           (length tasks) (if (= (length tasks) 1) "" "s"))
   (mapconcat (lambda (task)
                (pcase-let ((`(,file ,line ,heading) task))
                  (format "\n  %s:%d %s"
                          (file-relative-name file pos-directory) line heading)))
              tasks "")))

(defun pos-refile-stranded (&optional dry-run)
  "Move open tasks from files outside the sweep into the intray.
With DRY-RUN or prefix argument, only report.
Return (FILE LINE HEADING) for each."
  (interactive "P")
  (let* ((root (file-name-as-directory pos-directory))
         (tasks (pos-stranded-open-tasks root)))
    (unless dry-run
      (let ((intray (find-file-noselect (expand-file-name "intray.org" root)))
            (make-backup-files nil))
        (dolist (file (delete-dups (mapcar #'car tasks)))
          (pos-refile-stranded-in-file root file intray))
        (with-current-buffer intray (save-buffer))))
    (message "%s" (pos-refile-stranded-report tasks dry-run))
    tasks))

(defun pos-refile-stranded-batch (dry-run)
  "Run `pos-refile-stranded' with DRY-RUN; print the report."
  (let ((tasks (pos-refile-stranded dry-run)))
    (princ (pos-refile-stranded-report tasks dry-run))
    (terpri)))

;;;; Duplicate tasks

(defun pos--subtree-body ()
  "Return the subtree at point below its heading, trimmed.
A property drawer is not body: two copies alike but for their IDs say
the same thing."
  (save-excursion
    (let* ((end (save-excursion (org-end-of-subtree t t) (point)))
           (start (min end (line-beginning-position 2)))
           (planning (progn (goto-char start)
                            (when (looking-at-p org-planning-line-re) (forward-line 1))
                            (buffer-substring-no-properties start (point))))
           (rest (progn (when (looking-at org-property-drawer-re)
                          (goto-char (min end (1+ (match-end 0)))))
                        (buffer-substring-no-properties (point) end))))
      (string-trim (concat planning rest)))))

(defun pos--task-entries (root)
  "Return a plist per keyword heading in covered files under ROOT.
Keys: :key :heading :file :line :parent :body :id."
  (mapcan (lambda (file)
            (pos--map-headings
             file
             (lambda ()
               (when (org-get-todo-state)
                 (let ((heading (string-trim (org-get-heading t t t t))))
                   (list :key (downcase heading)
                         :heading heading
                         :file file
                         :line (line-number-at-pos)
                         :parent (car (last (org-get-outline-path)))
                         :body (pos--subtree-body)
                         :id (org-entry-get nil "ID")))))))
          (pos-org-files root)))

(defun pos-dedupe-groups (root)
  "Return groups of duplicate tasks under ROOT."
  (seq-filter (lambda (group) (> (length group) 1))
              (mapcar #'cdr
                      (seq-group-by (lambda (entry) (plist-get entry :key))
                                    (pos--task-entries root)))))

(defun pos-dedupe-suggest (group root)
  "Return GROUP as (ACTION . ENTRY) pairs; ACTION is keep, drop or ?.
Pillar beats intray (under ROOT); in one file, longer body wins;
two pillar files: ?."
  (let* ((intray (expand-file-name "intray.org" root))
         (pillar (seq-remove (lambda (e) (equal (plist-get e :file) intray)) group))
         (candidates (or pillar group))
         (files (delete-dups (mapcar (lambda (e) (plist-get e :file)) candidates)))
         (survivor
          (cond ((= 1 (length candidates)) (car candidates))
                ((= 1 (length files))
                 (car (sort (copy-sequence candidates)
                            (lambda (a b) (> (length (plist-get a :body))
                                             (length (plist-get b :body)))))))
                (t nil))))
    (mapcar (lambda (entry)
              (cons (cond ((null survivor) "?")
                          ((eq entry survivor) "keep")
                          (t "drop"))
                    entry))
            group)))

(defun pos-dedupe--referrer-lines (root)
  "Return a function of an ID giving \"FILE:LINE\" per link to it under ROOT.
Call within `pos-roam-with-index'."
  (lambda (id)
    (mapcar (lambda (referrer)
              (pcase-let ((`(,file . ,pos) referrer))
                ;; The index names files by their true paths.
                (format "%s:%d" (file-relative-name file (file-truename root))
                        (with-current-buffer (pos-roam-buffer file)
                          (line-number-at-pos pos)))))
            (pos-roam-referrers id))))

(defun pos-dedupe-plan (root &optional groups)
  "Return the dedupe plan for ROOT as Org text.
GROUPS defaults to `pos-dedupe-groups'.  Each group lists, after its
table, the \"id:\" links the index records to each copy that has an ID."
  (pos-roam-with-index root
    (let ((referrers (pos-dedupe--referrer-lines root)))
      (with-temp-buffer
        (insert "#+TITLE: Duplicate tasks\n\n"
                "- Per group: one keep, rest drop; ? skips.\n"
                "- Differing dropped bodies merge under the keep.\n"
                "- Links to a dropped copy's ID are pointed at the keep;\n"
                "  a keep without an ID takes the first dropped one's.\n"
                "- Then: pos dedupe apply.\n")
        (dolist (group (or groups (pos-dedupe-groups root)))
          (insert "\n* " (plist-get (car group) :heading) "\n"
                  "| act | file | line | under | body lines | id |\n|-\n")
          (let (links)
            (dolist (suggestion (pos-dedupe-suggest group root))
              (pcase-let* ((`(,action . ,entry) suggestion)
                           (file (file-relative-name (plist-get entry :file) root))
                           (id (plist-get entry :id)))
                (insert (format "| %s | %s | %d | %s | %d | %s |\n"
                                action file
                                (plist-get entry :line)
                                (or (plist-get entry :parent) "")
                                (if (string-empty-p (plist-get entry :body)) 0
                                  (length (split-string (plist-get entry :body) "\n")))
                                (or id "")))
                (when id
                  (let ((from (funcall referrers id)))
                    (when from
                      (push (format "- Links to %s:%d from %s\n" file (plist-get entry :line)
                                    (string-join from ", "))
                            links))))))
            (apply #'insert (nreverse links))))
        (org-mode)
        (org-table-map-tables #'org-table-align t)
        (buffer-string)))))

(defun pos-dedupe-read-plan (file)
  "Read the plan in FILE as (KEY (ACTION FILE LINE)...) groups."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((groups nil) (current nil))
      (while (not (eobp))
        (cond ((looking-at "^\\* \\(.*\\)$")
               (when current (push (nreverse current) groups))
               (setq current (list (downcase (string-trim (match-string 1))))))
              ((looking-at "^| *\\([^| ]+\\) *| *\\([^|]*?\\) *| *\\([0-9]+\\) *|")
               (push (list (match-string 1) (match-string 2)
                           (string-to-number (match-string 3)))
                     current)))
        (forward-line 1))
      (when current (push (nreverse current) groups))
      (nreverse groups))))

(defun pos--marker-still-at-p (marker key)
  "Return non-nil if MARKER is at a heading matching KEY."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (and (org-at-heading-p)
           (equal key (downcase (string-trim (org-get-heading t t t t))))))))

(defun pos--marker-at (root file line key)
  "Return a marker at LINE of FILE under ROOT if KEY's heading is there."
  (with-current-buffer (find-file-noselect (expand-file-name file root))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (let ((marker (point-marker)))
        (and (pos--marker-still-at-p marker key) marker)))))

(defun pos--merge-under (drop keep origin)
  "Paste subtree DROP under KEEP, without keyword, headed ORIGIN."
  (progn
    (with-current-buffer (marker-buffer drop)
      (goto-char drop)
      (org-copy-subtree))
    (with-current-buffer (marker-buffer keep)
      (goto-char keep)
      (let ((level (org-current-level)))
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (let ((start (point)))
          (org-paste-subtree (1+ level))
          (goto-char start)
          (org-todo 'none)
          (org-edit-headline origin)
          ;; The merged note is not the task: links to the dropped
          ;; copy's ID go to the kept one, not here.
          (org-entry-delete nil "ID"))))))

(defun pos-dedupe--relink (from to dry-run)
  "Point each \"id:\" link to FROM the index records at TO instead.
With DRY-RUN count only.  Return (COUNT . BUFFERS): the links rewritten,
or that would be, and the buffers it wrote.  Call within
`pos-roam-with-index'; a link no longer where the index says is passed
over."
  (let ((count 0) buffers)
    ;; Later links first, so that a rewrite moves no earlier one.
    (dolist (referrer (reverse (pos-roam-referrers from)))
      (pcase-let* ((`(,file . ,pos) referrer)
                   (buffer (pos-roam-rewrite-link file pos from to dry-run)))
        (when buffer
          (setq count (1+ count))
          (cl-pushnew buffer buffers))))
    (cons count buffers)))

(defun pos-dedupe-apply (root plan &optional dry-run)
  "Apply dedupe PLAN to ROOT; with DRY-RUN change nothing.
Return (:resolved :merged :relinked :skipped :vanished :stale) counts.
A dropped copy's ID passes to the kept copy where it has none; else each
\"id:\" link to it that the index records is pointed at the kept copy's,
and :relinked counts them."
  (pos-roam-with-index root
  (let ((resolved 0) (merged 0) (relinked 0) (skipped 0) (vanished 0) (stale 0)
        (make-backup-files nil)
        (jobs nil) (touched nil))
    ;; Resolve all markers before any edit.
    (dolist (group (pos-dedupe-read-plan plan))
      (let* ((key (car group))
             (copies (cdr group))
             (keeps (seq-filter (lambda (c) (equal (car c) "keep")) copies))
             (drops (seq-filter (lambda (c) (equal (car c) "drop")) copies)))
        (if (not (and (= 1 (length keeps)) drops))
            (setq skipped (1+ skipped))
          (let ((markers (mapcar (lambda (c) (pos--marker-at root (nth 1 c) (nth 2 c) key))
                                 (cons (car keeps) drops))))
            (if (memq nil markers)
                (setq stale (1+ stale))
              (push (cons key
                          (cons (car markers)
                                (cl-mapcar (lambda (marker d)
                                             (cons marker (format "Merged copy from %s:%d"
                                                                  (nth 1 d) (nth 2 d))))
                                           (cdr markers) drops)))
                    jobs))))))
    (setq jobs (nreverse jobs))
    (dolist (job jobs)
      (pcase-let ((`(,key ,keep . ,drops) job))
        (if (not (pos--marker-still-at-p keep key))
            (setq vanished (1+ vanished))
          (let ((keep-body (with-current-buffer (marker-buffer keep)
                             (goto-char keep) (pos--subtree-body)))
                (keep-id (org-entry-get keep "ID")))
            (dolist (pair drops)
              (pcase-let ((`(,drop . ,label) pair))
                (if (not (pos--marker-still-at-p drop key))
                    (setq vanished (1+ vanished))
                  (let ((drop-body (with-current-buffer (marker-buffer drop)
                                     (goto-char drop) (pos--subtree-body)))
                        (drop-id (org-entry-get drop "ID")))
                    (when drop-id
                      (if keep-id
                          (pcase-let ((`(,count . ,buffers)
                                       (pos-dedupe--relink drop-id keep-id dry-run)))
                            (setq relinked (+ relinked count))
                            (dolist (buffer buffers) (cl-pushnew buffer touched)))
                        (setq keep-id drop-id)
                        (unless dry-run (org-entry-put keep "ID" drop-id))))
                    (when (and (not (string-empty-p drop-body))
                               (not (string= drop-body keep-body)))
                      (setq merged (1+ merged))
                      (unless dry-run (pos--merge-under drop keep label)))
                    (unless dry-run
                      (with-current-buffer (marker-buffer drop)
                        (goto-char drop)
                        (org-cut-subtree))))))))
          (setq resolved (1+ resolved)))))
    (unless dry-run
      (dolist (job jobs)
        (dolist (marker (cons (cadr job) (mapcar #'car (cddr job))))
          (cl-pushnew (marker-buffer marker) touched)))
      (dolist (buffer touched)
        (with-current-buffer buffer
          (when (buffer-modified-p) (save-buffer)))))
    (list :resolved resolved :merged merged :relinked relinked :skipped skipped
          :vanished vanished :stale stale))))

(defun pos-dedupe-report (result dry-run)
  "Return a report of RESULT from `pos-dedupe-apply'; DRY-RUN words it."
  (format "%s %d duplicate group%s (%d merged, %d links relinked, %d skipped as undecided, %d gone with an earlier cut, %d stale)"
          (if dry-run "Would resolve" "Resolved")
          (plist-get result :resolved)
          (if (= 1 (plist-get result :resolved)) "" "s")
          (plist-get result :merged)
          (plist-get result :relinked)
          (plist-get result :skipped)
          (plist-get result :vanished)
          (plist-get result :stale)))

(defun pos-dedupe-plan-batch (file)
  "Write the dedupe plan to FILE; print the group count."
  ;; Scan once: a rescan would prompt about the changed plan buffer.
  (let* ((root (file-name-as-directory pos-directory))
         (groups (pos-dedupe-groups root)))
    (with-temp-file file (insert (pos-dedupe-plan root groups)))
    (princ (format "%d duplicate groups written to %s\n"
                   (length groups)
                   (if (string-prefix-p root (expand-file-name file))
                       (file-relative-name file root)
                     file)))))

(defun pos-dedupe-apply-batch (file dry-run)
  "Apply the dedupe plan in FILE with DRY-RUN; print the report.
The index is brought up to date with what was written."
  (let ((root (file-name-as-directory pos-directory)))
    (princ (pos-dedupe-report (pos-dedupe-apply root file dry-run) dry-run))
    (terpri)
    (unless dry-run (pos-roam-sync root))))

;;;; Refiling the intray

(defcustom pos-refile-rules nil
  "Heading to pillar rules, (REGEXP . PILLAR); first match wins."
  :type '(alist :key-type regexp :value-type string)
  :group 'pos)

(defun pos-refile-suggest (heading)
  "Return the pillar `pos-refile-rules' suggests for HEADING, or nil."
  (let ((case-fold-search t))
    (cdr (seq-find (lambda (rule) (string-match-p (car rule) heading))
                   pos-refile-rules))))

(defun pos-refile-candidates (root)
  "Return (LINE HEADING SECTION SUBTREE) per level-two entry in ROOT's intray."
  (with-current-buffer (find-file-noselect (expand-file-name "intray.org" root))
    (delq nil
          (org-map-entries
           (lambda ()
             (when (= 2 (org-current-level))
               (list (line-number-at-pos)
                     (string-trim (org-get-heading t t t t))
                     (car (org-get-outline-path))
                     (string-trim-right
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (save-excursion (org-end-of-subtree t t) (point)))))))
           nil 'file))))

(defun pos--excerpt (subtree)
  "Return SUBTREE's first body line, shortened, for a table cell."
  (let ((line (seq-find (lambda (l)
                          (and (not (string-empty-p (string-trim l)))
                               (not (string-match-p "^\\(SCHEDULED\\|DEADLINE\\|CLOSED\\|:\\)" (string-trim l)))))
                        (cdr (split-string subtree "\n")))))
    (if line
        (truncate-string-to-width
         (replace-regexp-in-string "|" "/" (string-trim line)) 60 nil nil "…")
      "")))

(defun pos-refile-plan (root)
  "Return the refile plan for ROOT's intray as Org text.
One row per entry; each subtree quoted below."
  (let ((candidates (pos-refile-candidates root)))
    (with-temp-buffer
      (insert "#+TITLE: Refile the intray\n\n"
              "- move: set file, optionally under (heading path).\n"
              "- skip or ?: left in place.\n"
              "- Full entries quoted below.\n"
              "- Then: pos refile apply.\n\n"
              "| act | line | heading | file | under | excerpt |\n|-\n")
      (dolist (entry candidates)
        (pcase-let* ((`(,line ,heading ,_section ,subtree) entry)
                     (pillar (pos-refile-suggest heading)))
          (insert (format "| %s | %d | %s | %s | | %s |\n"
                          (if pillar "move" "?")
                          line
                          (replace-regexp-in-string "|" "/" heading)
                          (if pillar (format "%s/%s-projects.org" pillar pillar) "")
                          (pos--excerpt subtree)))))
      (org-mode)
      (org-table-map-tables #'org-table-align t)
      ;; Example blocks keep quoted headings as text.
      (goto-char (point-max))
      (insert "\n* Entries\n")
      (dolist (entry candidates)
        (pcase-let ((`(,line ,heading ,section ,subtree) entry))
          (insert (format "** %d: %s (under %s)\n#+begin_example\n%s\n#+end_example\n"
                          line heading section
                          (org-escape-code-in-string subtree)))))
      (buffer-string))))

(defun pos-refile-read-plan (file)
  "Read the plan in FILE as (ACT LINE HEADING TARGET UNDER) rows."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((rows nil))
      (while (re-search-forward
              "^| *\\([^| ]+\\) *| *\\([0-9]+\\) *| *\\([^|]*?\\) *| *\\([^|]*?\\) *| *\\([^|]*?\\) *|"
              nil t)
        (push (list (match-string 1) (string-to-number (match-string 2))
                    (match-string 3) (match-string 4) (match-string 5))
              rows))
      (nreverse rows))))

(defun pos--goto-refile-target (under)
  "Move point to the refile target; return the paste level.
UNDER is a slash-separated outline path; empty means end of file.
Return nil if the path is absent."
  (goto-char (point-min))
  (if (string-empty-p under)
      (progn (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             1)
    (let ((level 0) (bound (point-max)) (found t))
      (dolist (name (split-string under "/" t "[ \t]+"))
        (when found
          (setq level (1+ level))
          (if (re-search-forward
               (concat "^" (make-string level ?*) " " (regexp-quote name) "[ \t]*$")
               bound t)
              (setq bound (save-excursion (org-end-of-subtree t t) (point)))
            (setq found nil))))
      (when found
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (1+ level)))))

(defun pos-refile-apply (root plan &optional dry-run)
  "Apply refile PLAN to ROOT's intray; with DRY-RUN change nothing.
Return (:moved :left :missing :vanished) counts."
  (let ((moved 0) (left 0) (missing 0) (vanished 0)
        (make-backup-files nil)
        (intray (find-file-noselect (expand-file-name "intray.org" root)))
        (jobs nil))
    (dolist (row (pos-refile-read-plan plan))
      (pcase-let ((`(,act ,line ,heading ,target ,under) row))
        (if (not (and (equal act "move") (not (string-empty-p target))))
            (setq left (1+ left))
          (let ((marker (pos--marker-at root "intray.org" line
                                        (downcase (replace-regexp-in-string "|" "/" heading)))))
            (if (null marker)
                (setq vanished (1+ vanished))
              (push (list marker target under) jobs))))))
    (setq jobs (nreverse jobs))
    (dolist (job jobs)
      (pcase-let ((`(,marker ,target ,under) job))
        (let ((target-file (expand-file-name target root)))
          (if (not (file-exists-p target-file))
              (setq missing (1+ missing))
            (with-current-buffer (find-file-noselect target-file)
              (let ((level (save-excursion (pos--goto-refile-target under))))
                (if (null level)
                    (setq missing (1+ missing))
                  (setq moved (1+ moved))
                  (unless dry-run
                    (with-current-buffer intray
                      (goto-char marker)
                      (org-cut-subtree))
                    (pos--goto-refile-target under)
                    (org-paste-subtree level)))))))))
    (unless dry-run
      (with-current-buffer intray (when (buffer-modified-p) (save-buffer)))
      (dolist (job jobs)
        (with-current-buffer (find-file-noselect (expand-file-name (nth 1 job) root))
          (when (buffer-modified-p) (save-buffer)))))
    (list :moved moved :left left :missing missing :vanished vanished)))

(defun pos-refile-report (result dry-run)
  "Return a report of RESULT from `pos-refile-apply'; DRY-RUN words it."
  (format "%s %d intray entr%s (%d left, %d with a missing target, %d no longer at their line)"
          (if dry-run "Would refile" "Refiled")
          (plist-get result :moved)
          (if (= 1 (plist-get result :moved)) "y" "ies")
          (plist-get result :left)
          (plist-get result :missing)
          (plist-get result :vanished)))

(defun pos-refile-plan-batch (file)
  "Write the refile plan to FILE; print the row count."
  (let* ((root (file-name-as-directory pos-directory))
         (candidates (pos-refile-candidates root)))
    (with-temp-file file (insert (pos-refile-plan root)))
    (princ (format "%d intray entries written to %s\n"
                   (length candidates)
                   (if (string-prefix-p root (expand-file-name file))
                       (file-relative-name file root)
                     file)))))

(defun pos-refile-apply-batch (file dry-run)
  "Apply the refile plan in FILE with DRY-RUN; print the report."
  (let ((root (file-name-as-directory pos-directory)))
    (princ (pos-refile-report (pos-refile-apply root file dry-run) dry-run))
    (terpri)))

(provide 'pos)
;;; pos.el ends here
