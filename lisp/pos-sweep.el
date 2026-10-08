;;; pos-sweep.el --- Retire done items where their scope says  -*- lexical-binding: t -*-

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

;; Each week the done items of a tree's Org files are retired: moved
;; out of the files people work in, into an archive of the week that
;; ended.  Where they go is the owning scope's to say, in its archive
;; entry's sweep (doc/pos-directory.txt, section 4): weekly, into Org
;; archive files beneath the scope; or sealed, into such files beneath
;; _sweep/, which once the week closes are sealed into the scope's
;; archive, on disk or with its keeper, so that finished work has the
;; standing of any sealed item.  A scope with no entry of its own is
;; retired by the nearest above it; with none anywhere, weekly beneath
;; the root at archive/orgmode, as before there was a choice.
;;
;; Retiring takes the plan, review, apply shape dedupe and refile
;; have: the plan names each file's done entries and where they would
;; go; a reviewer marks a file skip to leave it; apply does the rest,
;; and leaves a file whose done entries changed since the plan.
;; Closing a week is a seal, and goes through pos-seal's plan, hash
;; and apply.
;;
;; - `pos-sweep-plan', `pos-sweep-apply': the two steps.
;; - `pos-sweep': both at once, for M-x.
;; - `pos-sweep-close': the seal plans of the weeks that have closed.
;; - `pos-sweep-destination', `pos-sweep-close-plans': the adapters,
;;   generic over the sweep's kind.
;; - `pos-sweep-batch': the command line.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-archive)
(require 'pos)
(require 'pos-corpus)
(require 'pos-seal)

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

(defun pos-sweep-week-time (week)
  "Return the boundary time of WEEK, an ISO week name such as \"2026-W36\".
The sweep boundary that closes the week: its Sunday at `pos-sweep-hour'."
  (unless (string-match "\\`\\([0-9]\\{4\\}\\)-W\\([0-9]\\{2\\}\\)\\'" week)
    (user-error "Not a week: %s" week))
  (let* ((year (string-to-number (match-string 1 week)))
         (number (string-to-number (match-string 2 week)))
         ;; January 4th is always in week 1; the Monday of week 1 is
         ;; found from it, and the Sunday of week N is six days after
         ;; the Monday of week N.
         (fourth (decode-time (encode-time (list 0 0 12 4 1 year nil -1 nil))))
         (monday-offset (- 1 (let ((day (decoded-time-weekday fourth)))
                               (if (= day 0) 7 day))))
         (days (+ monday-offset (* 7 (1- number)) 6)))
    (encode-time (list 0 0 pos-sweep-hour (+ 4 days) 1 year nil -1 nil))))

;;;; Archiving a file

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

(defun pos-sweep-done-entries (file)
  "Return (COUNT . SKIPPED) for the done entries of FILE, changing nothing.
COUNT is how many would be archived: each done entry with no open
entry beneath it, its subtree counted once.  SKIPPED are the headings
of done entries with an open entry beneath them, which stay."
  (let ((count 0) (skipped nil))
    (with-current-buffer (pos-visit file)
      (org-map-entries
       (lambda ()
         (when (org-entry-is-done-p)
           (if (pos--open-descendant-p)
               (push (org-get-heading t t t t) skipped)
             (setq org-map-continue-from
                   (save-excursion (org-end-of-subtree t) (point)))
             (setq count (1+ count)))))
       nil 'file))
    (cons count (nreverse skipped))))

(defun pos-archive-done-in-file (file archive relative time)
  "Archive the done entries of FILE into ARCHIVE, stamped TIME.
RELATIVE is FILE's path as the archive records it, relative to the
scope the archive belongs to.  A done entry with an open entry beneath
it stays.  Return (:archived COUNT :skipped HEADINGS)."
  (let* ((archived 0)
         (skipped nil)
         (org-archive-location (concat archive "::"))
         (make-backup-files nil)
         (org-archive-file-header-format
          (format "\nArchived entries from file %s\n\n" relative)))
    (make-directory (file-name-directory archive) t)
    (with-current-buffer (pos-visit file)
      (org-map-entries
       (lambda ()
         (when (org-entry-is-done-p)
           (if (pos--open-descendant-p)
               (push (org-get-heading t t t t) skipped)
             ;; Subtree gone; resume here.
             (setq org-map-continue-from (point))
             (org-archive-subtree)
             (pos--fix-last-archived (pos-visit archive) relative time)
             (setq archived (1+ archived)))))
       nil 'file)
      (save-buffer))
    ;; org-archive-subtree saves the archive only from the agenda.
    (when (> archived 0)
      (with-current-buffer (pos-visit archive)
        (save-buffer)))
    (list :archived archived :skipped (nreverse skipped))))

;;;; Adapters

(defun pos-sweep-adapter (corpus scope)
  "Return (KIND ENTRY . DIR): how SCOPE's done items are retired.
KIND is the symbol weekly or sealed; ENTRY the archives entry that
says so, or nil for the default; DIR the directory of the archive
scope the entry belongs to, as a directory name.  The entry is the
nearest with sweep at or above SCOPE, looking in each node from the
nearest up; none means weekly beneath the root of CORPUS."
  (let ((dir (pos-scope-dir scope))
        (node (if (pos-scope-config scope) scope (pos-scope-node scope)))
        (found nil))
    (while (and node (not found))
      (let ((best nil))
        (seq-doseq (entry (alist-get 'archives (pos-scope-config node)))
          (when (alist-get 'sweep entry)
            (let ((entry-dir (file-name-as-directory
                              (expand-file-name (alist-get 'scope entry)
                                                (pos-scope-dir node)))))
              (when (and (string-prefix-p entry-dir dir)
                         (or (null best) (> (length entry-dir) (length (cdr best)))))
                (setq best (cons entry entry-dir))))))
        (when best
          (setq found (cons (intern (alist-get 'sweep (car best))) best))))
      (setq node (pos-scope-node node)))
    (or found (cons 'weekly (cons nil (pos-corpus-root corpus))))))

(cl-defgeneric pos-sweep-destination (kind entry dir file week)
  "Return the archive file for FILE's done items in WEEK.
KIND is the sweep's kind and ENTRY its archives entry, as
`pos-sweep-adapter' gives them; DIR is the archive scope's directory.")

(cl-defmethod pos-sweep-destination ((_kind (eql weekly)) entry dir file week)
  "Return FILE's archive beneath DIR at ENTRY's path, in WEEK's directory.
One directory per week, one archive file per source."
  (expand-file-name (concat (file-relative-name file dir) "_archive")
                    (expand-file-name
                     week (expand-file-name (or (alist-get 'path entry)
                                                pos-sweep-default-path)
                                            dir))))

(cl-defmethod pos-sweep-destination ((_kind (eql sealed)) _entry dir file week)
  "Return FILE's archive beneath DIR's _sweep/, in WEEK's directory.
The week's directory stays there until the week closes and is sealed."
  (expand-file-name (concat (file-relative-name file dir) "_archive")
                    (expand-file-name week (expand-file-name "_sweep" dir))))

(cl-defgeneric pos-sweep-close-plans (kind dir week)
  "Return the seal plans that close the weeks before WEEK in DIR, or nil.
KIND is the sweep's kind and DIR the archive scope's directory.")

(cl-defmethod pos-sweep-close-plans ((_kind (eql weekly)) _dir _week)
  "Return nil: a weekly sweep's files stay as they are, and nothing closes."
  nil)

(cl-defmethod pos-sweep-close-plans ((_kind (eql sealed)) dir week)
  "Return a seal plan for each week beneath DIR's _sweep/ before WEEK.
Each is sealed into DIR's archive at sweep/ under the week's name."
  (let ((staging (expand-file-name "_sweep" dir)))
    (when (file-directory-p staging)
      (mapcar (lambda (closed)
                (pos-seal-plan (expand-file-name closed staging)
                               (expand-file-name (concat "archives/sweep/" closed) dir)))
              (seq-filter (lambda (name) (string< name week))
                          (directory-files staging nil "\\`[0-9]\\{4\\}-W[0-9]\\{2\\}\\'"))))))

;;;; The plan

(defun pos-sweep-rows (corpus week)
  "Return the rows of a sweep of CORPUS for WEEK, changing nothing.
One for each writable file with a done entry: (FILE DONE DESTINATION
SKIPPED), FILE and DESTINATION relative to the root, DONE how many
entries would be archived, SKIPPED the headings that would stay."
  (let ((root (pos-corpus-root corpus)) rows)
    (pcase-dolist (`(,file . ,scope) (pos-corpus-entries corpus))
      (when (pos-corpus-writable-p corpus file)
        (pcase-let* ((`(,count . ,skipped) (pos-sweep-done-entries file))
                     (`(,kind ,entry . ,dir) (pos-sweep-adapter corpus scope)))
          (when (or (> count 0) skipped)
            (push (list (file-relative-name file root) count
                        (file-relative-name
                         (pos-sweep-destination kind entry dir file week) root)
                        skipped)
                  rows)))))
    (nreverse rows)))

(defun pos-sweep-plan (root &optional week)
  "Return the plan to sweep the tree at ROOT for WEEK, as Org text.
WEEK defaults to the one the latest boundary closed.  One table row
per file with done entries; a done entry with an open entry beneath
it is named below the table and stays."
  (let* ((week (or week (pos-week-name (pos-sweep-boundary (current-time)))))
         (rows (pos-sweep-rows (pos-corpus root) week)))
    (with-temp-buffer
      (insert (format "#+TITLE: Sweep %s\n\n" week)
              "- sweep: archive the file's done entries to the destination.\n"
              "- skip or ?: left in place.\n"
              "- Then: pos sweep apply.\n\n"
              "| act | file | done | destination |\n|-\n")
      (pcase-dolist (`(,file ,count ,destination ,_) rows)
        (insert (format "| %s | %s | %d | %s |\n"
                        (if (> count 0) "sweep" "skip") file count destination)))
      (let ((skipped (seq-filter (lambda (row) (nth 3 row)) rows)))
        (when skipped
          (insert "\nDone, but with open children; these stay:\n")
          (pcase-dolist (`(,file ,_ ,_ ,headings) skipped)
            (dolist (heading headings)
              (insert (format "- %s: %s\n" file heading))))))
      (org-mode)
      (goto-char (point-min))
      (when (re-search-forward "^| act " nil t) (org-table-align))
      (buffer-string))))

(defun pos-sweep-read-plan (file)
  "Return (WEEK . ROWS) from the plan in FILE.
Each row is (ACT FILE DONE DESTINATION)."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((week (and (re-search-forward "^#\\+TITLE: Sweep \\(\\S-+\\)" nil t)
                     (match-string 1)))
          rows)
      (unless week (user-error "Not a sweep plan: %s" file))
      (goto-char (point-min))
      (while (re-search-forward
              "^| *\\([^|]*?\\) *| *\\([^|]*?\\) *| *\\([0-9]+\\) *| *\\([^|]*?\\) *|"
              nil t)
        (unless (equal (match-string 1) "act")
          (push (list (match-string 1) (match-string 2)
                      (string-to-number (match-string 3)) (match-string 4))
                rows)))
      (cons week (nreverse rows)))))

;;;; The apply

(defun pos-sweep-apply (root plan &optional dry-run)
  "Apply the sweep PLAN, a file, to the tree at ROOT; with DRY-RUN change nothing.
Each row marked sweep whose file still has the done entries the plan
counted is archived to its destination, stamped with the week's
boundary.  Return (:archived :skipped :left :stale): entries archived,
headings that stayed for an open child, rows left as not sweep, and
rows whose count has changed since the plan."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (corpus (pos-corpus root))
         (read (pos-sweep-read-plan plan))
         (week (car read))
         (time (pos-sweep-week-time week))
         (archived 0) (skipped nil) (left 0) (stale 0))
    (pcase-dolist (`(,act ,relative ,count ,destination) (cdr read))
      (let ((file (expand-file-name relative root)))
        (cond
         ((not (equal act "sweep")) (setq left (1+ left)))
         ((or (not (pos-corpus-writable-p corpus file))
              (/= count (car (pos-sweep-done-entries file))))
          (setq stale (1+ stale)))
         (dry-run
          (setq archived (+ archived count)
                skipped (append skipped (cdr (pos-sweep-done-entries file)))))
         (t
          (pcase-let* ((`(,_kind ,_entry . ,dir)
                        (pos-sweep-adapter corpus (pos-corpus-owner corpus file)))
                       (result (pos-archive-done-in-file
                                file (expand-file-name destination root)
                                (file-relative-name file dir) time)))
            (setq archived (+ archived (plist-get result :archived))
                  skipped (append skipped (plist-get result :skipped))))))))
    (list :archived archived :skipped skipped :left left :stale stale)))

(defun pos-sweep-report (week result &optional dry-run)
  "Return a report of RESULT, a sweep of WEEK; DRY-RUN words it."
  (let ((skipped (plist-get result :skipped)))
    (concat
     (format "%s %s: archived %d, skipped %d, %d files left, %d stale"
             (if dry-run "Would sweep" "Sweep") week (plist-get result :archived)
             (length skipped) (or (plist-get result :left) 0)
             (or (plist-get result :stale) 0))
     (when skipped
       (concat "\n  skipped (done, but has open children):\n"
               (mapconcat (lambda (heading) (concat "    " heading))
                          skipped "\n"))))))

(defun pos-sweep (&optional week)
  "Plan and apply the sweep of `pos-directory' for WEEK at once.
WEEK defaults to the one the latest boundary closed.  Return what
`pos-sweep-apply' returns."
  (interactive)
  (let* ((root (file-name-as-directory pos-directory))
         (week (or week (pos-week-name (pos-sweep-boundary (current-time)))))
         (plan (make-temp-file "pos-sweep-" nil ".org" (pos-sweep-plan root week))))
    (unwind-protect
        (let ((result (pos-sweep-apply root plan)))
          (message "%s" (pos-sweep-report week result))
          result)
      (delete-file plan))))

;;;; Closing weeks

(defun pos-sweep-close (root &optional week)
  "Return the seal plans that close the swept weeks before WEEK in ROOT.
WEEK defaults to the one the latest boundary closed.  One plan for
each week beneath the _sweep/ of each scope whose sweep is sealed.
A scope in another repository of the tree is left to that repository."
  (let* ((corpus (pos-corpus root))
         (week (or week (pos-week-name (pos-sweep-boundary (current-time)))))
         (seen nil) plans)
    (dolist (scope (pos-corpus-scopes corpus))
      (when (and (pos-scope-config scope)
                 (pos-corpus-scope-writable-p corpus scope))
        (seq-doseq (entry (alist-get 'archives (pos-scope-config scope)))
          (when-let* ((kind (alist-get 'sweep entry))
                      (dir (file-name-as-directory
                            (expand-file-name (alist-get 'scope entry) (pos-scope-dir scope)))))
            (unless (member dir seen)
              (push dir seen)
              (setq plans (append plans (pos-sweep-close-plans (intern kind) dir week))))))))
    plans))

(defun pos-sweep-close-apply (plans)
  "Apply PLANS, as `pos-sweep-close' gives them, each under its own hash.
Return the event file of each."
  (mapcar (lambda (plan)
            (car (pos-seal-apply plan (pos-ledger-sha (pos-ledger-json plan)))))
          plans))

;;;; Command line

(defconst pos-sweep-usage
  "Usage: COMMAND ...  (help prints this; Emacs itself takes --help)

  plan [FILE]
      write the plan to FILE, default sweep.org: the done entries of
      each file and where they would go; change nothing
  apply [--dry-run] [FILE]
      archive the done entries of each file marked sweep in FILE;
      --dry-run, before or after FILE, counts and changes nothing
  close [--apply]
      print the seal plans of the swept weeks that have closed, as
      JSON; with --apply, seal them
"
  "What `pos-sweep-batch' prints for help.")

(defun pos-sweep--apply-arguments (arguments)
  "Read ARGUMENTS, what follows apply, as (DRY-RUN . FILE), or nil.
DRY-RUN is non-nil where --dry-run is among them, before or after
FILE; FILE is the plan's name, or nil where none is given.  Nil for
any other option or a second name, so that no word is passed over."
  (let ((names (remove "--dry-run" arguments)))
    (unless (or (cdr names)
                (seq-some (lambda (name) (string-prefix-p "-" name)) names))
      (cons (and (member "--dry-run" arguments) t) (car names)))))

(defun pos-sweep-batch ()
  "Run a command from `command-line-args-left', as in `pos-sweep-usage'."
  (let ((root (file-name-as-directory pos-directory)))
    (condition-case err
        (pcase (prog1 command-line-args-left (setq command-line-args-left nil))
          (`("plan" . ,rest)
           (let* ((file (expand-file-name (or (car rest) "sweep.org") root))
                  (text (pos-sweep-plan root)))
             (with-temp-file file (insert text))
             (princ (format "%d files with done entries written to %s\n"
                            (cl-count-if (lambda (line) (string-prefix-p "| sweep" line))
                                         (split-string text "\n"))
                            (file-relative-name file root)))))
          (`("apply" . ,(app pos-sweep--apply-arguments `(,dry-run . ,name)))
           (let* ((file (expand-file-name (or name "sweep.org") root))
                  (week (car (pos-sweep-read-plan file))))
             (princ (concat (pos-sweep-report week (pos-sweep-apply root file dry-run) dry-run)
                            "\n"))))
          (`("close")
           (princ (decode-coding-string
                   (pos-ledger-json (vconcat (pos-sweep-close root))) 'utf-8))
           (princ "\n"))
          (`("close" "--apply")
           (dolist (event (pos-sweep-close-apply (pos-sweep-close root)))
             (princ (format "sealed %s\n" (file-relative-name event root)))))
          (`(,(or "help" "-h" "--help")) (princ pos-sweep-usage))
          (_ (message "%s" pos-sweep-usage)
             (kill-emacs 2)))
      (pos-ledger-refused
       (message "%s: %s" (nth 1 err) (nth 2 err))
       (kill-emacs 2)))))

(provide 'pos-sweep)
;;; pos-sweep.el ends here
