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

;; The commands over a tree's Org files: the lint for duplicate and
;; malformed tasks, and the plans, reviewed then applied, that refile
;; the intray and resolve duplicates; the root and its settings, which
;; pos-sweep.el and the others share.  Each reads the corpus,
;; pos-corpus.el: every Org file the tree's configurations allow, and
;; writes only the files of the root's own repository.  The same from
;; M-x and batch Emacs.

;;; Code:

(require 'org)
(require 'org-archive)
(require 'seq)
(require 'cl-lib)
(require 'pos-roam)
(require 'pos-corpus)

(defgroup pos nil
  "A personal operating system in Org files."
  :group 'org
  :prefix "pos-")

;;;; Files and paths

(defconst pos-sweep-default-path "archive/orgmode"
  "Where a sweep writes beneath a scope that names no path.
One directory per week, one archive file per source, as
doc/pos-directory.txt has it.")

(defun pos-files (root &optional writable)
  "Return the Org files of the tree at ROOT: its corpus, sorted.
With WRITABLE, only those a command of ROOT may write: the files of
the root's own repository, not those of a repository mounted within
it, which that repository's own configuration governs."
  (let ((corpus (pos-corpus root)))
    (if writable
        (seq-filter (lambda (file) (pos-corpus-writable-p corpus file))
                    (pos-corpus-files corpus))
      (pos-corpus-files corpus))))

;;;; Keywords

;; One sequence for every file; per-file #+TODO lines drift.
(defconst pos-todo-keywords
  '((sequence "TODO" "NEXT" "WAITING" "SOMEDAY"
              "|" "DONE" "CANCELLED"))
  "The TODO sequence, shaped as `org-todo-keywords'.")

(defun pos-done-keywords ()
  "Return the done keywords of `pos-todo-keywords'."
  (cdr (member "|" (cdr (car pos-todo-keywords)))))

(defvar pos-visit-corpus nil
  "The corpus whose files are being visited, or nil.
A command that reads a corpus binds it, so that `pos-visit' knows
which files the root may not write.")

(defun pos-visit--own-keywords-p (file)
  "Return non-nil if the keywords FILE was written with stand.
A file of `pos-visit-corpus' that the root may not write: one of
another repository of the tree, or of a product."
  (and pos-visit-corpus
       (pos-corpus-owner pos-visit-corpus file)
       (not (pos-corpus-writable-p pos-visit-corpus file))))

(defun pos-visit (file)
  "Return a buffer visiting FILE, with the one TODO sequence in force.
Org reads `org-todo-keywords' when a buffer enters Org mode, so the
sequence is bound for that moment and the user's own setting is left
as it is.  A buffer already visiting FILE is returned as it is.  A
file of `pos-visit-corpus' that the root may not write is visited
with no sequence bound: its own #+TODO line stands, and with none,
the user's setting."
  (if (pos-visit--own-keywords-p file)
      (find-file-noselect file)
    (let ((org-todo-keywords pos-todo-keywords))
      (find-file-noselect file))))

;;;; The root and its settings

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

(defconst pos-retired-settings
  '((pos-pillars . "the files are every Org file the configuration allows")
    (pos-startup-excluded-directories
     . "the directories not read are declared under exclude in the configuration")
    (pos-archive-directory
     . "where a scope's done items go is declared by its archive entry's sweep and path")
    (pos-roam-excluded-directories
     . "the index covers the corpus, whose exclusions are declared under exclude in the configuration"))
  "Settings pos-config.el once set, each with what replaced it.")

(defun pos-load-config (&optional root)
  "Load the `pos-config-file' of ROOT, default `pos-directory', if present.
Return non-nil if loaded.  A retired setting the file still sets is
reported, and ignored."
  (prog1 (load (expand-file-name pos-config-file (or root pos-directory)) t t t)
    (pcase-dolist (`(,setting . ,replacement) pos-retired-settings)
      (when (boundp setting)
        (message "%s sets %s, which is no longer read: %s"
                 pos-config-file setting replacement)))))

;;;; Normalising keywords

(defun pos-normalise-keywords-in-file (file)
  "Remove #+TODO lines from FILE; respell CANCELED as CANCELLED.
Return (:lines-removed N :respelled N)."
  (let ((lines-removed 0)
        (respelled 0)
        (make-backup-files nil))
    (with-current-buffer (pos-visit file)
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
  "Run `pos-normalise-keywords-in-file' on the tree's files; return totals.
The files are those of the corpus a command may write."
  (interactive)
  (let ((lines-removed 0)
        (respelled 0))
    (dolist (file (pos-files (file-name-as-directory pos-directory) t))
      (let ((result (pos-normalise-keywords-in-file file)))
        (setq lines-removed (+ lines-removed (plist-get result :lines-removed))
              respelled (+ respelled (plist-get result :respelled)))))
    (message "Normalised keywords: %d #+TODO lines removed, %d CANCELED respelled"
             lines-removed respelled)
    (list :lines-removed lines-removed :respelled respelled)))

;;;; Lint

(defun pos--open-descendant-p ()
  "Return non-nil if the entry at point has an open TODO below it."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil))
      (re-search-forward org-not-done-heading-regexp end t))))

(defun pos--map-headings (file function)
  "Call FUNCTION at each heading of FILE; collect non-nil results."
  (with-current-buffer (pos-visit file)
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
  '("BACKLOG" "WIP" "BLOCKED" "CANCELED" "CLARIFY" "DELEGATE" "DELEGATED"
    "MAYBE")
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
  "Report #+TODO lines in FILE.
None is reported in a file the root may not write, by
`pos-visit--own-keywords-p': the line is that file's own to keep, and
`pos-normalise-keywords' could not remove it."
  (with-current-buffer (pos-visit file)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (findings nil))
        (while (and (not (pos-visit--own-keywords-p file))
                    (re-search-forward "^#\\+\\(SEQ_\\|TYP_\\)?TODO:" nil t))
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

(defun pos-lint-clarified-in-intray (file)
  "Report items under Unsorted in FILE, an intray.org, that are not TODO.
An intray holds what is captured and not yet clarified.  An item that
is NEXT, WAITING or SOMEDAY has been clarified and belongs where its
work is; a done one is finished and waits to be retired."
  (when (string= "intray.org" (file-name-nondirectory file))
    (pos--map-headings
     file
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when (and state (not (string= state "TODO"))
                    (string= "Unsorted" (or (car (org-get-outline-path)) "")))
           (pos--finding
            file
            (format (if (org-entry-is-done-p)
                        "%s item in the intray: finished, to be retired"
                      "%s item in the intray: clarified, to be placed")
                    state))))))))

(defun pos--entry-text ()
  "Return the text of the entry at point, without its children.
The planning line and the drawers are part of it."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (buffer-substring-no-properties
     (point) (save-excursion (outline-next-heading) (point)))))

(defconst pos-waiting-on-property "WAITING_ON"
  "The property of a WAITING item that links to the person it waits on.
Its value is a link by ID to a person-identity of the item's scope.")

(defun pos-lint-waiting-without-who-or-when (file)
  "Report WAITING items in FILE that do not say who or what, or since when.
An item says who or what it waits on in a `pos-waiting-on-property'
or DELEGATED_TO property, or in its own text.  It says since when
in the record of its change to WAITING, as `pos-set-state' writes
it, or in a date in its own text, a timestamp or YYYY-MM-DD."
  (pos--map-headings
   file
   (lambda ()
     (when (equal (org-get-todo-state) "WAITING")
       (let* ((text (pos--entry-text))
              (end (save-excursion (outline-next-heading) (point)))
              (own (save-excursion
                     (org-end-of-meta-data t)
                     (string-trim
                      (buffer-substring-no-properties
                       (min (point) end) end))))
              (who (or (org-entry-get nil pos-waiting-on-property)
                       (org-entry-get nil "DELEGATED_TO")
                       (not (string-empty-p own))))
              (since (or (string-match-p "^[ \t]*- State \"WAITING\"" text)
                         (string-match-p
                          "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" own))))
         (cond ((and who since) nil)
               (who (pos--finding
                     file "WAITING item does not say since when"))
               (since (pos--finding
                       file "WAITING item does not say who or what it waits on"))
               (t (pos--finding
                   file (concat "WAITING item does not say who or what it"
                                " waits on, or since when")))))))))

(defvar pos-lint-checks
  '(pos-lint-done-with-open-children
    pos-lint-stale-keyword
    pos-lint-todo-line
    pos-lint-merged-copy
    pos-lint-clarified-in-intray
    pos-lint-waiting-without-who-or-when)
  "Per-file checks: file to findings.")

(defcustom pos-prose-directories nil
  "Root subdirectories of prose; not scanned for stranded tasks."
  :type '(repeat string)
  :group 'pos)

(defun pos-uncovered-org-files (root)
  "Return Org files under ROOT that are not in its corpus.
A walk of its own, kept as it was: every Org file beneath ROOT but
those in the archive, prose and hidden directories, less the files
the corpus reads, and less the files of a methodology a project of the
tree uses, which are the method's own and no task of this root.  A
lock file, which Emacs leaves as a dangling link beside a file being
edited, is not one; visiting it would wait on a question."
  (let* ((corpus (pos-corpus root))
         (covered (pos-corpus-files corpus))
         (excluded (append
                    (mapcar (lambda (dir)
                              (file-name-as-directory (expand-file-name dir root)))
                            (cons pos-sweep-default-path pos-prose-directories))
                    (mapcar #'cddr (pos-corpus-methodologies corpus)))))
    (seq-remove (lambda (file)
                  (or (file-symlink-p file)
                      (seq-some (lambda (dir) (string-prefix-p dir file)) excluded)))
                (seq-difference
                 (directory-files-recursively
                  root "\\`[^.#].*\\.org\\'" nil
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
  "Return (KEY FILE LINE) for keyword headings in the files of ROOT's corpus.
KEY is the lower-cased heading text."
  (mapcan (lambda (file)
            (pos--map-headings
             file
             (lambda ()
               (when (org-get-todo-state)
                 (list (downcase (string-trim (org-get-heading t t t t)))
                       file (line-number-at-pos))))))
          (pos-files root)))

(defun pos-lint-duplicate-tasks (root)
  "Report task headings in more than one file of ROOT's corpus.
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

(defun pos-lint--check-output (command project)
  "Run COMMAND, a check, in PROJECT; return (STATUS OUTPUT . ERRORS).
OUTPUT is what it printed on standard output, ERRORS on standard
error, trimmed."
  (let ((errors (make-temp-file "pos-lint-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((default-directory project)
                 (status (call-process command nil (list t errors) nil)))
            (cons status
                  (cons (buffer-string)
                        (string-trim (with-temp-buffer
                                       (insert-file-contents errors)
                                       (buffer-string)))))))
      (delete-file errors))))

(defun pos-lint-methodology-checks (root)
  "Run each check a methodology of ROOT's projects declares; return findings.
As doc/pos-methodology.txt section 3 has it: a check is run in the
project; each FILE:LINE: MESSAGE line it prints is a finding at that
file, relative to the project, and any other line a finding at the
project; a check that exits 2 is a finding at the methodology with what
it printed on standard error.  A declaration that is refused, and a
check bin/ does not hold, are each a finding at the methodology."
  (let (findings)
    (pcase-dolist (`(,scope ,name . ,dir) (pos-corpus-methodologies (pos-corpus root)))
      (let ((at (directory-file-name dir))
            (project (pos-scope-dir scope)))
        (condition-case err
            (seq-doseq (check (alist-get 'checks (or (pos-tree-read-methodology-file dir)
                                                     '((checks . [])))))
              (let ((command (expand-file-name (concat "bin/" check) dir)))
                (if (not (file-executable-p command))
                    (push (list at 1 (format "check %s is not in bin/ of %s" check name))
                          findings)
                  (pcase-let ((`(,status ,output . ,errors)
                               (pos-lint--check-output command project)))
                    (if (memq status '(0 1))
                        (dolist (line (split-string output "\n" t))
                          (push (if (string-match "\\`\\(.+?\\):\\([0-9]+\\): \\(.*\\)\\'" line)
                                    (list (expand-file-name (match-string 1 line) project)
                                          (string-to-number (match-string 2 line))
                                          (match-string 3 line))
                                  (list (directory-file-name project) 1 line))
                                findings))
                      (push (list at 1 (format "check %s could not check: %s" check errors))
                            findings))))))
          (pos-tree-refused
           (push (list at 1 (format "declaration refused: %s: %s" (nth 1 err) (nth 2 err)))
                 findings)))))
    (nreverse findings)))

(defvar pos-lint-repo-checks
  '(pos-lint-stranded-tasks
    pos-lint-duplicate-tasks
    pos-lint-methodology-checks)
  "Whole-tree checks: root to findings.")

(defun pos-lint-format (findings root)
  "Render FINDINGS as \"file:line: message\" lines relative to ROOT."
  (mapconcat (lambda (finding)
               (pcase-let ((`(,file ,line ,message) finding))
                 (format "%s:%d: %s\n"
                         (file-relative-name file root) line message)))
             findings ""))

(defun pos-lint ()
  "Lint the files of the corpus; return findings sorted by file and line.
Interactively, show them in a compilation buffer."
  (interactive)
  (let* ((root (file-name-as-directory pos-directory))
         (pos-visit-corpus (pos-corpus root))
         (findings
          (sort (append
                 (mapcan (lambda (file)
                           (mapcan (lambda (check) (funcall check file))
                                   pos-lint-checks))
                         (pos-corpus-files pos-visit-corpus))
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
    (with-current-buffer (pos-visit file)
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
      (let ((intray (pos-visit (expand-file-name "intray.org" root)))
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
          (pos-files root)))

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
  (with-current-buffer (pos-visit (expand-file-name file root))
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

(defun pos-dedupe--jobs (root plan corpus)
  "Read PLAN's groups for ROOT into jobs, each with its copies' markers.
A job is (KEY KEEP . DROPS): KEEP the kept copy's marker, each of
DROPS (MARKER . LABEL) with the label its merged body is filed
under.  A group without one keep and a drop is skipped; one with a
copy in a file CORPUS says ROOT may not write is unwritable; one
whose copies are not at their lines is stale.  Return (JOBS :skipped
N :unwritable N :stale N), JOBS in the plan's order."
  (let ((skipped 0) (unwritable 0) (stale 0) jobs)
    (dolist (group (pos-dedupe-read-plan plan))
      (let* ((key (car group))
             (copies (cdr group))
             (keeps (seq-filter (lambda (c) (equal (car c) "keep")) copies))
             (drops (seq-filter (lambda (c) (equal (car c) "drop")) copies)))
        (cond
         ((not (and (= 1 (length keeps)) drops))
          (setq skipped (1+ skipped)))
         ((seq-some (lambda (c)
                      (not (pos-corpus-writable-p corpus (expand-file-name (nth 1 c) root))))
                    copies)
          (setq unwritable (1+ unwritable)))
         (t
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
                    jobs)))))))
    (list (nreverse jobs) :skipped skipped :unwritable unwritable :stale stale)))

(defun pos-dedupe--relinks-unwritable-p (job root corpus)
  "Return non-nil if resolving JOB would relink a file ROOT may not write.
A dropped copy's ID that does not pass to the kept copy has its links
pointed at the kept copy's; a link the index records in a file CORPUS
says ROOT may not write cannot be.  Call within `pos-roam-with-index'."
  (pcase-let* ((`(,key ,keep . ,drops) job)
               (keep-id (and (pos--marker-still-at-p keep key)
                             (org-entry-get keep "ID")))
               (true-root (file-truename root)))
    (seq-some
     (lambda (drop)
       (when-let* ((drop-id (and (pos--marker-still-at-p (car drop) key)
                                 (org-entry-get (car drop) "ID"))))
         (if (not keep-id)
             (progn (setq keep-id drop-id) nil)
           (seq-some
            (lambda (referrer)
              ;; The index names a file by its true path, the corpus as
              ;; ROOT was given.
              (not (pos-corpus-writable-p
                    corpus
                    (expand-file-name (file-relative-name (car referrer) true-root)
                                      root))))
            (pos-roam-referrers drop-id)))))
     drops)))

(defun pos-dedupe--resolve (job dry-run)
  "Resolve JOB, a group with its markers, as `pos-dedupe--jobs' gives it.
Each dropped copy whose body differs from the kept one's is merged
beneath it under the drop's label, then cut; a dropped ID passes to
the kept copy where it has none, else the links to it are pointed at
the kept copy's.  With DRY-RUN count only.  Return (:resolved 0 or 1
:merged N :relinked N :vanished N :touched BUFFERS): vanished counts
copies no longer at their marker, touched the buffers relinking
wrote."
  (pcase-let ((`(,key ,keep . ,drops) job)
              (resolved 0) (merged 0) (relinked 0) (vanished 0) (touched nil))
    (if (not (pos--marker-still-at-p keep key))
        (setq vanished (1+ vanished))
      (setq resolved 1)
      (let ((keep-body (with-current-buffer (marker-buffer keep)
                         (goto-char keep) (pos--subtree-body)))
            (keep-id (org-entry-get keep "ID")))
        (pcase-dolist (`(,drop . ,label) drops)
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
    (list :resolved resolved :merged merged :relinked relinked
          :vanished vanished :touched touched)))

(defun pos-dedupe-apply (root plan &optional dry-run)
  "Apply dedupe PLAN to ROOT; with DRY-RUN change nothing.
Return (:resolved :merged :relinked :skipped :unwritable :vanished
:stale) counts.  A group with a copy in a file a command of ROOT may
not write, one of another repository's, is left as it is and counted
unwritable.  A dropped copy's ID passes to the kept copy where it has
none; else each \"id:\" link to it that the index records is pointed
at the kept copy's, and :relinked counts them.  A group with such a
link in a file ROOT may not write is left and counted unwritable too,
since cutting the copy would leave that link pointing at nothing."
  (pos-roam-with-index root
    (let ((resolved 0) (merged 0) (relinked 0) (vanished 0) (unlinkable 0)
          (corpus (pos-corpus root))
          (make-backup-files nil)
          (touched nil))
      ;; Every marker is resolved before any edit, so that a plan whose
      ;; lines have moved is found stale as a whole.
      (pcase-let ((`(,jobs . ,counts) (pos-dedupe--jobs root plan corpus)))
        (dolist (job jobs)
          (if (pos-dedupe--relinks-unwritable-p job root corpus)
              (setq unlinkable (1+ unlinkable))
            (let ((result (pos-dedupe--resolve job dry-run)))
              (setq resolved (+ resolved (plist-get result :resolved))
                    merged (+ merged (plist-get result :merged))
                    relinked (+ relinked (plist-get result :relinked))
                    vanished (+ vanished (plist-get result :vanished)))
              (dolist (buffer (plist-get result :touched)) (cl-pushnew buffer touched)))))
        (unless dry-run
          (dolist (job jobs)
            (dolist (marker (cons (cadr job) (mapcar #'car (cddr job))))
              (cl-pushnew (marker-buffer marker) touched)))
          (dolist (buffer touched)
            (with-current-buffer buffer
              (when (buffer-modified-p) (save-buffer)))))
        (list :resolved resolved :merged merged :relinked relinked
              :skipped (plist-get counts :skipped)
              :unwritable (+ unlinkable (plist-get counts :unwritable))
              :vanished vanished :stale (plist-get counts :stale))))))

(defun pos-dedupe-report (result dry-run)
  "Return a report of RESULT from `pos-dedupe-apply'; DRY-RUN words it."
  (format "%s %d duplicate group%s (%d merged, %d links relinked, %d skipped as undecided, %d with a copy the root may not write, %d gone with an earlier cut, %d stale)"
          (if dry-run "Would resolve" "Resolved")
          (plist-get result :resolved)
          (if (= 1 (plist-get result :resolved)) "" "s")
          (plist-get result :merged)
          (plist-get result :relinked)
          (plist-get result :skipped)
          (plist-get result :unwritable)
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
  "Where a heading belongs, as (REGEXP . FILE); the first match wins.
FILE is an Org file relative to the root, which a refile plan proposes
as the target of a heading REGEXP matches."
  :type '(alist :key-type regexp :value-type string)
  :group 'pos)

(defun pos-refile-suggest (heading)
  "Return the file `pos-refile-rules' suggests for HEADING, or nil."
  (let ((case-fold-search t))
    (cdr (seq-find (lambda (rule) (string-match-p (car rule) heading))
                   pos-refile-rules))))

(defun pos-refile-candidates (root)
  "Return (LINE HEADING SECTION SUBTREE) per level-two entry in ROOT's intray."
  (with-current-buffer (pos-visit (expand-file-name "intray.org" root))
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
                     (target (pos-refile-suggest heading)))
          (insert (format "| %s | %d | %s | %s | | %s |\n"
                          (if target "move" "?")
                          line
                          (replace-regexp-in-string "|" "/" heading)
                          (or target "")
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
Return (:moved :left :missing :unwritable :vanished) counts.  A row
whose target is a file a command of ROOT may not write, one of another
repository's, is left and counted unwritable."
  (let ((moved 0) (left 0) (missing 0) (unwritable 0) (vanished 0)
        (make-backup-files nil)
        (corpus (pos-corpus root))
        (intray (pos-visit (expand-file-name "intray.org" root)))
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
          (cond
           ((not (file-exists-p target-file))
            (setq missing (1+ missing)))
           ((not (pos-corpus-writable-p corpus target-file))
            (setq unwritable (1+ unwritable)))
           (t
            (with-current-buffer (pos-visit target-file)
              (let ((level (save-excursion (pos--goto-refile-target under))))
                (if (null level)
                    (setq missing (1+ missing))
                  (setq moved (1+ moved))
                  (unless dry-run
                    (with-current-buffer intray
                      (goto-char marker)
                      (org-cut-subtree))
                    (pos--goto-refile-target under)
                    (org-paste-subtree level))))))))))
    (unless dry-run
      (with-current-buffer intray (when (buffer-modified-p) (save-buffer)))
      (dolist (job jobs)
        (with-current-buffer (pos-visit (expand-file-name (nth 1 job) root))
          (when (buffer-modified-p) (save-buffer)))))
    (list :moved moved :left left :missing missing :unwritable unwritable
          :vanished vanished)))

(defun pos-refile-report (result dry-run)
  "Return a report of RESULT from `pos-refile-apply'; DRY-RUN words it."
  (format "%s %d intray entr%s (%d left, %d with a missing target, %d with a target the root may not write, %d no longer at their line)"
          (if dry-run "Would refile" "Refiled")
          (plist-get result :moved)
          (if (= 1 (plist-get result :moved)) "y" "ies")
          (plist-get result :left)
          (plist-get result :missing)
          (plist-get result :unwritable)
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
