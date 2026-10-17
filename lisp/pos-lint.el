;;; pos-lint.el --- Lint a tree's Org files  -*- lexical-binding: t -*-

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

;; The lint of a tree's Org files: the corpus, pos-corpus.el.  A check
;; returns findings, each a `pos-finding'.  Nothing is written.
;;
;; - `pos-lint': the findings of every check, sorted by file and line;
;;   from M-x, shown in a compilation buffer.
;; - `pos-lint-checks': the checks made of each file.
;; - `pos-lint-repo-checks': the checks made of the whole tree.
;; - `pos-lint-format': findings as file:line: message lines.
;; - `pos-lint-batch': the command line.
;; - `pos-lint-done-with-open-children': report done entries in FILE
;;   with open children.
;; - `pos-lint-stale-keywords': retired keywords; headings starting with
;;   one are plain text.
;; - `pos-lint-stale-keyword': report headings in FILE that start with a
;;   retired keyword.
;; - `pos-lint-todo-line': report #+TODO lines in FILE, each as `pos-
;;   next-todo-line' has it.
;; - `pos-lint-merged-copy': report merged copies in FILE awaiting
;;   reconciliation.
;; - `pos-lint-clarified-in-intray': report items under Unsorted in
;;   FILE, an intray.org, that are not TODO.
;; - `pos-lint-waiting-without-who-or-when': report WAITING items in
;;   FILE that do not say who or what, or since when.
;; - `pos-lint-file-id': report FILE if it has no file-level ID.
;; - `pos-lint-duplicate-tasks': report task headings in more than one
;;   file of ROOT's corpus.
;; - `pos-lint-shared-file-ids': report files of ROOT's corpus that
;;   share a file-level ID.
;; - `pos-lint-methodology-checks': run each check of each methodology
;;   of ROOT's projects; return findings.
;; - `pos-lint-skills-lock': report each entry of a skills-lock.json in
;;   ROOT's tree that is undeclared.

;;; Code:

(require 'org)
(require 'seq)
(require 'cl-lib)
(require 'pos)
(require 'pos-corpus)
(require 'pos-lock)
(require 'pos-node)

;;;; Checks of a file

(defun pos-lint--finding (file message)
  "Return a finding for the heading at point in FILE with MESSAGE."
  (pos-finding-make :file file :line (line-number-at-pos) :message message))

(defun pos-lint-done-with-open-children (file)
  "Report done entries in FILE with open children."
  (pos-map-headings
   file
   (lambda ()
     (when (and (org-entry-is-done-p) (pos-open-descendant-p))
       (pos-lint--finding file "done entry has open children")))))

(defconst pos-lint-stale-keywords
  '("BACKLOG" "WIP" "BLOCKED" "CANCELED" "CLARIFY" "DELEGATE" "DELEGATED"
    "MAYBE")
  "Retired keywords; headings starting with one are plain text.")

(defun pos-lint-stale-keyword (file)
  "Report headings in FILE that start with a retired keyword."
  (pos-map-headings
   file
   (lambda ()
     (let ((word (car (split-string (org-get-heading t t t t)))))
       (when (member word pos-lint-stale-keywords)
         (pos-lint--finding file (format "stale keyword %s (not in the sequence)" word)))))))

(defun pos-lint-todo-line (file)
  "Report #+TODO lines in FILE, each as `pos-next-todo-line' has it.
None is reported in a file the root may not write, by
`pos-visit-own-keywords-p': the line is that file's own to keep, and
`pos-normalise-keywords' could not remove it."
  (with-current-buffer (pos-visit file)
    (save-excursion
      (goto-char (point-min))
      (let ((findings nil))
        (while (and (not (pos-visit-own-keywords-p file))
                    (pos-next-todo-line))
          (push (pos-lint--finding file "#+TODO line overrides the one sequence; run pos-normalise-keywords")
                findings))
        (nreverse findings)))))

(defun pos-lint-merged-copy (file)
  "Report merged copies in FILE awaiting reconciliation."
  (pos-map-headings
   file
   (lambda ()
     (when (string-prefix-p "Merged copy from " (org-get-heading t t t t))
       (pos-lint--finding file "merged copy awaiting reconciliation with its parent")))))

(defun pos-lint-clarified-in-intray (file)
  "Report items under Unsorted in FILE, an intray.org, that are not TODO.
An intray holds what is captured and not yet clarified.  An item that
is NEXT, WAITING or SOMEDAY has been clarified and belongs where its
work is; a done one is finished and waits to be retired."
  (when (string= "intray.org" (file-name-nondirectory file))
    (pos-map-headings
     file
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when (and state (not (string= state "TODO"))
                    (string= "Unsorted" (or (car (org-get-outline-path)) "")))
           (pos-lint--finding
            file
            (format (if (org-entry-is-done-p)
                        "%s item in the intray: finished, to be retired"
                      "%s item in the intray: clarified, to be placed")
                    state))))))))

(defun pos-lint--entry-text ()
  "Return the text of the entry at point, without its children.
The planning line and the drawers are part of it."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (buffer-substring-no-properties
     (point) (save-excursion (outline-next-heading) (point)))))

(defun pos-lint-waiting-without-who-or-when (file)
  "Report WAITING items in FILE that do not say who or what, or since when.
An item says who or what it waits on in a `pos-waiting-on-property'
or DELEGATED_TO property, or in its own text.  It says since when
in the record of its change to WAITING, as `pos-set-state' writes
it, or in a date in its own text, a timestamp or YYYY-MM-DD."
  (pos-map-headings
   file
   (lambda ()
     (when (equal (org-get-todo-state) "WAITING")
       (let* ((text (pos-lint--entry-text))
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
               (who (pos-lint--finding
                     file "WAITING item does not say since when"))
               (since (pos-lint--finding
                       file "WAITING item does not say who or what it waits on"))
               (t (pos-lint--finding
                   file (concat "WAITING item does not say who or what it"
                                " waits on, or since when")))))))))

(defun pos-lint--own-file-p (file)
  "Return non-nil if FILE is one the root may write.
A file of another repository of the tree, or of a product, is not."
  (not (pos-visit-own-keywords-p file)))

(defun pos-lint--file-id (file)
  "Return FILE's file-level ID, or nil if it has none or an empty one.
An ID in a heading's drawer, or a CUSTOM_ID, is not the file's."
  (with-current-buffer (pos-visit file)
    (save-excursion
      (goto-char (point-min))
      (let ((id (and (org-before-first-heading-p) (org-entry-get nil "ID"))))
        (and id (not (string-empty-p (string-trim id))) (string-trim id))))))

(defun pos-lint-file-id (file)
  "Report FILE if it has no file-level ID, at its first line.
A file the root may not write is not reported."
  (and (pos-lint--own-file-p file)
       (not (pos-lint--file-id file))
       (list (pos-finding-make :file file :line 1 :message "no file-level ID"))))

(defvar pos-lint-checks
  '(pos-lint-done-with-open-children
    pos-lint-stale-keyword
    pos-lint-todo-line
    pos-lint-merged-copy
    pos-lint-clarified-in-intray
    pos-lint-waiting-without-who-or-when
    pos-lint-file-id)
  "Per-file checks: file to findings.")

;;;; Checks of the tree

(defun pos-lint--task-headings (root)
  "Return (KEY FILE LINE) for keyword headings in the files of ROOT's corpus.
KEY is the lower-cased heading text."
  (mapcan (lambda (file)
            (pos-map-headings
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
    (dolist (group (seq-group-by #'car (pos-lint--task-headings root)))
      (let ((copies (cdr group)))
        (when (> (length copies) 1)
          (dolist (copy copies)
            (pcase-let ((`(,_ ,file ,line) copy))
              (push (pos-finding-make
                     :file file :line line
                     :message
                     (format "duplicate task (also %s)"
                                  (mapconcat
                                   (lambda (other)
                                     (pcase-let ((`(,_ ,ofile ,oline) other))
                                       (format "%s:%d"
                                               (file-relative-name ofile root) oline)))
                                  (remove copy copies) ", ")))
                    findings))))))
    (nreverse findings)))

(defun pos-lint-shared-file-ids (root)
  "Report files of ROOT's corpus that share a file-level ID.
Each file the root may write is reported, naming the others; a file
the root may not write is named and not reported."
  (let ((owners (make-hash-table :test 'equal)) (findings nil))
    (dolist (file (pos-files root))
      (let ((id (pos-lint--file-id file)))
        (when id (push file (gethash id owners)))))
    (maphash
     (lambda (id files)
       (when (cdr files)
         (setq files (sort files #'string<))
         (dolist (file files)
           (when (pos-lint--own-file-p file)
             (push (pos-finding-make
                    :file file :line 1
                    :message (format "file ID %s also in %s" id
                                     (mapconcat (lambda (other) (file-relative-name other root))
                                                (remove file files) ", ")))
                   findings)))))
     owners)
    (nreverse findings)))

(defun pos-lint--check-output (command project)
  "Run COMMAND, a check, in PROJECT; return (STATUS OUTPUT . ERRORS).
OUTPUT is what it printed on standard output, ERRORS on standard
error, trimmed.  Both are read as UTF-8, whatever the locale."
  (let ((errors (make-temp-file "pos-lint-"))
        (coding-system-for-read 'utf-8))
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

(defun pos-lint--skills (dir)
  "Return the skills of DIR, a methodology: each directory in its skills/.
A skill holds a SKILL.md.  Absolute, by name."
  (let ((skills (expand-file-name "skills" dir)))
    (and (file-directory-p skills)
         (seq-filter (lambda (skill)
                       (file-exists-p (expand-file-name "SKILL.md" skill)))
                     (directory-files skills t directory-files-no-dot-files-regexp)))))

(defun pos-lint--skill-checks (skill)
  "Return each scripts/check-* of SKILL, a skill's directory, by name.
Only an executable file is a check."
  (let ((scripts (expand-file-name "scripts" skill)))
    (and (file-directory-p scripts)
         (seq-filter (lambda (file)
                       (and (file-regular-p file) (file-executable-p file)))
                     (directory-files scripts t "\\`check-")))))

(defun pos-lint--run-check (command project at)
  "Run COMMAND, a check, in PROJECT; return its findings.
Each FILE:LINE: MESSAGE line it prints is a finding at that file,
relative to PROJECT, and any other line a finding at PROJECT.  A check
that exits other than 0 or 1 is a finding at AT, the skill's
directory, with what it printed on standard error."
  (pcase-let ((`(,status ,output . ,errors) (pos-lint--check-output command project)))
    (if (memq status '(0 1))
        (mapcar (lambda (line)
                  (if (string-match "\\`\\(.+?\\):\\([0-9]+\\): \\(.*\\)\\'" line)
                      (pos-finding-make
                       :file (expand-file-name (match-string 1 line) project)
                       :line (string-to-number (match-string 2 line))
                       :message (match-string 3 line))
                    (pos-finding-make :file (directory-file-name project)
                                      :line 1 :message line)))
                (split-string output "\n" t))
      (list (pos-finding-make
             :file (directory-file-name at) :line 1
             :message (format "check %s could not check: %s"
                              (file-name-nondirectory command) errors))))))

(defun pos-lint-methodology-checks (root)
  "Run each check of each methodology of ROOT's projects; return findings.
As doc/pos-methodology.txt section 3 has it: each executable
scripts/check-* of each skill of a mounted methodology, and of each
skill installed from a source the project declares, is run in the
project.  A declaration that names checks in bin/, which are not run,
and one that is refused, are each a finding at the methodology."
  (let (findings)
    (pcase-dolist (`(,scope ,_name . ,dir) (pos-corpus-methodologies (pos-corpus root)))
      (let ((at (directory-file-name dir))
            (project (pos-scope-dir scope)))
        (condition-case err
            (let ((checks (alist-get 'checks (pos-node-read-methodology-file dir))))
              (unless (seq-empty-p checks)
                (push (pos-finding-make
                       :file at :line 1
                       :message (concat "checks in bin/ are not run; move each"
                                        " to a skill's scripts/check-NAME: "
                                        (string-join (append checks nil) ", ")))
                      findings)))
          (pos-node-refused
           (push (pos-finding-make
                  :file at :line 1
                  :message (format "declaration refused: %s: %s" (nth 1 err) (nth 2 err)))
                 findings)))
        (dolist (skill (pos-lint--skills dir))
          (dolist (check (pos-lint--skill-checks skill))
            (setq findings (append (reverse (pos-lint--run-check check project skill))
                                   findings))))))
    (dolist (scope (pos-corpus-scopes (pos-corpus root)))
      (when-let* ((config (pos-scope-config scope)))
        (let ((project (pos-scope-dir scope)))
          (dolist (name (pos-lock-installed project config (pos-lint--image project)))
            (let ((skill (expand-file-name (concat ".agents/skills/" name) project)))
              (dolist (check (pos-lint--skill-checks skill))
                (setq findings (append (reverse (pos-lint--run-check check project skill))
                                       findings))))))))
    (nreverse findings)))

(defun pos-lint--image (dir)
  "Return the image's skills at DIR as a source, or nil.
DIR is a node's directory; where it is a repository's root with auto/
in its configuration directory, they are what auto/ holds, as
`pos-lock-image' gives them."
  (when-let* ((auto (pos-lock-auto dir)))
    (and (file-directory-p auto) (pos-lock-image auto dir))))

(defun pos-lint-skills-lock (root)
  "Report each entry of a skills-lock.json in ROOT's tree that is undeclared.
That is an entry of a skill its declared source's entry does not name,
as section 6 of doc/pos-directory.txt has it.  An entry of a source no
configuration declares is wild-caught, and not reported.  The finding
is at the entry's line."
  (let (findings)
    (dolist (scope (pos-corpus-scopes (pos-corpus root)))
      (when-let* ((config (pos-scope-config scope)))
        (let ((file (expand-file-name pos-lock-file (pos-scope-dir scope))))
          (dolist (difference (pos-lock-differences
                               (pos-scope-dir scope) config
                               (pos-lint--image (pos-scope-dir scope))))
            (let-alist difference
              (when (equal .finding "lock-undeclared")
                (push (pos-finding-make
                       :file file
                       :line (with-temp-buffer
                               (insert-file-contents file)
                               (if (search-forward (format "\"%s\":" .skill) nil t)
                                   (line-number-at-pos)
                                 1))
                       :message (concat "lock-undeclared: " .detail))
                      findings)))))))
    (nreverse findings)))

(autoload 'pos-person-lint "pos-person")

(defvar pos-lint-repo-checks
  '(pos-lint-duplicate-tasks
    pos-lint-shared-file-ids
    pos-lint-methodology-checks
    pos-lint-skills-lock
    pos-person-lint)
  "Whole-tree checks: root to findings.")

;;;; The lint

(defun pos-lint-format (findings root)
  "Render FINDINGS as \"file:line: message\" lines relative to ROOT."
  (mapconcat (lambda (finding) (pos-finding-format finding root)) findings ""))

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
                #'pos-finding<)))
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

(provide 'pos-lint)
;;; pos-lint.el ends here
