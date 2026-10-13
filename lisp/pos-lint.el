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
;; returns findings, each (FILE LINE MESSAGE).  Nothing is written.
;;
;; - `pos-lint': the findings of every check, sorted by file and line;
;;   from M-x, shown in a compilation buffer.
;; - `pos-lint-checks': the checks made of each file.
;; - `pos-lint-repo-checks': the checks made of the whole tree.
;; - `pos-lint-format': findings as file:line: message lines.
;; - `pos-lint-batch': the command line.

;;; Code:

(require 'org)
(require 'seq)
(require 'cl-lib)
(require 'pos)
(require 'pos-corpus)
(require 'pos-node)

;;;; Checks of a file

(defun pos--finding (file message)
  "Return a finding for the heading at point in FILE with MESSAGE."
  (list file (line-number-at-pos) message))

(defun pos-lint-done-with-open-children (file)
  "Report done entries in FILE with open children."
  (pos-map-headings
   file
   (lambda ()
     (when (and (org-entry-is-done-p) (pos-open-descendant-p))
       (pos--finding file "done entry has open children")))))

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
         (pos--finding file (format "stale keyword %s (not in the sequence)" word)))))))

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
          (push (pos--finding file "#+TODO line overrides the one sequence; run pos-normalise-keywords")
                findings))
        (nreverse findings)))))

(defun pos-lint-merged-copy (file)
  "Report merged copies in FILE awaiting reconciliation."
  (pos-map-headings
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
    (pos-map-headings
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

;;;; Checks of the tree

(defun pos--task-headings (root)
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
            (seq-doseq (check (alist-get 'checks (or (pos-node-read-methodology-file dir)
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
          (pos-node-refused
           (push (list at 1 (format "declaration refused: %s: %s" (nth 1 err) (nth 2 err)))
                 findings)))))
    (nreverse findings)))

(autoload 'pos-person-lint "pos-person")

(defvar pos-lint-repo-checks
  '(pos-lint-duplicate-tasks
    pos-lint-methodology-checks
    pos-person-lint)
  "Whole-tree checks: root to findings.")

;;;; The lint

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

(provide 'pos-lint)
;;; pos-lint.el ends here
