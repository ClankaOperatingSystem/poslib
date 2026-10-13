;;; pos-refile.el --- Refile the intray by a reviewed plan  -*- lexical-binding: t -*-

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

;; The entries of the root's intray.org, moved to the files they
;; belong in by a plan that is reviewed and then applied.  The plan is
;; Org text: a row for each entry, with the target `pos-refile-rules'
;; suggests.
;;
;; - `pos-refile-rules': where a heading belongs, set in pos-config.el.
;; - `pos-refile-candidates', `pos-refile-suggest': the intray's
;;   entries, and the file suggested for a heading.
;; - `pos-refile-plan', `pos-refile-apply': the two steps.
;; - `pos-refile-read-plan': a reviewed plan, read from its file.
;; - `pos-refile-report': what applying a plan did, in a sentence.
;; - `pos-refile-plan-batch', `pos-refile-apply-batch': the command
;;   line.

;;; Code:

(require 'org)
(require 'seq)
(require 'cl-lib)
(require 'pos)
(require 'pos-corpus)

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

(defun pos-refile--excerpt (subtree)
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
              "- Then: pos-refile apply.\n\n"
              "| act | line | heading | file | under | excerpt |\n|-\n")
      (dolist (entry candidates)
        (pcase-let* ((`(,line ,heading ,_section ,subtree) entry)
                     (target (pos-refile-suggest heading)))
          (insert (format "| %s | %d | %s | %s | | %s |\n"
                          (if target "move" "?")
                          line
                          (replace-regexp-in-string "|" "/" heading)
                          (or target "")
                          (pos-refile--excerpt subtree)))))
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

(defun pos-refile--goto-refile-target (under)
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
          (let ((marker (pos-marker-at root "intray.org" line
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
              (let ((level (save-excursion (pos-refile--goto-refile-target under))))
                (if (null level)
                    (setq missing (1+ missing))
                  (setq moved (1+ moved))
                  (unless dry-run
                    (with-current-buffer intray
                      (goto-char marker)
                      (org-cut-subtree))
                    (pos-refile--goto-refile-target under)
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

(provide 'pos-refile)
;;; pos-refile.el ends here
