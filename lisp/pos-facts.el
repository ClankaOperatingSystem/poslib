;;; pos-facts.el --- What happened in a node since a commit -*- lexical-binding: t; -*-

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

;; What happened in a node since a commit, for a handover and for a
;; start-up that finds commits newer than the last handover: the
;; commits after it, each pushed or not, and the ledger events added
;; under the node, each committed or not.  Other sessions' commits and
;; seals of the same time are listed too; nothing here tells one
;; session's from another's.  Reads git and the ledgers; writes nothing.
;;
;; - `pos-facts-commits': the commits after a commit.
;; - `pos-facts-events': the ledger events added after a commit.
;; - `pos-facts-report': both, as text.
;; - `pos-facts-batch': shell entry; the commit on the command line.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-bytes)
(require 'pos-git)
(require 'pos-ledger)

(defun pos-facts-commit-p (root commit)
  "Return non-nil if the repository at ROOT has COMMIT."
  (and (pos-git-line root "rev-parse" "--verify" "--quiet" (concat commit "^{commit}"))
       t))

(defun pos-facts-commits (root since)
  "Return the commits of the repository at ROOT after SINCE, oldest first.
Each is an alist: hash; short, the abbreviated hash; subject; and
pushed, t when the upstream of HEAD has the commit, nil when it does
not, and :null when HEAD has no upstream."
  (let ((upstream (pos-git-line root "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}"))
        (log (pos-git-line root "log" "--reverse" "--format=%H %h %s" (concat since "..HEAD"))))
    (mapcar (lambda (line)
              (string-match "\\`\\([0-9a-f]+\\) \\([0-9a-f]+\\) \\(.*\\)\\'" line)
              (let ((hash (match-string 1 line)))
                `((hash . ,hash)
                  (short . ,(match-string 2 line))
                  (subject . ,(match-string 3 line))
                  (pushed . ,(if upstream
                                 (eq 0 (car (pos-git root "merge-base" "--is-ancestor"
                                                     hash upstream)))
                               :null)))))
            (split-string (or log "") "\n" t))))

(defun pos-facts--scope (root archive)
  "Return the path from ROOT of the scope ARCHIVE belongs to, \".\" for ROOT."
  (let ((scope (file-relative-name (file-name-directory (directory-file-name archive)) root)))
    (if (equal scope "./") "." (directory-file-name scope))))

(defun pos-facts-events (root since)
  "Return the ledger events under ROOT that SINCE does not have, in ledger order.
Each is an alist: scope, the path from ROOT of the archive's scope,
\".\" for ROOT; number, the event's; item, the path the event sealed,
or :null; and committed, whether the repository tracks the event's
file.  An event is after SINCE when that commit has no file at its
path."
  (let ((root (file-name-as-directory (file-truename (expand-file-name root))))
        events)
    (dolist (archive (pos-ledger-roots root))
      (dolist (file (pos-history-files (pos-ledger-history archive)))
        (let ((rel (file-relative-name file root)))
          (unless (eq 0 (car (pos-git root "cat-file" "-e" (concat since ":./" rel))))
            (push `((scope . ,(pos-facts--scope root archive))
                    (number . ,(string-to-number (file-name-nondirectory file)))
                    (item . ,(or (alist-get 'item (pos-bytes-parse (pos-bytes-read file)))
                                 :null))
                    (committed . ,(eq 0 (car (pos-git root "ls-files" "--error-unmatch"
                                                      "--" rel)))))
                  events)))))
    (nreverse events)))

(defun pos-facts-report (root since)
  "Return what happened in the node at ROOT since the commit SINCE, as text.
The commits after it, oldest first, each marked pushed or not pushed
by the upstream of HEAD; then the ledger events added, each committed
or not.  A list with nothing says (none)."
  (let ((commits (pos-facts-commits root since))
        (events (pos-facts-events root since)))
    (concat
     (format "Commits after %s\n" (pos-git-line root "log" "-1" "--format=%h %s" since))
     (if commits
         (mapconcat (lambda (commit)
                      (let-alist commit
                        (format "  %s %s%s\n" .short .subject
                                (cond ((eq .pushed t) " (pushed)")
                                      ((null .pushed) " (not pushed)")
                                      (t "")))))
                    commits "")
       "  (none)\n")
     (and commits (eq (alist-get 'pushed (car commits)) :null)
          "  HEAD has no upstream: whether a commit is pushed is not known\n")
     "\nSealed after it\n"
     (if events
         (mapconcat (lambda (event)
                      (let-alist event
                        (format "  %s event %d: %s (%s)\n" .scope .number
                                (if (eq .item :null) "no item" .item)
                                (if .committed "committed" "not committed"))))
                    events "")
       "  (none)\n"))))

(defun pos-facts-batch ()
  "Print `pos-facts-report' of `pos-directory' since the commit named.
The commit is the one argument on the command line.  Exit 2 without
one commit named that the repository has, and 1 when a ledger
refuses."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (unless (and (= (length args) 1) (pos-facts-commit-p pos-directory (car args)))
      (message "Usage: SINCE, a commit the repository has")
      (kill-emacs 2))
    (condition-case err
        (princ (pos-facts-report pos-directory (car args)))
      (pos-ledger-refused
       (message "%s: %s" (nth 1 err) (nth 2 err))
       (kill-emacs 1)))))

(provide 'pos-facts)
;;; pos-facts.el ends here
