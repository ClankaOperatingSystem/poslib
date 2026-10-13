;;; pos-cli.el --- The command lines of the archive tools  -*- lexical-binding: t; -*-

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

;; The three programs a shell runs: `pos-seal-batch', the archive
;; tools, `pos-tree-batch', the tree tool, and `pos-migrate-batch',
;; the migration.  Each is run by Emacs in batch, which loads the
;; module and calls the program: -l MODULE -f NAME COMMAND ..., as
;; pyposlib's documents name them, and each module autoloads its
;; program from here.  What a command does is the work
;; of the module it calls; what is here is the reading of the command
;; line, the printing of what comes back, and the exit: 0 done or
;; clean, 1 findings or something to do, 2 refused.
;;
;; - `pos-seal-batch', `pos-tree-batch', `pos-migrate-batch': the programs.
;; - `pos-cli-run': run a command, with every refusal printed and exit 2.

;;; Code:

(require 'seq)
(require 'pos-bytes)
(require 'pos-checkpoint)
(require 'pos-index)
(require 'pos-keep)
(require 'pos-ledger)
(require 'pos-links)
(require 'pos-migrate)
(require 'pos-referrers)
(require 'pos-relink)
(require 'pos-salvage)
(require 'pos-seal)
(require 'pos-search)
(require 'pos-signin)
(require 'pos-tree)

;;;; What the programs share

(defun pos-cli-run (function)
  "Call FUNCTION with the arguments of the command line, taken up.
A refusal, of an archive, a configuration or a plan, is printed as
KIND: MESSAGE and the program exits 2; so is a plan that is not
readable JSON."
  (condition-case err
      (funcall function (prog1 command-line-args-left
                          (setq command-line-args-left nil)))
    ((pos-ledger-refused pos-node-refused pos-tree-refused)
     (message "%s: %s" (nth 1 err) (nth 2 err))
     (kill-emacs 2))
    (json-error
     (message "plan: Not a readable plan")
     (kill-emacs 2))))

(defun pos-cli--usage (usage)
  "Print USAGE as a message and exit 2: the command line was not understood."
  (message "%s" usage)
  (kill-emacs 2))

(defun pos-cli--read-plan (file)
  "Return the plan in FILE, or on standard input for -."
  (pos-bytes-parse (pos-bytes-read (if (equal file "-") "/dev/stdin" file))))

(defun pos-cli--print-planned (plan apply)
  "Print PLAN; with APPLY, apply it first and print plan, hash, event and root.
A program applying its own plan at once hashes it as a reviewer
would, so that `pos-seal-apply' takes it."
  (pos-bytes-print
   (if apply
       (let* ((hash (pos-bytes-sha (pos-bytes-json plan)))
              (result (pos-seal-apply plan hash)))
         `((plan . ,plan) (hash . ,hash)
           (event . ,(car result)) (root . ,(cdr result))))
     plan)))

;;;; The archive tools

(defconst pos-cli-seal-usage
  "Usage: COMMAND ...  (help prints this; Emacs itself takes --help)

  seal SOURCE DESTINATION [--apply]
      print the plan to seal SOURCE at DESTINATION, in an archive
  write-new DESTINATION [--links-from DIR] [--apply]
      print the plan to seal a new record, read from standard input;
      its links read as written from DESTINATION, or from DIR
  salvage ROOT SOURCE [--dry-run]
      copy each open task of SOURCE, a file or directory about to be
      sealed, to the intray of the tree at ROOT, and close the
      original; run before the seal is planned
  relink ROOT PLAN
      print the plan to rewrite the links to the item PLAN seals, a
      seal plan in a file, in the Org files of the tree at ROOT; made
      before PLAN is applied, and applied after it
  apply PLAN HASH
      apply a reviewed plan; PLAN is a file holding the plan exactly
      as seal, write-new or relink printed it, and HASH is the SHA-256
      of that file's bytes in lower-case hex, as sha256sum PLAN prints it
  check ROOT
      report every archive under ROOT, as JSON
  checkpoint ROOT
      record the ledger heads under ROOT, once the check is clean
  repair ROOT
      restore recorded directories and read bits; remove write bits; never enrols
  convert ROOT
      bring each schema 2 ledger under ROOT to schema 3, once it is clean
  keep ROOT
      move to its keeper each archive under ROOT a keeper is to keep
  recall ROOT
      bring back from its keeper each archive under ROOT a keeper keeps
  link PATH
      print the ipfs:// link to PATH, a sealed path in an archive
  fetch LINK
      print the bytes of the archived file LINK names, an ipfs:// link,
      from a scope above the current directory: on disk or its keeper's
  links-into ROOT PATH
      print each link in the Org files of the tree at ROOT that leads
      to PATH or into it, by file or by ID, as FILE:LINE: LINK -> FILE
  search ROOT QUERY [--mode MODE] [--limit N] [--within CID]
      print each line QUERY is found on in the archives under ROOT, on
      disk or at their keepers, as LINK:LINE:TEXT; exit 1 with none
  sign-in URL
      sign in to the keeper at URL, in a browser, and keep the token

--apply applies a program's own plan at once and prints both.
Exit 0 done or clean, 1 findings, 2 refused.
"
  "The usage of `pos-seal-batch'.")

(defun pos-seal-batch ()
  "Run a command from `command-line-args-left', as in `pos-cli-seal-usage'."
  (pos-cli-run
   (lambda (args)
     (pcase args
       (`("seal" ,source ,destination . ,rest)
        (unless (member rest '(nil ("--apply")))
          (pos-cli--usage "Usage: seal SOURCE DESTINATION [--apply]"))
        (pos-cli--print-planned (pos-seal-plan source destination) rest))
       (`("write-new" ,destination . ,rest)
        (let ((links-from (and (equal (car rest) "--links-from") (cadr rest))))
          (when links-from (setq rest (cddr rest)))
          (unless (and (member rest '(nil ("--apply")))
                       (or links-from (not (member "--links-from" rest))))
            (pos-cli--usage "Usage: write-new DESTINATION [--links-from DIR] [--apply]"))
          (pos-cli--print-planned
           (pos-seal-stage (pos-bytes-read "/dev/stdin") destination nil links-from)
           rest)))
       (`("salvage" ,root ,source . ,rest)
        (unless (member rest '(nil ("--dry-run")))
          (pos-cli--usage "Usage: salvage ROOT SOURCE [--dry-run]"))
        (let ((root (file-name-as-directory (expand-file-name root))))
          (condition-case err
              (princ (pos-salvage-report
                      root (pos-salvage root source (and rest t)) (and rest t)))
            (user-error
             (message "salvage: %s" (error-message-string err))
             (kill-emacs 2)))))
       (`("relink" ,root ,plan-file)
        (pos-bytes-print (pos-relink-plan root (pos-cli--read-plan plan-file))))
       (`("apply" ,plan-file ,hash)
        (let ((plan (pos-cli--read-plan plan-file)))
          (if (equal (alist-get 'operation plan) "relink")
              (pos-bytes-print `((rewritten . ,(vconcat (pos-relink-apply plan hash)))))
            (let ((result (pos-seal-apply plan hash)))
              (pos-bytes-print `((event . ,(car result)) (root . ,(cdr result))))))))
       (`("check" ,root)
        (let ((report (vconcat (pos-ledger-check root))))
          (pos-bytes-print report)
          (kill-emacs (if (pos-checkpoint-findings-p report) 1 0))))
       (`("checkpoint" ,root)
        (princ (pos-bytes-json `((checkpointed . ,(pos-checkpoint-record root))))))
       (`("repair" ,root)
        (princ (pos-bytes-json (pos-checkpoint-repair root))))
       (`("convert" ,root)
        (pos-bytes-print (pos-keep-convert root)))
       (`("keep" ,root)
        (pos-bytes-print (pos-keep root)))
       (`("recall" ,root)
        (pos-bytes-print (pos-keep-recall root)))
       (`("link" ,path)
        (princ (pos-links-link path))
        (terpri))
       (`("fetch" ,link)
        ;; The file's bytes as they are: `princ' would encode them.
        (send-string-to-terminal (pos-index-bytes link)))
       (`("links-into" ,root ,path)
        (pos-referrers-print (file-name-as-directory (expand-file-name root)) path))
       (`("search" ,root ,query . ,options)
        (let ((status (pos-search-command (cons root (cons query options)))))
          (unless status
            (pos-cli--usage
             "Usage: search ROOT QUERY [--mode MODE] [--limit N] [--within CID]"))
          (kill-emacs status)))
       (`("sign-in" ,url)
        (princ (json-serialize (pos-signin url) :false-object :false))
        (terpri))
       (`(,(or "help" "-h" "--help")) (princ pos-cli-seal-usage))
       (_ (pos-cli--usage pos-cli-seal-usage))))))

;;;; The tree tool

(defconst pos-cli-tree-usage
  "Usage: COMMAND ...  (help prints this; Emacs itself takes --help)

  plan ROOT [SOURCE]
      print what needs to be done for the tree at ROOT to be as its
      configurations declare, as JSON; change nothing.  SOURCE is the
      directory of skills and commands to install; with none, nothing
      is installed
  apply ROOT PLAN [SOURCE]
      do what the plan in the file PLAN holds, or - for standard input,
      if the tree at ROOT still gives it with SOURCE; print the plan
      that remains
  archives ROOT
      update only the managed archive rules in Git's info/exclude;
      print the remaining plan; do not clone repositories, install or
      link
  install ROOT SOURCE
      install from SOURCE and bring the links to it up to date; print
      the remaining plan; do not clone repositories or change archive
      rules

Exit 0 nothing to do, 1 something to do or to report, 2 refused.
"
  "The usage of `pos-tree-batch'.")

(defun pos-cli--print-tree-plan (plan)
  "Print PLAN as JSON and exit, with 0 for an empty plan and 1 otherwise."
  (pos-bytes-print plan)
  (kill-emacs (if (and (seq-empty-p (alist-get 'actions plan))
                       (seq-empty-p (alist-get 'findings plan)))
                  0
                1)))

(defun pos-tree-batch ()
  "Run a command from `command-line-args-left', as in `pos-cli-tree-usage'."
  (pos-cli-run
   (lambda (args)
     (pcase args
       ((or `("plan" ,root) `("plan" ,root ,source))
        (pos-cli--print-tree-plan (pos-tree-plan root source)))
       (`("archives" ,root)
        (pos-cli--print-tree-plan (pos-tree-ignore-archives root)))
       (`("install" ,root ,source)
        (pos-cli--print-tree-plan (pos-tree-install root source)))
       ((or `("apply" ,root ,file) `("apply" ,root ,file ,source))
        (pos-cli--print-tree-plan
         (pos-tree-apply root (pos-cli--read-plan file) source)))
       (`(,(or "help" "-h" "--help")) (princ pos-cli-tree-usage))
       (_ (pos-cli--usage pos-cli-tree-usage))))))

;;;; The migration

(defun pos-migrate-batch ()
  "Run a migration command from `command-line-args-left'.
plan ARCHIVE [ROOT [DIR...]] prints a plan, each DIR excluding a
candidate collection, or with a leading + including one; apply PLAN HASH
applies it.  Exit 0 done, 2 refused."
  (pos-cli-run
   (lambda (args)
     (pcase args
       (`("plan" ,archive . ,rest)
        ;; After ROOT, +DIR includes a collection and DIR excludes one.
        (let ((names (cdr rest)))
          (pos-bytes-print
           (pos-migrate-plan
            archive (car rest)
            (seq-remove (lambda (n) (string-prefix-p "+" n)) names) nil nil
            (mapcar (lambda (n) (substring n 1))
                    (seq-filter (lambda (n) (string-prefix-p "+" n)) names))))))
       (`("apply" ,plan-file ,hash)
        (let ((result (pos-migrate-apply (pos-cli--read-plan plan-file) hash)))
          (pos-bytes-print `((event . ,(car result)) (root . ,(cdr result))))))
       (_ (pos-cli--usage "Usage: plan ARCHIVE [ROOT [DIR... +DIR...]] | apply PLAN HASH"))))))

(provide 'pos-cli)
;;; pos-cli.el ends here
