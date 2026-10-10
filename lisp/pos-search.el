;;; pos-search.el --- Where a query is found in an archive -*- lexical-binding: t; -*-

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

;; Searching an archive, as section 12 of doc/remote-archive-protocol.txt
;; specifies: where a query is found, as hits that name a file by its
;; item's ipfs:// link, a range of lines in it and the passage there.
;; In lockstep with pyposlib's search module.
;;
;; - `pos-search-disk': grep's behaviour over an archive on disk, behind
;;   the same port a keeper answers; `pos-remote-search' is the port for
;;   a keeper.
;; - `pos-search-found': the hits of a query over sources, in the
;;   archive's order, which is what every adapter here answers with;
;;   `pos-search-matcher', `pos-search-lines' and `pos-search-reference'
;;   are its parts.
;; - `pos-search-scope': every archive under a root, each through the
;;   adapter that reaches it.
;; - `pos-search-command': the command line's search, which
;;   `pos-seal-batch' runs.
;;
;; A hit is an alist as the wire has it: ref, range with lines a vector
;; of the first and last line, and passage.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pos-bytes)
(require 'pos-ledger)
(require 'pos-cid)
(require 'pos-remote)

(defconst pos-search-modes '("literal" "regex")
  "The modes this library matches itself; a keeper may declare more.")

(defconst pos-search-limit 1000
  "Hits answered where no limit is asked for; the protocol wants at least 100.")

;;;; Matching

(defun pos-search--regexp (expression)
  "Return EXPRESSION, a POSIX extended regular expression, in Emacs syntax.
As `grep -E' takes one: a group, an alternative and an interval are
written bare, and escaped to be literal, which is the other way about
in Emacs.  A bracket expression is the same in both.  Signal
`invalid-regexp' for one Emacs does not take."
  (let ((out nil) (i 0) (n (length expression)) in-bracket)
    (while (< i n)
      (let ((c (aref expression i)))
        (cond
         (in-bracket
          (push c out)
          (cond ((and (= c ?\[) (< (1+ i) n) (= (aref expression (1+ i)) ?:))
                 ;; A class such as [:alpha:], whose ] ends no bracket.
                 (let ((end (string-search ":]" expression i)))
                   (when end
                     (push (substring expression (1+ i) (+ end 2)) out)
                     (setq i (1+ end)))))
                ((and (= c ?\]) (not (eq (car (cdr out)) ?\[))
                      (not (and (eq (car (cdr out)) ?^) (eq (car (cddr out)) ?\[))))
                 (setq in-bracket nil))))
         ((= c ?\\)
          (setq i (1+ i))
          (when (< i n)
            (let ((escaped (aref expression i)))
              (if (memq escaped '(?\( ?\) ?| ?{ ?}))
                  (push escaped out)
                (push ?\\ out)
                (push escaped out)))))
         ((= c ?\[) (push c out) (setq in-bracket t))
         ((memq c '(?\( ?\) ?| ?{ ?})) (push ?\\ out) (push c out))
         (t (push c out))))
      (setq i (1+ i)))
    (let ((regexp (apply #'concat (mapcar (lambda (part) (if (stringp part) part (string part)))
                                          (nreverse out)))))
      (string-match-p regexp "")
      regexp)))

(defun pos-search-matcher (query mode &optional modes)
  "Return a function of a line: whether QUERY hits it in MODE, one of MODES.
Literal: QUERY's characters occur in it as given.  Regex: QUERY is a
POSIX extended regular expression found within it.  Refuse `mode' for
a mode not in MODES, `pos-search-modes' by default, and `request' for
an expression that does not parse."
  (let ((modes (or modes pos-search-modes)))
    (unless (member mode modes)
      (pos-ledger--refuse 'mode "Not a mode this keeper searches in, %s: %s" modes mode))
    (if (equal mode "literal")
        (lambda (line) (string-search query line))
      (let ((regexp (condition-case err
                        (pos-search--regexp query)
                      (invalid-regexp
                       (pos-ledger--refuse 'request "Not a regular expression: %s"
                                           (error-message-string err))))))
        (lambda (line) (string-match-p regexp line))))))

(defun pos-search-lines (bytes)
  "Return the lines of BYTES as the protocol counts them, or nil.
BYTES decoded as UTF-8 and split at LF, a terminator after the last
line making no line of its own.  Nil where BYTES is not UTF-8, which
no text mode searches."
  (let ((text (decode-coding-string bytes 'utf-8)))
    (unless (seq-some (lambda (c) (>= c #x3fff80)) text)
      (let ((lines (split-string text "\n")))
        (when (equal (car (last lines)) "")
          (setq lines (butlast lines)))
        lines))))

(defun pos-search--hits-in (bytes ref hit-p)
  "Return each hit in BYTES, the file REF names, that HIT-P takes.
One for each line, its range that line and its passage the line without
its terminator."
  (let ((number 0) hits)
    (dolist (line (pos-search-lines bytes))
      (setq number (1+ number))
      (when (funcall hit-p line)
        (push `((ref . ,ref) (range . ((lines . [,number ,number]))) (passage . ,line))
              hits)))
    (nreverse hits)))

(defun pos-search-found (sources query &optional mode limit modes)
  "Return the hits of QUERY over SOURCES, in their order.
Each source is (REF . READ), READ a function of no arguments giving the
file's bytes; the order is the archive's, by path bytewise, which the
protocol wants of literal and regex.  MODE is literal where nil; LIMIT,
a positive integer, keeps the first hits, `pos-search-limit' where nil;
MODES is what is served, `pos-search-modes' by default."
  (unless (and (stringp query) (not (string-empty-p query)))
    (pos-ledger--refuse 'request "Expected q, the query"))
  (let ((limit (or limit pos-search-limit)))
    (unless (and (integerp limit) (> limit 0))
      (pos-ledger--refuse 'request "limit is a positive integer: %S" limit))
    (let ((hit-p (pos-search-matcher query (or mode "literal") modes))
          hits (left limit))
      (cl-loop for (ref . read) in sources
               while (> left 0)
               do (dolist (hit (pos-search--hits-in (funcall read) ref hit-p))
                    (when (> left 0)
                      (push hit hits)
                      (setq left (1- left)))))
      (nreverse hits))))

;;;; Naming

(defun pos-search-by-path (paths)
  "Return PATHS in the archive's order: bytewise."
  (sort (copy-sequence paths) #'pos-bytes-utf8<))

(defun pos-search-reference (path cids items collections)
  "Return the ipfs:// link to the enrolled file at PATH, from CIDS.
As a sealed item's links name it (doc/formats.org, \"Links\"): the CID
of the item among ITEMS that holds it, else of the collection among
COLLECTIONS, else of the file itself, from CIDS, an alist of path and
CID, then the path beneath that holder where the file is not the whole."
  (let ((holder (or (seq-find (lambda (h) (pos-ledger--within-p path h))
                              (append items collections))
                    path)))
    (concat "ipfs://" (cdr (assoc holder cids))
            (unless (equal holder path)
              (concat "/" (substring path (1+ (length holder))))))))

(defun pos-search-beneath (cids within)
  "Return a function of a path: whether it is WITHIN, a CID of CIDS, or beneath.
Beneath a directory that is.  Refuse `absent' where nothing in CIDS has it."
  (let ((bases (mapcar #'car (seq-filter (lambda (pair) (equal (cdr pair) within)) cids))))
    (unless bases
      (pos-ledger--refuse 'absent "The ledger enrols nothing under %s" within))
    (lambda (path)
      (seq-some (lambda (base) (or (equal base ".") (pos-ledger--within-p path base)))
                bases))))

;;;; On disk

(defun pos-search-disk (archive query &optional mode limit within)
  "Return where QUERY is found in ARCHIVE, on disk, in MODE.
What grep finds there, each hit a reference instead of a path, so that
an agent at a shell gets from the archive what it would have got from
grep and can cite it.  Literal and regex.  At most LIMIT hits, beneath
the CID WITHIN where it is given.

The CIDs a reference names come from the ledger's fold, as a kept
archive's do, with no file hashed; a ledger whose entries record no
CID, one of schema 1, is hashed from disk as link hashes it."
  (pcase-let* ((`(,entries ,_ ,_ ,_ ,_ ,collections ,items ,empty) (pos-ledger-history archive))
               (cids (condition-case nil
                         (pos-ledger-fold entries empty)
                       (pos-ledger-refused (pos-cid-tree archive))))
               (chosen (if within (pos-search-beneath cids within) (lambda (_) t))))
    (pos-search-found
     (mapcar (lambda (path)
               (cons (pos-search-reference path cids items collections)
                     (lambda () (pos-bytes-read (expand-file-name path archive)))))
             (seq-filter (lambda (path) (and (assoc path cids) (funcall chosen path)))
                         (pos-search-by-path (mapcar (lambda (pair) (pos-bytes-key (car pair)))
                                                     entries))))
     query mode limit pos-search-modes)))

;;;; A scope

(defun pos-search-scope (root query &optional mode limit within)
  "Return the hits of QUERY in each archive under ROOT, as (ARCHIVE . HITS).
In the order check finds them.  An archive on disk is searched here and
one a keeper keeps by its keeper, made of its URL with
`pos-remote-keeper-function'; a keeper that declares no search is
refused as `absent', naming the archive, before anything is searched.
LIMIT caps the hits of all together.  With WITHIN, an archive that
enrols nothing under it gives nothing, and it is refused as `absent'
only where none does."
  (let ((left limit) (enrolled (null within)) answers)
    (cl-loop for archive in (pos-ledger-roots root)
             while (or (null left) (> left 0))
             do (let* ((url (pos-ledger-kept archive))
                       (keeper (and url (funcall pos-remote-keeper-function url))))
                  (when keeper
                    (let ((search (alist-get 'search (pos-remote-describe keeper))))
                      (when (or (null search) (eq search :null))
                        (pos-ledger--refuse 'absent "The keeper of %s does not search: %s"
                                            archive url))))
                  (let ((hits (condition-case err
                                  (if keeper
                                      (append (alist-get 'hits (pos-remote-search
                                                                keeper query mode left within))
                                              nil)
                                    (pos-search-disk archive query mode left within))
                                (pos-ledger-refused
                                 (if (and within (eq (nth 1 err) 'absent))
                                     :nothing
                                   (signal (car err) (cdr err)))))))
                    (unless (eq hits :nothing)
                      (setq enrolled t)
                      (when hits
                        (push (cons archive hits) answers)
                        (when left (setq left (- left (length hits)))))))))
    (unless enrolled
      (pos-ledger--refuse 'absent "No archive under %s enrols anything under %s" root within))
    (nreverse answers)))

;;;; The command

(defun pos-search-command (args)
  "Run search ROOT QUERY [--mode MODE] [--limit N] [--within CID], ARGS.
Print each hit of every archive under ROOT, a line of the passage at a
time as LINK:LINE:TEXT, grep's shape with a reference for the path.
Return 0 with hits, 1 with none; nil where ARGS are not the command's,
for the caller to print the usage."
  (catch 'usage
    (unless (and (>= (length args) 2) (cl-evenp (length args)))
      (throw 'usage nil))
    (let ((root (nth 0 args)) (query (nth 1 args)) options (rest (cddr args)))
      (while rest
        (let ((name (pop rest)) (value (pop rest)))
          (unless (and (member name '("--mode" "--limit" "--within"))
                       (not (assoc name options)))
            (throw 'usage nil))
          (push (cons name value) options)))
      (let ((limit (cdr (assoc "--limit" options))))
        (when limit
          (unless (and (string-match-p "\\`[0-9]+\\'" limit) (> (string-to-number limit) 0))
            (pos-ledger--refuse 'request "limit is a positive integer: %s" limit))
          (setq limit (string-to-number limit)))
        (let ((answers (pos-search-scope root query (cdr (assoc "--mode" options)) limit
                                         (cdr (assoc "--within" options)))))
          (dolist (answer answers)
            (dolist (hit (cdr answer))
              (let ((first (aref (alist-get 'lines (alist-get 'range hit)) 0))
                    (offset 0))
                (dolist (line (split-string (alist-get 'passage hit) "\n"))
                  (princ (format "%s:%d:%s\n" (alist-get 'ref hit) (+ first offset) line))
                  (setq offset (1+ offset))))))
          (if answers 0 1))))))

(provide 'pos-search)
;;; pos-search.el ends here
