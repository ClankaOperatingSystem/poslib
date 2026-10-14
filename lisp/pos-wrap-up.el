;;; pos-wrap-up.el --- Keep a session's handover -*- lexical-binding: t; -*-

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

;; The source's filling of the wrap-up slot, doc/pos-slots.txt section
;; 3: a session's handover sealed in the node's archive, and a heading
;; for it in the node's journal.org, tagged the latest, as section 4 of
;; that document has the journal.
;;
;; - `pos-wrap-up': seal a handover and index it; return its citation.
;; - `pos-wrap-up-slug': the file name a title gives.
;; - `pos-wrap-up-batch': shell entry; reads POS_WRAP_UP_ROOT,
;;   POS_WRAP_UP_TITLE and POS_WRAP_UP_DATE, and the handover from
;;   `pos-wrap-up-input'; exits 1 when it refuses.

;;; Code:

(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'pos)
(require 'pos-bytes)
(require 'pos-ledger)
(require 'pos-seal)

(defvar pos-wrap-up-input "/dev/stdin"
  "The file `pos-wrap-up-batch' reads the handover from.")

(defun pos-wrap-up-slug (title)
  "Return TITLE as a file name: lower case, each other run one hyphen.
Letters and digits stay; each run of anything else becomes one
hyphen, and none is left at either end.  A title with no letter or
digit gives \"handover\"."
  (let ((slug (string-trim (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title))
                           "-+" "-+")))
    (if (string-empty-p slug) "handover" slug)))

(defun pos-wrap-up--date (date)
  "Return DATE, YYYY-MM-DD, as an inactive timestamp with its weekday.
Refuse what is not a date of that form."
  (unless (and (stringp date)
               (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date))
    (user-error "The date is not YYYY-MM-DD: %s" date))
  (pcase-let* ((`(,year ,month ,day) (mapcar #'string-to-number (split-string date "-")))
               (time (encode-time (list 0 0 0 day month year nil -1 nil))))
    (unless (equal date (format-time-string "%Y-%m-%d" time))
      (user-error "Not a date: %s" date))
    (format-time-string (org-time-stamp-format nil 'inactive) time)))

(defun pos-wrap-up--bytes (body)
  "Return BODY, the handover, as bytes; refuse an empty one."
  (when (or (null body) (string-empty-p (string-trim body)))
    (user-error "Wrap-up needs the handover on its input"))
  (if (multibyte-string-p body) (encode-coding-string body 'utf-8) body))

(defun pos-wrap-up--seal (root title bytes date)
  "Seal BYTES as the handover TITLE of DATE in ROOT's archive; return its citation.
The record is journal/DATE-SLUG.org in the archive at ROOT's archives/,
made if it is absent, so a node's first handover opens its archive."
  (let* ((archive (expand-file-name "archives" root))
         (rel (format "journal/%s-%s.org" date (pos-wrap-up-slug title)))
         (plan (progn (make-directory archive t)
                      (pos-seal-stage bytes (expand-file-name rel archive))))
         (added (seq-find (lambda (pair) (equal (pos-bytes-key (car pair)) rel))
                          (alist-get 'add plan))))
    (pos-seal-apply plan (pos-bytes-sha (pos-bytes-json plan)))
    (concat "ipfs://" (alist-get 'cid (cdr added)))))

(defun pos-wrap-up--index (buffer title stamp citation)
  "Add to BUFFER, a journal.org to write, the heading of a handover.
TITLE is its heading, before the file's first heading, tagged
`pos-handover-tag' and `pos-latest-tag', with an ID, STAMP as its
`pos-handover-date-property' and CITATION as its
`pos-handover-record-property'.  The latest tag leaves the heading
that had it.  Return the new heading's line."
  (pos-writing buffer
    (org-map-entries
     (lambda ()
       (when (member pos-latest-tag (org-get-tags nil t))
         (org-toggle-tag pos-latest-tag 'off)))
     nil 'file)
    (goto-char (point-min))
    (if (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))
    (let ((line (line-number-at-pos)))
      (insert "* " (string-trim title) "\n")
      (forward-line -1)
      (org-set-tags (list pos-handover-tag pos-latest-tag))
      ;; Not `org-id-get-create', which also writes the ID to
      ;; `org-id-locations-file'.
      (org-entry-put (point) "ID" (pos-new-id))
      (org-entry-put (point) pos-handover-date-property stamp)
      (org-entry-put (point) pos-handover-record-property citation)
      line)))

(defun pos-wrap-up (root title body &optional date)
  "Keep BODY as the handover TITLE of the node at ROOT; return its citation.
BODY is sealed in ROOT's archive as journal/DATE-SLUG.org, and a heading
for it is added to ROOT's journal.org, made if absent, tagged the
latest.  DATE is YYYY-MM-DD, today by default.  Refuse a TITLE that is
not one nonempty line, an empty BODY, a DATE not of that form, a
journal.org that cannot be written, and what the archive refuses;
nothing is written then."
  (unless (pos-one-line-p title)
    (user-error "Wrap-up needs a nonempty, single-line title"))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (date (or date (format-time-string "%Y-%m-%d")))
         (stamp (pos-wrap-up--date date))
         (bytes (pos-wrap-up--bytes body))
         (journal (expand-file-name "journal.org" root))
         (had (file-exists-p journal))
         (buffer (and had (pos-buffer-to-write root journal)))
         (citation (pos-wrap-up--seal root title bytes date)))
    (unless had
      (pos-bytes-write journal (encode-coding-string "#+TITLE: Journal\n\n" 'utf-8))
      (setq buffer (pos-buffer-to-write root journal)))
    (pos-wrap-up--index buffer title stamp citation)
    citation))

(defun pos-wrap-up-batch ()
  "Keep the handover on `pos-wrap-up-input' for POS_WRAP_UP_ROOT.
POS_WRAP_UP_TITLE is its title and POS_WRAP_UP_DATE its date, today
when unset or empty.  Print \"CITATION: TITLE, DATE\".  A refusal
prints its reason and exits 1."
  (condition-case err
      (let* ((root (getenv "POS_WRAP_UP_ROOT"))
             (title (or (getenv "POS_WRAP_UP_TITLE") ""))
             (given (getenv "POS_WRAP_UP_DATE"))
             (date (if (member given '(nil "")) (format-time-string "%Y-%m-%d") given))
             (body (pos-bytes-read pos-wrap-up-input)))
        (princ (format "%s: %s, %s\n" (pos-wrap-up root title body date)
                       (string-trim title) date)))
    (user-error
     (message "%s" (error-message-string err))
     (kill-emacs 1))
    (pos-ledger-refused
     (message "%s: %s" (nth 1 err) (nth 2 err))
     (kill-emacs 1))))

(provide 'pos-wrap-up)
;;; pos-wrap-up.el ends here
