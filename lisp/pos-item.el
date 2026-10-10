;;; pos-item.el --- Set an item's dates and tags -*- lexical-binding: t; -*-

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

;; A task's scheduled date and deadline are set with Org's own
;; commands, `org-schedule' and `org-deadline', and its tags with
;; `org-set-tags', so that the planning line and the heading are
;; written as Org writes them.  Nothing is recorded in LOGBOOK.
;;
;; An item is named as `pos-set-state' names it: by its ID, or by its
;; file and the line of its heading.
;;
;; - `pos-set-date', `pos-set-tags': commands.
;; - `pos-item-batch': shell entry; schedule, deadline and tag.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-state)

(defconst pos-item--date-regexp
  (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
          "\\(?: \\([.+]?\\+[0-9]+[hdwmy]\\)\\)?\\'")
  "A regexp matching a date, YYYY-MM-DD, and a repeater after it if any.")

(defun pos-item--date (date)
  "Return (DAY . REPEATER) of DATE, or refuse it.
DATE is YYYY-MM-DD, with an Org repeater such as +1w after a space if
wanted.  DAY is the date, and REPEATER the repeater or nil."
  (unless (string-match pos-item--date-regexp date)
    (user-error (concat "Not a date: %s (YYYY-MM-DD, then a repeater"
                        " such as +1w if wanted, or none)")
                date))
  (let ((year (string-to-number (match-string 1 date)))
        (month (string-to-number (match-string 2 date)))
        (day (string-to-number (match-string 3 date)))
        (repeater (match-string 4 date)))
    (unless (and (<= 1 month 12)
                 (<= 1 day (date-days-in-month year month)))
      (user-error "No such day: %s" (substring date 0 10)))
    (cons (substring date 0 10) repeater)))

(defun pos-item--planning (keyword)
  "Return the timestamp after KEYWORD on the item at point's planning line.
KEYWORD is `org-scheduled-string' or `org-deadline-string'.  Nil if
the item has none."
  (save-excursion
    (org-back-to-heading t)
    (forward-line)
    (when (and (org-at-planning-p)
               (re-search-forward
                (concat (regexp-quote keyword) " *\\(<[^>\n]+>\\)")
                (line-end-position) t))
      (match-string-no-properties 1))))

(defun pos-set-date (root target kind date)
  "Set the KIND date of the item TARGET names in the tree at ROOT to DATE.
TARGET is as `pos-set-state' takes it.  KIND is \"scheduled\" or
\"deadline\".  DATE is YYYY-MM-DD, with a repeater such as +1w after
a space if wanted, or \"none\", which removes the date.  A date given
without a repeater keeps the repeater the item has, as Org does.
The change is made by `org-schedule' or `org-deadline'.  Return (FILE
LINE STATE TITLE TIMESTAMP) as the item is afterwards, TIMESTAMP nil
when it has no such date.  Refuse what `pos-set-state' refuses."
  (unless (member kind '("scheduled" "deadline"))
    (user-error "Unknown date: %s (scheduled or deadline)" kind))
  (let* ((set (if (equal kind "scheduled") #'org-schedule #'org-deadline))
         (keyword (if (equal kind "scheduled")
                      org-scheduled-string
                    org-deadline-string))
         (parsed (and (not (equal date "none")) (pos-item--date date)))
         (org-log-reschedule nil)
         (org-log-redeadline nil))
    (pos-state--at-item
     root target "setting a date"
     (lambda (file)
       (cond
        ((null parsed) (funcall set '(4)))
        ((cdr parsed)
         ;; Org keeps an item's repeater when it sets a date.  Remove
         ;; the date first, so that the one given is the one written.
         (funcall set '(4))
         (funcall set nil (car parsed))
         (org-back-to-heading t)
         (forward-line)
         (re-search-forward (concat (regexp-quote keyword) " *<[^>\n]+")
                            (line-end-position))
         (insert " " (cdr parsed)))
        (t (funcall set nil (car parsed))))
       (org-back-to-heading t)
       (list file (line-number-at-pos)
             (substring-no-properties (org-get-todo-state))
             (substring-no-properties (org-get-heading t t t t))
             (pos-item--planning keyword))))))

(defun pos-set-tags (root target specs)
  "Add and remove tags on the item TARGET names in the tree at ROOT.
TARGET is as `pos-set-state' takes it.  SPECS is a list of strings,
each +TAG, which adds the tag, or -TAG, which removes it; a tag is
letters, digits, and any of _ @ # %.  Adding a tag the item has and
removing one it lacks change nothing.  The heading is written by
`org-set-tags'.  Return (FILE LINE STATE TITLE TAGS), TAGS the item's
own tags afterwards.  Refuse what `pos-set-state' refuses."
  (unless specs
    (user-error "No tag to add or remove"))
  (dolist (change specs)
    (unless (string-match-p "\\`[-+][[:alnum:]_@#%]+\\'" change)
      (user-error "Not +TAG or -TAG: %s" change)))
  (pos-state--at-item
   root target "setting tags"
   (lambda (file)
     (let ((tags (mapcar #'substring-no-properties (org-get-tags nil t))))
       (dolist (change specs)
         (let ((tag (substring change 1)))
           (if (string-prefix-p "+" change)
               (unless (member tag tags)
                 (setq tags (append tags (list tag))))
             (setq tags (remove tag tags)))))
       (org-set-tags tags)
       (org-back-to-heading t)
       (list file (line-number-at-pos)
             (substring-no-properties (org-get-todo-state))
             (substring-no-properties (org-get-heading t t t t))
             tags)))))

(defun pos-item-batch ()
  "Set a date or the tags of an item of `pos-directory' and print the item.
`command-line-args-left' is one of
  schedule TARGET DATE
  deadline TARGET DATE
  tag TARGET SPEC...
as `pos-set-date' and `pos-set-tags' take them.  Exit 2 on any other
arguments."
  (pcase command-line-args-left
    (`(,(and command (or "schedule" "deadline")) ,target ,date)
     (setq command-line-args-left nil)
     (pcase-let ((`(,file ,line ,state ,title ,stamp)
                  (pos-set-date
                   pos-directory target
                   (if (equal command "schedule") "scheduled" "deadline")
                   date)))
       (princ (format "%s:%d: %s %s%s\n" file line state title
                      (if stamp
                          (format " %s %s" (if (equal command "schedule")
                                               org-scheduled-string
                                             org-deadline-string)
                                  stamp)
                        "")))))
    (`("tag" ,target . ,(and specs (guard specs)))
     (setq command-line-args-left nil)
     (pcase-let ((`(,file ,line ,state ,title ,tags)
                  (pos-set-tags pos-directory target specs)))
       (princ (format "%s:%d: %s %s%s\n" file line state title
                      (if tags (format " :%s:" (string-join tags ":")) "")))))
    (_
     (message (concat "Usage: schedule TARGET DATE|none\n"
                      "       deadline TARGET DATE|none\n"
                      "       tag TARGET +TAG|-TAG ..."))
     (kill-emacs 2))))

(provide 'pos-item)
;;; pos-item.el ends here
