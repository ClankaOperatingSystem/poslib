;;; pos-state.el --- Set the state of an item -*- lexical-binding: t; -*-

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

;; A task's state is changed with Org's own command, `org-todo', so
;; that what Org does on a change is done: a closing date on a done
;; state, and on a repeating item the date moved on by the item's own
;; repeater and the state put back.  Each change is recorded in the
;; item's LOGBOOK drawer in Org's form, with a note when one is given.
;;
;; An item is named by its ID, or by its file and the line of its
;; heading.
;;
;; - `pos-set-state': command.
;; - `pos-state-batch': shell entry; TARGET STATE [NOTE].

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-corpus)

(defun pos-state--states ()
  "Return the states of `pos-todo-keywords', open and done."
  (remove "|" (cdr (car pos-todo-keywords))))

(defun pos-state--find-id (corpus id)
  "Return (FILE . LINE) of the heading in CORPUS whose ID property is ID.
Refuse an ID no heading has, and one that two headings have."
  (let ((property (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id)))
        places)
    (dolist (file (pos-corpus-files corpus))
      (with-current-buffer (pos-visit file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward property nil t)
           (unless (org-before-first-heading-p)
             (save-excursion
               (org-back-to-heading t)
               (push (cons file (line-number-at-pos)) places)))))))
    (pcase places
      ('nil (user-error "No heading has the ID %s" id))
      (`(,place) place)
      (_ (user-error "The ID %s is on %d headings: %s" id (length places)
                     (mapconcat (lambda (place)
                                  (format "%s:%d" (car place) (cdr place)))
                                (nreverse places) ", "))))))

(defun pos-state--place (root corpus target)
  "Return (FILE . LINE) that TARGET names in CORPUS, the corpus of ROOT.
TARGET is FILE:LINE, with FILE relative to ROOT, or an ID."
  (if (string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" target)
      (let ((file (expand-file-name (match-string 1 target) root))
            (line (string-to-number (match-string 2 target))))
        (unless (member file (pos-corpus-files corpus))
          (user-error "Not a file the tree reads: %s" (match-string 1 target)))
        (cons file line))
    (pos-state--find-id corpus target)))

(defun pos-state--log (state previous note)
  "Record at point the change from PREVIOUS to STATE, with NOTE if non-nil.
The record is the one Org writes for a state change, where
`org-log-into-drawer' says.  Org takes a note in a buffer of its own
after the command ends; here the note is given, so it is stored at
once."
  (org-add-log-setup 'state state previous (if note 'note 'time))
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-setup nil
        org-log-note-window-configuration (current-window-configuration))
  (move-marker org-log-note-return-to (point))
  ;; `org-store-log-note' reads the note from the current buffer and
  ;; kills it.
  (with-current-buffer (generate-new-buffer " *pos-state note*")
    (insert (or note ""))
    (org-store-log-note))
  (setq org-log-post-message nil))

(defun pos-set-state (root target state &optional note)
  "Set the item TARGET names in the tree at ROOT to STATE, with NOTE.
TARGET is the item's ID, or FILE:LINE, the file relative to ROOT and
the line of the heading.  STATE is one of `pos-todo-keywords'.  NOTE
is one line, or nil.  The change is made by `org-todo' and recorded
in the item's LOGBOOK drawer.  Return (FILE LINE STATE TITLE) as the
item is afterwards; a repeating item set to a done state is open
again, on its next date.  Refuse a file the root may not write,
symlinks, unsaved or stale buffers and foreign locks."
  (unless (member state (pos-state--states))
    (user-error "Unknown state: %s (one of %s)"
                state (string-join (pos-state--states) ", ")))
  (when (and note (or (string-empty-p (string-trim note))
                      (string-match-p "[[:cntrl:]]" note)))
    (user-error "A note is one nonempty line"))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (corpus (pos-corpus root))
         (place (pos-state--place root corpus target))
         (file (car place))
         (enable-local-variables nil)
         (enable-local-eval nil)
         (make-backup-files nil)
         (auto-save-default nil)
         (create-lockfiles t)
         (vc-handled-backends nil)
         (org-log-done 'time)
         (org-log-repeat 'time)
         (org-log-into-drawer t)
         (org-todo-repeat-to-state nil)
         (org-enforce-todo-dependencies nil)
         (org-enforce-todo-checkbox-dependencies nil))
    (unless (pos-corpus-writable-p corpus file)
      (user-error "Not a file this root may write: %s"
                  (file-relative-name file root)))
    (when (file-symlink-p file)
      (user-error "The file is a symlink: %s" (file-relative-name file root)))
    (when (stringp (file-locked-p file))
      (user-error "The file is locked by another editor; save it there first"))
    (with-current-buffer (pos-visit file)
      (when (buffer-modified-p)
        (user-error "Save the file's modified buffer before setting a state"))
      (unless (verify-visited-file-modtime (current-buffer))
        (user-error "The file changed on disk; revert its buffer first"))
      (org-with-wide-buffer
       (goto-char (point-min))
       (forward-line (1- (cdr place)))
       (unless (and (org-at-heading-p) (org-get-todo-state))
         (user-error "No item at %s:%d"
                     (file-relative-name file root) (cdr place)))
       (let ((previous (org-get-todo-state)))
         (when (equal previous state)
           (user-error "The item is already %s" state))
         (unwind-protect
             (progn
               (lock-buffer)
               (atomic-change-group
                 (org-todo state)
                 (org-back-to-heading t)
                 (pos-state--log state previous (and note (string-trim note)))
                 (org-back-to-heading t)
                 (save-buffer)
                 (list file (line-number-at-pos)
                       (substring-no-properties (org-get-todo-state))
                       (substring-no-properties (org-get-heading t t t t)))))
           (unlock-buffer)))))))

(defun pos-state-batch ()
  "Set the state of an item of `pos-directory' and print the item.
`command-line-args-left' is TARGET STATE [NOTE], as `pos-set-state'
takes them.  Exit 2 on any other arguments."
  (unless (memq (length command-line-args-left) '(2 3))
    (message "Usage: TARGET %s [NOTE]" (string-join (pos-state--states) "|"))
    (kill-emacs 2))
  (pcase-let* ((`(,target ,state ,note) command-line-args-left)
               (`(,file ,line ,now ,title)
                (pos-set-state pos-directory target state note)))
    (setq command-line-args-left nil)
    (princ (format "%s:%d: %s %s\n" file line now title))))

(provide 'pos-state)
;;; pos-state.el ends here
