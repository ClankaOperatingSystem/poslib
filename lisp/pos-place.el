;;; pos-place.el --- Place an item under a heading -*- lexical-binding: t; -*-

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

;; An item is placed where it belongs: its heading, properties, body
;; and children are moved to the end of a heading's children, in the
;; same file or another, with Org's own cut and paste.  The move is
;; recorded in the item's LOGBOOK drawer in Org's form for a refile,
;; with the place it came from.  An `id:' link to the item follows it,
;; since the ID moves with it.  A `file:' link that names the item by
;; its file and title does not, so each is reported.
;;
;; An item is named as `pos-set-state' names it.
;;
;; - `pos-place': command.
;; - `pos-place-stale-links': the links that name an item at a place.
;; - `pos-place-batch': shell entry; TARGET FILE UNDER [STATE].

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-corpus)
(require 'pos-state)

(defun pos-place--buffer (root corpus file)
  "Return a buffer visiting FILE of CORPUS, the corpus of ROOT, to write it.
Refuse a file the tree does not read or the root may not write,
symlinks, unsaved or stale buffers and foreign locks."
  (let ((name (file-relative-name file root)))
    (unless (member file (pos-corpus-files corpus))
      (user-error "Not a file the tree reads: %s" name))
    (unless (pos-corpus-writable-p corpus file)
      (user-error "Not a file this root may write: %s" name))
    (when (file-symlink-p file)
      (user-error "The file is a symlink: %s" name))
    (when (stringp (file-locked-p file))
      (user-error "%s is locked by another editor; save it there first" name))
    (with-current-buffer (pos-visit file)
      (when (buffer-modified-p)
        (user-error "Save the modified buffer of %s first" name))
      (unless (verify-visited-file-modtime (current-buffer))
        (user-error "%s changed on disk; revert its buffer first" name))
      (current-buffer))))

(defun pos-place--heading (under)
  "Return (MARKER . LEVEL) of the heading UNDER names in the current buffer.
UNDER is a slash-separated outline path, each name a heading's whole
title.  MARKER is at the heading and LEVEL is its level.  Return nil
if the path is absent."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((level 0) (bound (point-max)) (found t))
     (dolist (name (split-string under "/" t "[ \t]+"))
       (when found
         (setq level (1+ level))
         (if (re-search-forward
              (concat "^" (make-string level ?*) " +\\(?:"
                      (regexp-opt (pos-state--states)) " +\\)?"
                      (regexp-quote name) "\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$")
              bound t)
             (progn (beginning-of-line)
                    (setq bound (save-excursion (org-end-of-subtree t t) (point))))
           (setq found nil))))
     (when (and found (> level 0))
       (cons (point-marker) level)))))

(defun pos-place--log (from)
  "Record at point that the item was placed, having been at FROM.
The record is the one Org writes for a refile, with FROM as its note."
  (org-add-log-setup 'refile nil nil 'note)
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-setup nil
        org-log-post-message nil
        org-log-note-window-configuration (current-window-configuration))
  (move-marker org-log-note-return-to (point))
  ;; `org-store-log-note' reads the note from the current buffer and
  ;; kills it.
  (with-current-buffer (generate-new-buffer " *pos-place note*")
    (insert "From " from)
    (org-store-log-note)))

(defun pos-place-stale-links (root file title)
  "Return (FILE . LINE) for each link to the heading TITLE of FILE.
The links are those in the corpus of ROOT that name the heading by
file and title, [[file:FILE::*TITLE]], and those within FILE that name
it by title alone, [[*TITLE]].  An `id:' link is not one."
  (let ((file (expand-file-name file root))
        (link (concat "\\[\\[\\(?:file:\\([^]:]+\\)::\\)?\\*"
                      (regexp-quote title) "\\]"))
        places)
    (dolist (other (pos-corpus-files (pos-corpus root)))
      (with-current-buffer (pos-visit other)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward link nil t)
           (when (equal file (if (match-string 1)
                                 (expand-file-name (match-string 1)
                                                   (file-name-directory other))
                               other))
             (push (cons other (line-number-at-pos)) places))))))
    (nreverse places)))

(defun pos-place (root target file under &optional state)
  "Place the item TARGET names in the tree at ROOT under a heading of FILE.
TARGET is the item's ID, or FILE:LINE, as `pos-set-state' takes it.
FILE is relative to ROOT.  UNDER is the heading's outline path,
slash-separated, as \"Tasks\" or \"Tasks/House\"; the item becomes
the last of its children, with its properties, body and own
children.  With STATE, one of `pos-todo-keywords', the item is set
to it as `pos-set-state' does.  The move is recorded in the item's
LOGBOOK drawer with the place it came from.  Return (FILE LINE STATE
TITLE) as the item is afterwards.  Refuse a heading that is the item
or within it, a path FILE does not have, and what `pos-set-state'
refuses of either file."
  (when (and state (not (member state (pos-state--states))))
    (user-error "Unknown state: %s (one of %s)"
                state (string-join (pos-state--states) ", ")))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (corpus (pos-corpus root))
         (place (pos-state--place root corpus target))
         (to-file (expand-file-name file root))
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
         (org-enforce-todo-checkbox-dependencies nil)
         (kill-ring nil)
         (from (pos-place--buffer root corpus (car place)))
         (to (pos-place--buffer root corpus to-file))
         (heading (with-current-buffer to (pos-place--heading under))))
    (unless heading
      (user-error "No heading %s in %s" under file))
    (with-current-buffer from
      (org-with-wide-buffer
       (goto-char (point-min))
       (forward-line (1- (cdr place)))
       (unless (and (org-at-heading-p) (org-get-todo-state))
         (user-error "No item at %s:%d"
                     (file-relative-name (car place) root) (cdr place)))
       (let ((previous (org-get-todo-state))
             (end (save-excursion (org-end-of-subtree t t) (point)))
             (came-from (string-join
                         (cons (file-relative-name (car place) root)
                               (org-get-outline-path))
                         ", ")))
         (when (and (eq from to) (<= (point) (car heading)) (< (car heading) end))
           (user-error "The heading %s is the item or within it" under))
         (when (equal previous state)
           (user-error "The item is already %s" state))
         (unwind-protect
             (progn
               (lock-buffer)
               (with-current-buffer to (lock-buffer))
               (let ((changes (list (prepare-change-group from)
                                    (prepare-change-group to)))
                     done)
                 (unwind-protect
                     (progn
                       (mapc #'activate-change-group changes)
                       (when state
                         (org-todo state)
                         (org-back-to-heading t)
                         (pos-state--log state previous nil)
                         (org-back-to-heading t))
                       (pos-place--log came-from)
                       (org-back-to-heading t)
                       (org-cut-subtree)
                       (prog1
                           (with-current-buffer to
                             (org-with-wide-buffer
                              (goto-char (car heading))
                              (org-end-of-subtree t t)
                              (unless (bolp) (insert "\n"))
                              (org-paste-subtree (1+ (cdr heading)))
                              (org-back-to-heading t)
                              (prog1
                                  (list to-file (line-number-at-pos)
                                        (substring-no-properties
                                         (org-get-todo-state))
                                        (substring-no-properties
                                         (org-get-heading t t t t)))
                                ;; The file that gains the item is
                                ;; saved first: a failure between the
                                ;; two saves leaves it twice, not lost.
                                (save-buffer))))
                         (unless (eq from to) (save-buffer))
                         (setq done t)))
                   (mapc (if done #'accept-change-group #'cancel-change-group)
                         changes))))
           (unlock-buffer)
           (with-current-buffer to (unlock-buffer))
           (set-marker (car heading) nil)))))))

(defun pos-place-batch ()
  "Place an item of `pos-directory' and print it, then each stale link.
`command-line-args-left' is TARGET FILE UNDER [STATE], as `pos-place'
takes them.  The first line printed is FILE:LINE: STATE TITLE for the
item where it now is.  Each line after it is \"Link to the old place:
FILE:LINE\", one for each of `pos-place-stale-links', where the link
is after the move.  Exit 2 on any other arguments."
  (unless (memq (length command-line-args-left) '(3 4))
    (message "Usage: TARGET FILE UNDER [%s]"
             (string-join (pos-state--states) "|"))
    (kill-emacs 2))
  (pcase-let* ((`(,target ,to ,under ,state) command-line-args-left)
               (root (file-name-as-directory (expand-file-name pos-directory)))
               (old (pos-state--place root (pos-corpus root) target))
               (title (with-current-buffer (pos-visit (car old))
                        (org-with-wide-buffer
                         (goto-char (point-min))
                         (forward-line (1- (cdr old)))
                         (and (org-at-heading-p)
                              (substring-no-properties
                               (org-get-heading t t t t))))))
               (`(,file ,line ,now ,placed) (pos-place root target to under state)))
    (setq command-line-args-left nil)
    (princ (format "%s:%d: %s %s\n" file line now placed))
    (dolist (link (and title (pos-place-stale-links root (car old) title)))
      (princ (format "Link to the old place: %s:%d\n" (car link) (cdr link))))))

(provide 'pos-place)
;;; pos-place.el ends here
