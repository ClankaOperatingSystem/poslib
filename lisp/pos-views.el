;;; pos-views.el --- The start-up views in a buffer -*- lexical-binding: t; -*-

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

;; The views a session opens with, pos-startup.el, shown in a buffer
;; for a person at the keyboard: the same text the report prints, made
;; by the same functions, with each line leading to what it is of.
;; A review can be read and acted on here with no program between.
;;
;; In the buffer:
;;
;;   RET   visit the item, or the scope, of the line
;;   t     set the item's state, as `pos-set-state' does
;;   g     read the files again
;;   v     show another view
;;   n, p  the next and the previous line that leads somewhere
;;   q     bury the buffer
;;
;; - `pos-views-show': show one view.
;; - `pos-views-weekly': show the views of a weekly review.

;;; Code:

(require 'org)
(require 'seq)
(require 'pos)
(require 'pos-startup)
(require 'pos-state)

(defvar-local pos-views--root nil
  "The root whose views this buffer shows.")

(defvar-local pos-views--views nil
  "The names of the views this buffer shows, in order.")

(defvar-keymap pos-views-mode-map
  :doc "Keymap for `pos-views-mode'."
  :parent special-mode-map
  "RET" #'pos-views-visit
  "t" #'pos-views-set-state
  "v" #'pos-views-show
  "n" #'pos-views-next
  "p" #'pos-views-previous)

(define-derived-mode pos-views-mode special-mode "POS-Views"
  "Major mode for the start-up views of a tree of responsibilities."
  (setq-local revert-buffer-function #'pos-views--revert)
  (setq truncate-lines t))

(defun pos-views--insert (root view)
  "Insert VIEW of ROOT at point, each line given what it is of.
A line that names an item or a scope has the text property pos-item:
the item or the scope's row, as `pos-startup-view-items' gives them.
The lines and the items are made in one reading of the files, and
are in the same order."
  (let* ((pos-startup--gathering t)
         (pos-startup--items nil)
         ;; Making a view visits files and shows agendas: it does not
         ;; leave the current buffer as it found it.
         (text (save-current-buffer (pos-startup-view root view)))
         (items (nreverse pos-startup--items))
         (start (point)))
    (insert text)
    (save-excursion
      (goto-char start)
      (while (and items (re-search-forward "^  [^ (\n]" nil t))
        (put-text-property (line-beginning-position) (line-end-position)
                           'pos-item (pop items))))))

(defun pos-views--render ()
  "Fill the current buffer with its views, read afresh."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (root pos-views--root))
    (erase-buffer)
    (dolist (view pos-views--views)
      (unless (bobp) (insert "\n"))
      (pos-views--insert root view))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun pos-views--revert (&rest _)
  "Read the files again; the `revert-buffer-function' of the mode."
  (pos-views--render))

(defun pos-views--buffer (root views)
  "Show VIEWS of ROOT in the views buffer; return the buffer."
  (let ((buffer (get-buffer-create "*pos-views*")))
    (with-current-buffer buffer
      (pos-views-mode)
      (setq pos-views--root (file-name-as-directory (expand-file-name root))
            pos-views--views views
            default-directory pos-views--root)
      (pos-views--render)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun pos-views-show (view &optional root)
  "Show VIEW, one of `pos-startup-views', of the tree at ROOT.
ROOT defaults to the root of the views buffer, else `pos-directory'."
  (interactive
   (list (completing-read "View: " pos-startup-views nil t)))
  (pos-views--buffer (or root pos-views--root pos-directory) (list view)))

;;;###autoload
(defun pos-views-weekly (&optional root)
  "Show the views of a weekly review of the tree at ROOT, in its order.
The views are `pos-startup-weekly-views'.  ROOT defaults to
`pos-directory'."
  (interactive)
  (pos-views--buffer (or root pos-directory) pos-startup-weekly-views))

(defun pos-views--item ()
  "Return what the current line is of, or refuse a line that is of nothing."
  (or (get-text-property (line-beginning-position) 'pos-item)
      (user-error "This line is of no item or scope")))

(defun pos-views--place (item)
  "Return (FILE . LINE) that ITEM leads to; LINE is nil for a scope's row.
An item leads to its heading.  A scope leads to its project.org, to
its one file, or to its directory."
  (let-alist item
    (if .file
        (cons (expand-file-name .file pos-views--root) .line)
      (let ((path (expand-file-name .scope pos-views--root)))
        (list (cond ((file-exists-p (expand-file-name "project.org" path))
                     (expand-file-name "project.org" path))
                    ((file-exists-p (concat path ".org")) (concat path ".org"))
                    (t path)))))))

(defun pos-views-visit ()
  "Visit the item of the current line, or the scope it names."
  (interactive)
  (pcase-let ((`(,file . ,line) (pos-views--place (pos-views--item))))
    (find-file-other-window file)
    (when line
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (when (derived-mode-p 'org-mode) (org-fold-show-context)))))

(defun pos-views-set-state (state &optional note)
  "Set the item of the current line to STATE, with NOTE, and read again.
As `pos-set-state' does it: the change is recorded in the item's
LOGBOOK drawer.  NOTE is one line, or empty for none."
  (interactive
   (list (completing-read "State: " (pos-state--states) nil t)
         (read-string "Note (empty for none): ")))
  (let-alist (pos-views--item)
    (unless .file (user-error "This line is of a scope, not an item"))
    (pos-set-state pos-views--root (format "%s:%d" .file .line) state
                   (and note (not (string-empty-p (string-trim note))) note)))
  (pos-views--render))

(defun pos-views-next ()
  "Move to the next line that is of an item or a scope."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'pos-item)))
      (forward-line 1))
    (when (eobp) (goto-char start))))

(defun pos-views-previous ()
  "Move to the previous line that is of an item or a scope."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'pos-item)))
      (forward-line -1))
    (unless (get-text-property (point) 'pos-item) (goto-char start))))

(provide 'pos-views)
;;; pos-views.el ends here
