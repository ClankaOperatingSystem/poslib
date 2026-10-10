;;; pos-capture.el --- Capture a task into an intray -*- lexical-binding: t; -*-

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

;; - `pos-capture': command.
;; - `pos-capture-batch': shell entry; reads POS_CAPTURE_ROOT,
;;   POS_CAPTURE_TITLE and POS_CAPTURE_BODY.

;;; Code:

(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'pos)

(defun pos-capture (root title &optional body)
  "File TITLE as a TODO under Unsorted in ROOT's intray; return its line.
The item has an ID property, a new Org ID, and a CREATED property, the
time of capture as an inactive timestamp.  BODY, if it is not nil or
blank, is the item's text, written after its properties.  Refuse bad
titles, a BODY with a line that begins with a star, which Org would
read as a heading, symlinks, unsaved or stale buffers and foreign
locks."
  (interactive "DRoot: \nsTask: ")
  (unless (and (stringp title)
               (not (string-empty-p (string-trim title)))
               (not (string-match-p "[[:cntrl:]]" title)))
    (user-error "Capture needs a nonempty, single-line title"))
  (when (and body (string-empty-p (string-trim body)))
    (setq body nil))
  (when body
    (when (string-match-p "^\\*" body)
      (user-error "A line of the body begins with a star"))
    (when (string-match-p "[^[:print:]\n\t]" body)
      (user-error "The body has a control character")))
  (let ((file (expand-file-name "intray.org" root))
        (enable-local-variables nil)
        (enable-local-eval nil)
        (make-backup-files nil)
        (auto-save-default nil)
        (create-lockfiles t))
    (unless (and (file-regular-p file) (not (file-symlink-p file)))
      (user-error "Intray must be an existing regular file, not a symlink"))
    (when (stringp (file-locked-p file))
      (user-error "Intray is locked by another editor; save it there first"))
    (with-current-buffer (pos-visit file)
      (when (buffer-modified-p)
        (user-error "Save the intray's modified buffer before capturing"))
      (unless (verify-visited-file-modtime (current-buffer))
        (user-error "Intray changed on disk; revert its buffer first"))
      (save-restriction
        (widen)
        (save-excursion
          (unwind-protect
              (progn
                (lock-buffer)
                (atomic-change-group
                  (pos-goto-unsorted-end)
                  (let ((line (line-number-at-pos)))
                    (insert "** TODO " (string-trim title) "\n")
                    (forward-line -1)
                    ;; Not `org-id-get-create', which also writes the
                    ;; ID to `org-id-locations-file'.
                    (org-entry-put (point) "ID" (org-id-new))
                    (org-entry-put (point) "CREATED"
                                   (format-time-string
                                    (org-time-stamp-format t t)))
                    (when body
                      (org-end-of-meta-data t)
                      (unless (bolp) (insert "\n"))
                      (insert (string-trim body) "\n"))
                    (save-buffer)
                    line)))
            (unlock-buffer)))))))

(defun pos-capture-batch ()
  "Capture POS_CAPTURE_TITLE into POS_CAPTURE_ROOT's intray.
POS_CAPTURE_BODY, if set, is the item's text."
  (let* ((root (getenv "POS_CAPTURE_ROOT"))
         (title (getenv "POS_CAPTURE_TITLE"))
         (line (pos-capture root title (getenv "POS_CAPTURE_BODY"))))
    (princ (format "%s:%d: TODO %s\n"
                   (expand-file-name "intray.org" root)
                   line (string-trim title)))))

(provide 'pos-capture)
;;; pos-capture.el ends here
