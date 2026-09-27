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
;; - `pos-capture-batch': shell entry; reads POS_CAPTURE_ROOT, POS_CAPTURE_TITLE.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'pos)

(defun pos-capture (root title)
  "File TITLE as a TODO under Unsorted in ROOT's intray; return its line.
Refuse bad titles, symlinks, unsaved or stale buffers and foreign locks."
  (interactive "DRoot: \nsTask: ")
  (unless (and (stringp title)
               (not (string-empty-p (string-trim title)))
               (not (string-match-p "[[:cntrl:]]" title)))
    (user-error "Capture needs a nonempty, single-line title"))
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
    (with-current-buffer (find-file-noselect file)
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
                    (save-buffer)
                    line)))
            (unlock-buffer)))))))

(defun pos-capture-batch ()
  "Capture POS_CAPTURE_TITLE into POS_CAPTURE_ROOT's intray."
  (let* ((root (getenv "POS_CAPTURE_ROOT"))
         (title (getenv "POS_CAPTURE_TITLE"))
         (line (pos-capture root title)))
    (princ (format "%s:%d: TODO %s\n"
                   (expand-file-name "intray.org" root)
                   line (string-trim title)))))

(provide 'pos-capture)
;;; pos-capture.el ends here
