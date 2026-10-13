;;; pos-git.el --- Git, run for a tool -*- lexical-binding: t; -*-

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

;; Git run in a directory, for the tree tool and for the claims a seal
;; makes: what it printed, and whether it succeeded.  Nothing here
;; decides what to run.
;;
;; - `pos-git': run git; its status and what it printed.
;; - `pos-git-line': what git printed, or nil if it failed.

;;; Code:

(require 'subr-x)

(defun pos-git (dir &rest args)
  "Run git in DIR with ARGS; return (STATUS . OUTPUT).
OUTPUT is what git printed, less its final newline."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (status (apply #'process-file "git" nil (list t nil) nil args)))
      (cons status (string-trim-right (buffer-string) "\n")))))

(defun pos-git-line (dir &rest args)
  "Return what git printed in DIR with ARGS, or nil if it failed."
  (let ((result (apply #'pos-git dir args)))
    (and (eq (car result) 0) (cdr result))))

(provide 'pos-git)
;;; pos-git.el ends here
