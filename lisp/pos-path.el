;;; pos-path.el --- Paths beneath a directory -*- lexical-binding: t; -*-

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

;; The two rules for a path written in a ledger or a configuration.
;;
;; - `pos-path-safe-p': whether a path is relative and only goes down.
;; - `pos-path-within-p': whether a path is another or lies beneath it.

;;; Code:

(require 'seq)

(defun pos-path-safe-p (path)
  "Return non-nil if PATH is a plain relative path beneath a directory.
It has no empty part, no part that is . or .., and no backslash."
  (and (not (string-empty-p path))
       (not (string-match-p "\\\\" path))
       (not (seq-some (lambda (part) (member part '("" "." "..")))
                      (split-string path "/")))))

(defun pos-path-within-p (path container)
  "Return non-nil if PATH is CONTAINER or beneath it."
  (or (equal path container) (string-prefix-p (concat container "/") path)))

(provide 'pos-path)
;;; pos-path.el ends here
