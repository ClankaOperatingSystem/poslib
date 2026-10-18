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

;; The two rules for a path written in a ledger or a configuration, and
;; where a node's configuration is, as doc/pos-directory.txt names it.
;;
;; - `pos-path-safe-p': whether a path is relative and only goes down.
;; - `pos-path-within-p': whether a path is another or lies beneath it.
;; - `pos-path-config-paths': every path a node's configuration may have.
;; - `pos-path-config-files': the configuration files a node has.
;; - `pos-path-config-directories': the names a configuration directory
;;   may have.
;; - `pos-path-config-names': the names a configuration file may have
;;   within its directory.
;; - `pos-path-archive-scope': the scope an archive belongs to.
;; - `pos-path-archive-paths': where a scope's archive may lie, relative
;;   to the scope.

;;; Code:

(require 'seq)

;;;; The rules

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

;;;; Where a node's configuration is

(defconst pos-path-config-directories '(".clanka" ".clankos" ".pos")
  "The names a configuration directory may have.")

(defconst pos-path-config-names '("config.yaml" "config.yml")
  "The names a configuration file may have within its directory.")

(defun pos-path-config-paths ()
  "Return every path a node's configuration may have, relative to the node."
  (mapcan (lambda (directory)
            (mapcar (lambda (name) (concat directory "/" name))
                    pos-path-config-names))
          pos-path-config-directories))

(defun pos-path-config-files (dir)
  "Return the configuration files the node at DIR has, relative to it.
One, as doc/pos-directory.txt allows, or none; two are refused by
whoever reads them."
  (seq-filter (lambda (file) (file-exists-p (expand-file-name file dir)))
              (pos-path-config-paths)))

;;;; Where a scope's archive is

(defun pos-path-archive-paths ()
  "Return every path a scope's archive may have, relative to the scope.
The archive in each configuration directory, in the order of
`pos-path-config-directories', then the visible archives of the
legacy layout."
  (append (mapcar (lambda (directory) (concat directory "/archives"))
                  pos-path-config-directories)
          '("archives")))

(defun pos-path-archive-scope (archive)
  "Return the scope ARCHIVE, a directory named archives, belongs to.
The directory holding it, or, where that is a configuration directory,
the directory holding that.  The value has no final slash."
  (let ((parent (directory-file-name
                 (file-name-directory (directory-file-name archive)))))
    (if (member (file-name-nondirectory parent) pos-path-config-directories)
        (directory-file-name (file-name-directory parent))
      parent)))

(provide 'pos-path)
;;; pos-path.el ends here
