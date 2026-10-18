;;; pos-path-test.el --- Tests for pos-path.el  -*- lexical-binding: t -*-

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

;; Run: make test.
;;
;; Each test is named for the rule it pins: the two rules for a path
;; written in a ledger or a configuration, and where a node's
;; configuration is.

;;; Code:

(require 'ert)
(require 'pos-path)
(require 'pos-test-support)

(ert-deftest pos-path/a-safe-path-is-relative-and-only-goes-down ()
  "A safe path has no empty part, no part that is . or .., and no backslash.
So it is relative, names something, and leads only down from the
directory it is written beneath."
  (dolist (path '("a" "a/b.org" "a b/c" ".hidden" "a..b/..c"))
    (ert-info (path) (should (pos-path-safe-p path))))
  (dolist (path '("" "/a" "a/" "a//b" "." "a/./b" ".." "../a" "a/.." "a\\b"))
    (ert-info (path) (should-not (pos-path-safe-p path)))))

(ert-deftest pos-path/a-path-is-within-its-container-or-is-it ()
  "A path is within a container when it is the container or lies beneath it.
A name that merely begins with the container's is not."
  (should (pos-path-within-p "a" "a"))
  (should (pos-path-within-p "a/b" "a"))
  (should (pos-path-within-p "a/b/c" "a/b"))
  (should-not (pos-path-within-p "ab" "a"))
  (should-not (pos-path-within-p "a" "a/b"))
  (should-not (pos-path-within-p "b/a" "a")))

(ert-deftest pos-path/a-configuration-may-be-at-one-of-six-paths ()
  "The configuration is config.yaml or config.yml in .clanka, .clankos or .pos.
The paths are listed in that order, directory by directory."
  (should (equal '(".clanka/config.yaml" ".clanka/config.yml"
                   ".clankos/config.yaml" ".clankos/config.yml"
                   ".pos/config.yaml" ".pos/config.yml")
                 (pos-path-config-paths))))

(ert-deftest pos-path/the-configuration-files-a-node-has-are-those-that-are-there ()
  "The files a node has are those of the six paths that exist, in that order.
None for a node with no configuration; two where two are written,
which whoever reads them refuses."
  (pos-test-with-files root '(("a.org" . ""))
    (should-not (pos-path-config-files root)))
  (pos-test-with-files root '((".pos/config.yaml" . "pos: 2\n"))
    (should (equal '(".pos/config.yaml") (pos-path-config-files root))))
  (pos-test-with-files root '((".pos/config.yml" . "pos: 2\n")
                              (".clanka/config.yaml" . "pos: 2\n"))
    (should (equal '(".clanka/config.yaml" ".pos/config.yml")
                   (pos-path-config-files root)))))

(ert-deftest pos-path/an-archive-belongs-to-the-scope-that-holds-it ()
  "An archive's scope is the directory holding it.
Where that is a configuration directory, it is the directory holding
that, so an archive in .clankos belongs to the same scope as one beside
it."
  (should (equal "/g/s" (pos-path-archive-scope "/g/s/archives")))
  (should (equal "/g/s" (pos-path-archive-scope "/g/s/archives/")))
  (dolist (directory '(".clanka" ".clankos" ".pos"))
    (ert-info (directory)
      (should (equal "/g/s" (pos-path-archive-scope
                             (concat "/g/s/" directory "/archives"))))))
  (should (equal "/g/s/.git" (pos-path-archive-scope "/g/s/.git/archives"))))

(ert-deftest pos-path/a-scope-s-archive-may-lie-in-a-configuration-directory ()
  "A scope's archive is in one of its configuration directories, or beside them."
  (should (equal '(".clanka/archives" ".clankos/archives" ".pos/archives" "archives")
                 (pos-path-archive-paths))))

(provide 'pos-path-test)
;;; pos-path-test.el ends here
