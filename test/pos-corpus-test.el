;;; pos-corpus-test.el --- What each Org-file walker reads today  -*- lexical-binding: t -*-

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

;; A characterisation, not a specification.  poslib has four functions
;; that decide which Org files a tree holds, each with rules of its
;; own: `pos-org-files' for the sweep, `pos-uncovered-org-files' for
;; the stranded-task lint, `pos-startup-files' for the start-up views
;; and `pos-roam-files' for the index; and the tree tool keeps a fifth
;; list of directories it does not look into.  They are to be replaced
;; by one corpus the configuration describes.  Before that change,
;; these tests record what each does today, over one tree, cell by
;; cell, so that each cell the change moves is moved on purpose and
;; each it keeps is kept on purpose.
;;
;; Where the walkers disagree, the disagreement is recorded, not
;; endorsed.  A cell here is what the code does, not what it should.

;;; Code:

(require 'ert)
(require 'seq)
(require 'pos)
(require 'pos-startup)
(require 'pos-roam)
(require 'pos-tree)
(require 'pos-test-support)

;;;; The tree

(defconst pos-corpus-test-files
  '(("intray.org" . "")
    ("life/x.org" . "")
    ("life/sub/y.org" . "")
    (".#lock.org" . "")
    (".hidden/z.org" . "")
    ("_tmp/a.org" . "")
    ("archives/b.org" . "")
    ("attic/c.org" . "")
    ("node_modules/d.org" . "")
    ("notes.txt" . "")
    ("archive/orgmode/2026-W35/e.org" . "")
    ("archive/orgmode/2026-W35/intray.org_archive" . "")
    ("meta/journal/j.org" . "")
    ("sub/archives/f.org" . "")
    ("sub/_deep/g.org" . "")
    ("life/x.org_archive" . ""))
  "The files of the tree the walkers are read over, each empty.
A symbolic link linked.org -> intray.org is added after they are written.")

(defmacro pos-corpus-test-with-tree (dir &rest body)
  "Evaluate BODY with DIR a tree of `pos-corpus-test-files' and the link.
The pillar is life/ and the prose directory meta/journal/."
  (declare (indent 1) (debug (symbolp body)))
  `(pos-test-with-files ,dir pos-corpus-test-files
     (make-symbolic-link "intray.org" (expand-file-name "linked.org" ,dir))
     (let ((pos-pillars '("life"))
           (pos-prose-directories '("meta/journal")))
       ,@body)))

;;;; The four walkers

(ert-deftest pos-corpus/today-each-walker-reads-a-different-set-of-files ()
  "Each walker reads its own set of files from one tree; this is the table.
A characterisation of present behaviour: every cell is what the code
does today, found by running it, and the divergences between the
columns are deliberate records, not desired rules.  The columns are
`pos-org-files', `pos-uncovered-org-files', `pos-startup-files' and
`pos-roam-files', with life/ the pillar and meta/journal/ the prose
directory.

The divergences found:

- Lock files: the sweep and the index read .#lock.org; start-up does
  not, its file-name regexp refusing names that begin with a dot or
  a hash; the lint does not list it only because the sweep covers it.

- node_modules: the three recursive walkers all read beneath it; only
  the tree tool, below, skips it.

- archive and archives: the lint alone skips the sweep's archive,
  archive/orgmode/; start-up and the index read the Org files kept
  there.  Start-up and the index skip archives/ and attic/ at any
  depth; the lint lists what they hold as uncovered.

- Underscore directories: start-up and the index skip _tmp/ and
  sub/_deep/; the lint lists what they hold.

- Prose directories: the lint alone honours `pos-prose-directories';
  start-up and the index read meta/journal/j.org.

- Pillar depth: the sweep reads a pillar's top level only, so
  life/sub/y.org is uncovered to the lint and read by the other two.

- Hidden directories: all four skip .hidden/.  Symbolic links to Org
  files are read by all, as the file they name.  Names that do not end
  in .org, including an archive's intray.org_archive, are read by none."
  (pos-corpus-test-with-tree dir
    (let ((org-files (pos-test-relative (pos-org-files dir) dir))
          (uncovered (pos-test-relative (pos-uncovered-org-files dir) dir))
          (startup (pos-test-relative (pos-startup-files dir) dir))
          (roam (pos-test-relative (pos-roam-files dir) dir))
          (table
           ;; PATH                                        ORG-FILES UNCOVERED STARTUP ROAM
           '(("intray.org"                                 t     nil   t     t)
             ("life/x.org"                                 t     nil   t     t)
             ("life/sub/y.org"                             nil   t     t     t)
             (".#lock.org"                                 t     nil   nil   t)
             (".hidden/z.org"                              nil   nil   nil   nil)
             ("_tmp/a.org"                                 nil   t     nil   nil)
             ("archives/b.org"                             nil   t     nil   nil)
             ("attic/c.org"                                nil   t     nil   nil)
             ("node_modules/d.org"                         nil   t     t     t)
             ("notes.txt"                                  nil   nil   nil   nil)
             ("archive/orgmode/2026-W35/e.org"             nil   nil   t     t)
             ("archive/orgmode/2026-W35/intray.org_archive" nil  nil   nil   nil)
             ("meta/journal/j.org"                         nil   nil   t     t)
             ("sub/archives/f.org"                         nil   t     nil   nil)
             ("sub/_deep/g.org"                            nil   t     nil   nil)
             ("life/x.org_archive"                         nil   nil   nil   nil)
             ("linked.org"                                 t     nil   t     t))))
      (pcase-dolist (`(,path ,in-org-files ,in-uncovered ,in-startup ,in-roam) table)
        (ert-info ((format "%s in pos-org-files" path))
          (should (eq in-org-files (and (member path org-files) t))))
        (ert-info ((format "%s in pos-uncovered-org-files" path))
          (should (eq in-uncovered (and (member path uncovered) t))))
        (ert-info ((format "%s in pos-startup-files" path))
          (should (eq in-startup (and (member path startup) t))))
        (ert-info ((format "%s in pos-roam-files" path))
          (should (eq in-roam (and (member path roam) t)))))
      ;; The table names every file a walker found; nothing is read
      ;; that the table does not have a row for.
      (let ((rows (mapcar #'car table)))
        (dolist (found (append org-files uncovered startup roam))
          (ert-info ((format "%s has a row" found))
            (should (member found rows))))))))

;;;; The tree tool

(ert-deftest pos-corpus/today-the-tree-tool-has-its-own-exclusion-list ()
  "The tree tool skips its own list of directories, not start-up's.
A characterisation of present behaviour, recorded, not endorsed.
`pos-tree--unwalked' names archives, attic and node_modules, so it
differs from `pos-startup-excluded-directories' by node_modules alone;
and `pos-tree--undeclared' further skips a name beginning with a dot or
an underscore, as start-up and the index do, at any depth."
  (should (equal '("archives" "attic" "node_modules") pos-tree--unwalked))
  (should (equal '("node_modules")
                 (seq-difference pos-tree--unwalked pos-startup-excluded-directories)))
  (should-not (seq-difference pos-startup-excluded-directories pos-tree--unwalked))
  (should (equal pos-startup-excluded-directories pos-roam-excluded-directories))
  ;; A repository beneath each name: those the tool looks into are
  ;; found undeclared, the rest are passed over without a word.
  (pos-test-with-temp-dir tmp
    (let ((root (file-truename tmp)))
      (pos-test-git-init root)
      (dolist (path '("plain" "sub/plain" "_tmp" "sub/_deep" ".hidden"
                      "node_modules" "archives" "attic"))
        (make-directory (expand-file-name path root) t)
        (with-temp-file (expand-file-name ".git" (expand-file-name path root))
          (insert "gitdir: nowhere\n")))
      (let ((pos-tree--root root)
            (pos-tree--findings nil))
        (pos-tree--undeclared root nil nil)
        (should (equal '("plain" "sub/plain")
                       (sort (mapcar (lambda (finding) (alist-get 'path finding))
                                     pos-tree--findings)
                             #'string<)))))))

(provide 'pos-corpus-test)
;;; pos-corpus-test.el ends here
