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

;; The rules of the corpus, pos-corpus.el, each shown on a listing
;; given to the walk in place of the disk: which files are read, which
;; scope owns each, what is not entered and what may be written.  One
;; test gives the walk a real directory, to show the disk is listed as
;; the rules expect; and one records, over the tree the four former
;; walkers were characterised on, what the one corpus reads and which
;; cells moved when they were replaced.

;;; Code:

(require 'ert)
(require 'seq)
(require 'pos-corpus)
(require 'pos-test-support)

;;;; A listing in place of the disk

(defun pos-corpus-test-lister (spec)
  "Return a lister over SPEC, a tree described as a list of entries.
Each entry is (PATH KIND . PROPERTIES): PATH relative to the root,
KIND file, dir or link, and PROPERTIES :repository or :config as
`pos-corpus--walk' asks for.  A directory a path passes through
need not be listed; it is a plain directory."
  (let ((entries (make-hash-table :test #'equal)))
    (dolist (entry spec)
      (puthash (car entry) (cdr entry) entries)
      (let ((parent (file-name-directory (car entry))))
        (while (and parent (not (gethash (directory-file-name parent) entries)))
          (puthash (directory-file-name parent) '(dir) entries)
          (setq parent (file-name-directory (directory-file-name parent))))))
    (lambda (path)
      (let ((entry (or (gethash path entries) (and (equal path "") '(dir)))))
        (pcase (car entry)
          ('file '(:kind file))
          ('link '(:kind link))
          ('dir
           (let ((prefix (if (equal path "") "" (concat path "/"))) names)
             (maphash (lambda (other _)
                        (when (and (string-prefix-p prefix other)
                                   (not (equal other path))
                                   (not (string-match-p "/" (substring other (length prefix)))))
                          (push (substring other (length prefix)) names)))
                      entries)
             (list :kind 'dir
                   :repository (plist-get (cdr entry) :repository)
                   :config (plist-get (cdr entry) :config)
                   :names (sort names #'string<)))))))))

(defun pos-corpus-test-walk (spec)
  "Return the corpus of SPEC, as `pos-corpus-test-lister' describes it, at /r/."
  (pos-corpus--walk "/r/" (pos-corpus-test-lister spec)))

(defun pos-corpus-test-files (corpus)
  "Return the files of CORPUS relative to its root, with each owner's path."
  (mapcar (lambda (entry)
            (cons (file-relative-name (car entry) "/r/") (pos-scope-path (cdr entry))))
          (pos-corpus-entries corpus)))

(defun pos-corpus-test-scopes (corpus)
  "Return the scopes of CORPUS as (KIND . PATH), the root left out."
  (mapcar (lambda (scope) (cons (pos-scope-kind scope) (pos-scope-path scope)))
          (cdr (pos-corpus-scopes corpus))))

(defconst pos-corpus-test-responsibility "pos: 2\nprojects: projects/\n"
  "The configuration of a responsibility whose projects lie in projects/.")

(defconst pos-corpus-test-project
  (concat "pos: 2\nmethodologies: methodologies\nchildren:\n"
          "  - path: methodologies/adr\n"
          "  - path: methodologies/mounted\n    remote: git@example.org:m.git\n")
  "The configuration of a project with a local methodology and a mounted one.")

(ert-deftest pos-corpus/a-methodology-is-not-entered ()
  "A project's methodology is not read, a directory of it or a repository in it.
Its own Org files are not the project's canon.  A mounted one is a
repository the root's commands do not write, as before; the corpus
names both, with their directories."
  (let ((corpus (pos-corpus-test-walk
                 `(("p" dir :config ,pos-corpus-test-project)
                   ("p/project.org" file)
                   ("p/methodologies/adr/README.org" file)
                   ("p/methodologies/adr/skills/adr-record/SKILL.md" file)
                   ("p/methodologies/mounted" dir :repository t)
                   ("p/methodologies/mounted/README.org" file)
                   ("p/decisions/0001-keep.md" file)))))
    (should (equal '(("p/project.org" . "p")) (pos-corpus-test-files corpus)))
    (should (equal '("p/methodologies/mounted") (pos-corpus-unwritable corpus)))
    (should (equal '(("adr" . "p/methodologies/adr") ("mounted" . "p/methodologies/mounted"))
                   (mapcar (lambda (methodology)
                             (cons (cadr methodology)
                                   (file-relative-name
                                    (directory-file-name (cddr methodology)) "/r/")))
                           (pos-corpus-methodologies corpus))))))

;;;; Files and the root

(ert-deftest pos-corpus/the-root-owns-every-file-no-scope-claims ()
  "An Org file at the root or in a plain directory belongs to the root."
  (let ((corpus (pos-corpus-test-walk '(("intray.org" file)
                                        ("notes/a.org" file)
                                        ("notes/deep/b.org" file)))))
    (should (equal '(("intray.org" . ".") ("notes/a.org" . ".") ("notes/deep/b.org" . "."))
                   (pos-corpus-test-files corpus)))
    (should (equal 'root (pos-scope-kind (car (pos-corpus-scopes corpus)))))
    (should-not (pos-corpus-test-scopes corpus))))

(ert-deftest pos-corpus/only-org-files-that-are-not-hidden-or-locks-are-read ()
  "A file is read if it is an Org file whose name begins with no dot.
A lock file begins with .#, a hidden one with a dot; an archive file, a
text file and a symbolic link are not read either."
  (should (equal '(("a.org" . "."))
                 (pos-corpus-test-files
                  (pos-corpus-test-walk '(("a.org" file) ("notes.txt" file)
                                          (".#a.org" file) (".secret.org" file)
                                          ("a.org_archive" file) ("linked.org" link)))))))

;;;; Exclusions

(ert-deftest pos-corpus/the-default-exclusions-keep-the-walk-out ()
  "With no exclude declared, the default list keeps the walk out.
Archives, attics, node_modules and names beginning with an underscore
or a dot are not entered, at any depth."
  (should (equal '(("a.org" . ".") ("sub/b.org" . "."))
                 (pos-corpus-test-files
                  (pos-corpus-test-walk '(("a.org" file) ("sub/b.org" file)
                                          ("archives/x.org" file) ("sub/attic/y.org" file)
                                          ("node_modules/m/z.org" file)
                                          ("_work/w.org" file) (".hidden/h.org" file)
                                          ("sub/_deep/archives/v.org" file)))))))

(ert-deftest pos-corpus/a-declared-exclude-replaces-the-default-beneath-its-node ()
  "A node's exclude replaces the default beneath it, and is inherited.
A node that declares its own replaces it beneath itself; one that
declares none inherits."
  (let ((corpus (pos-corpus-test-walk
                 `(("" dir :config "pos: 2\nprojects: projects/\nexclude:\n  - vendor\n  - \"tmp*\"\n  - stray/deep\n")
                   ("archives/now-read.org" file) ("vendor/v.org" file)
                   ("tmpfiles/t.org" file) ("stray/deep/s.org" file) ("stray/kept.org" file)
                   ("work" dir :config ,pos-corpus-test-responsibility)
                   ("work/vendor/w.org" file) ("work/attic/a.org" file)
                   ("lab" dir :config "pos: 2\nprojects: projects/\nexclude:\n  - attic\n")
                   ("lab/vendor/l.org" file) ("lab/attic/a.org" file)))))
    (should (equal '(("archives/now-read.org" . ".") ("lab/vendor/l.org" . "lab")
                     ("stray/kept.org" . ".") ("work/attic/a.org" . "work"))
                   (pos-corpus-test-files corpus)))))

;;;; Scopes

(ert-deftest pos-corpus/a-configured-directory-is-a-responsibility-that-owns-what-lies-beneath ()
  "A configured directory is a responsibility and owns its files.
Its configuration says where its projects belong; a responsibility
within it is a scope of its own."
  (let ((corpus (pos-corpus-test-walk
                 `(("intray.org" file)
                   ("health" dir :config ,pos-corpus-test-responsibility)
                   ("health/intray.org" file) ("health/notes/n.org" file)
                   ("health/teeth" dir :config ,pos-corpus-test-responsibility)
                   ("health/teeth/intray.org" file)))))
    (should (equal '(("health/intray.org" . "health") ("health/notes/n.org" . "health")
                     ("health/teeth/intray.org" . "health/teeth") ("intray.org" . "."))
                   (pos-corpus-test-files corpus)))
    (should (equal '((responsibility . "health") (responsibility . "health/teeth"))
                   (pos-corpus-test-scopes corpus)))
    (let ((teeth (pos-corpus-owner corpus "/r/health/teeth/intray.org")))
      (should (equal "health" (pos-scope-path (pos-scope-node teeth)))))))

(ert-deftest pos-corpus/what-lies-in-a-projects-directory-is-a-project ()
  "What lies directly in a node's projects directory is a project.
A directory or a single Org file, named by its path; a project's files
are its own, and a project may hold a configuration of its own kind."
  (let ((corpus (pos-corpus-test-walk
                 `(("" dir :config ,pos-corpus-test-responsibility)
                   ("projects/alpha/project.org" file) ("projects/alpha/notes/n.org" file)
                   ("projects/solo.org" file)
                   ("projects/beta" dir :config "pos: 2\nmethodologies: methodologies/\n")
                   ("projects/beta/project.org" file)
                   ("health" dir :config ,pos-corpus-test-responsibility)
                   ("health/projects/checkup.org" file)))))
    (should (equal '(("health/projects/checkup.org" . "health/projects/checkup")
                     ("projects/alpha/notes/n.org" . "projects/alpha")
                     ("projects/alpha/project.org" . "projects/alpha")
                     ("projects/beta/project.org" . "projects/beta")
                     ("projects/solo.org" . "projects/solo"))
                   (pos-corpus-test-files corpus)))
    (should (equal '((responsibility . "health") (project . "health/projects/checkup")
                     (project . "projects/alpha") (project . "projects/beta")
                     (project . "projects/solo"))
                   (pos-corpus-test-scopes corpus)))
    (should (equal "health" (pos-scope-path
                             (pos-scope-node
                              (pos-corpus-owner corpus "/r/health/projects/checkup.org")))))))

(ert-deftest pos-corpus/a-directorys-name-carries-no-meaning-by-itself ()
  "A directory named projects or responsibilities is plain without a configuration.
Its files belong to the scope above, and nothing in it is a scope."
  (let ((corpus (pos-corpus-test-walk '(("projects/alpha/project.org" file)
                                        ("responsibilities/home/index.org" file)))))
    (should (equal '(("projects/alpha/project.org" . ".")
                     ("responsibilities/home/index.org" . "."))
                   (pos-corpus-test-files corpus)))
    (should-not (pos-corpus-test-scopes corpus))))

(ert-deftest pos-corpus/an-unconfigured-node-is-entered-and-has-no-kind ()
  "A node yet to be configured is read and is a scope of no kind.
Its configuration says neither where its projects belong nor its
methodologies."
  (let ((corpus (pos-corpus-test-walk '(("new" dir :config "pos: 2\n")
                                        ("new/a.org" file)))))
    (should (equal '(("new/a.org" . "new")) (pos-corpus-test-files corpus)))
    (should (equal '((nil . "new")) (pos-corpus-test-scopes corpus)))))

;;;; Repositories and writing

(ert-deftest pos-corpus/a-repository-with-no-configuration-is-a-product-and-not-entered ()
  "A repository within the tree is a product unless it has a configuration.
A product's files are not read."
  (should (equal '(("a.org" . "."))
                 (pos-corpus-test-files
                  (pos-corpus-test-walk '(("a.org" file)
                                          ("vendor/lib" dir :repository t)
                                          ("vendor/lib/README.org" file)))))))

(ert-deftest pos-corpus/a-configured-repository-is-read-and-not-written ()
  "A repository with a configuration is read and not written.
It is a node of the tree: its files are read and owned by it, and no
command of this root writes them; the root's own files may be written."
  (let ((corpus (pos-corpus-test-walk
                 `(("a.org" file)
                   ("child" dir :repository t :config ,pos-corpus-test-responsibility)
                   ("child/intray.org" file) ("child/projects/p.org" file)))))
    (should (equal '(("a.org" . ".") ("child/intray.org" . "child")
                     ("child/projects/p.org" . "child/projects/p"))
                   (pos-corpus-test-files corpus)))
    (should (pos-corpus-writable-p corpus "/r/a.org"))
    (should-not (pos-corpus-writable-p corpus "/r/child/intray.org"))
    (should-not (pos-corpus-writable-p corpus "/r/child/projects/p.org"))
    (should-not (pos-corpus-writable-p corpus "/r/not-read.org"))))

;;;; Refusals

(ert-deftest pos-corpus/a-refused-configuration-is-a-finding-and-a-leaf ()
  "A refused configuration, or a node with two, is a finding and a leaf.
It is reported and not entered; the rest of the tree is read."
  (let ((corpus (pos-corpus-test-walk
                 '(("a.org" file)
                   ("bad" dir :config "pos: 2\nprojects: p/\nmethodologies: m/\n")
                   ("bad/b.org" file)
                   ("twice" dir :config two-configurations)
                   ("twice/t.org" file)))))
    (should (equal '(("a.org" . ".")) (pos-corpus-test-files corpus)))
    (should (equal '(("bad" . "config-refused: bad-value: A node says where its projects belong or its methodologies, not both")
                     ("twice" . "config-refused: two-configurations: The node has two configurations"))
                   (pos-corpus-findings corpus)))))

(ert-deftest pos-corpus/a-refused-root-reads-nothing ()
  "When the root's own configuration is refused, no file is read.
The finding names the root."
  (let ((corpus (pos-corpus-test-walk '(("" dir :config "pos: 3\n") ("a.org" file)))))
    (should-not (pos-corpus-files corpus))
    (should (equal '(".") (mapcar #'car (pos-corpus-findings corpus))))))

;;;; The disk

(ert-deftest pos-corpus/the-disk-is-listed-as-the-rules-expect ()
  "Given a real directory, the walk finds what the rules say.
Files and owners, a product left out, a link not read, and two
configurations refused."
  (pos-test-with-files root
      `(("intray.org" . "")
        ("projects/alpha/project.org" . "")
        ("health/.clanka/config.yml" . ,pos-corpus-test-responsibility)
        ("health/intray.org" . "")
        ("health/projects/checkup.org" . "")
        ("archives/old.org" . "")
        ("vendor/lib/.git/HEAD" . "ref: refs/heads/master\n")
        ("vendor/lib/README.org" . "")
        ("twice/.pos/config.yaml" . "pos: 2\n")
        ("twice/.clanka/config.yml" . "pos: 2\n")
        ("twice/t.org" . ""))
    (make-symbolic-link "intray.org" (expand-file-name "linked.org" root))
    (let ((corpus (pos-corpus root)))
      (should (equal '(("health/intray.org" . "health")
                       ("health/projects/checkup.org" . "health/projects/checkup")
                       ("intray.org" . ".")
                       ("projects/alpha/project.org" . "."))
                     (mapcar (lambda (entry)
                               (cons (file-relative-name (car entry) root)
                                     (pos-scope-path (cdr entry))))
                             (pos-corpus-entries corpus))))
      (should (equal '("twice") (mapcar #'car (pos-corpus-findings corpus))))
      (should (equal '("vendor/lib") (pos-corpus-unwritable corpus)))
      (should (equal (file-name-as-directory (expand-file-name "health" root))
                     (pos-scope-dir (pos-corpus-owner corpus (expand-file-name "health/intray.org" root))))))))

;;;; The tree the four walkers were characterised on

(defconst pos-corpus-test-tree
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
  "The files of the tree the former walkers were read over, each empty.
A symbolic link linked.org -> intray.org is added after they are written.")

(ert-deftest pos-corpus/the-one-corpus-replaces-four-walkers-cell-by-cell ()
  "Over the characterised tree, the corpus reads one set of files.
Before it, four walkers read four sets: pos-org-files for the sweep,
pos-uncovered-org-files for the stranded lint, pos-startup-files for
the start-up views and pos-roam-files for the index.  Each cell below
is what the corpus reads; the cells that moved, and from where:

- .#lock.org: the sweep and the index read lock files; the corpus does
  not, as start-up did not.
- node_modules/d.org: every walker but the tree tool read beneath
  node_modules; the corpus does not, as the default exclusions name it.
- life/sub/y.org and meta/journal/j.org: the sweep read a pillar's top
  level only, and the lint skipped the prose directories; the corpus
  reads every directory the exclusions allow, so a garden that wants
  meta/journal left alone declares it under exclude.
- archive/orgmode/2026-W35/e.org: the lint alone skipped the sweep's
  archive directory; the corpus reads a stray Org file there, as
  start-up did.  The archive files themselves end in _archive and are
  never read.
- linked.org: every walker read a symbolic link as the file it names;
  the corpus reads no link, since nothing may be written through one.
- archives/, attic/, _tmp/, sub/_deep/, .hidden/: the lint listed what
  the first four held as uncovered; the corpus enters none of them, as
  start-up and the index did not."
  (pos-test-with-files dir pos-corpus-test-tree
    (make-symbolic-link "intray.org" (expand-file-name "linked.org" dir))
    (let ((read (pos-test-relative (pos-corpus-files (pos-corpus dir)) dir))
          (table
           '(("intray.org" . t)
             ("life/x.org" . t)
             ("life/sub/y.org" . t)
             (".#lock.org" . nil)
             (".hidden/z.org" . nil)
             ("_tmp/a.org" . nil)
             ("archives/b.org" . nil)
             ("attic/c.org" . nil)
             ("node_modules/d.org" . nil)
             ("notes.txt" . nil)
             ("archive/orgmode/2026-W35/e.org" . t)
             ("archive/orgmode/2026-W35/intray.org_archive" . nil)
             ("meta/journal/j.org" . t)
             ("sub/archives/f.org" . nil)
             ("sub/_deep/g.org" . nil)
             ("life/x.org_archive" . nil)
             ("linked.org" . nil))))
      (pcase-dolist (`(,path . ,in-corpus) table)
        (ert-info ((format "%s in the corpus" path))
          (should (eq in-corpus (and (member path read) t)))))
      (dolist (found read)
        (ert-info ((format "%s has a row" found))
          (should (assoc found table)))))))

(provide 'pos-corpus-test)
;;; pos-corpus-test.el ends here
