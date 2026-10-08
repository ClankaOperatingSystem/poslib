;;; pos-tree-test.el --- Tests for pos-tree.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The configuration fixtures in
;; fixtures/pos-directory/ are for pyposlib too.  The trees are real:
;; each test makes repositories with git init in a temporary directory,
;; as doc/pos-directory.txt asks, and a child's remote is a repository
;; beside the tree.

;;; Code:

(require 'ert)
(require 'pos-tree)
(require 'pos-fixtures)

;;;; Making trees

(defmacro pos-tree-test-with (dir &rest body)
  "Evaluate BODY with DIR bound to a new temporary directory's true name.
Paths git reports are true names, so the tests compare against one."
  (declare (indent 1))
  (let ((tmp (make-symbol "tmp")))
    `(pos-test-with-temp-dir ,tmp
       (let ((,dir (directory-file-name (file-truename ,tmp))))
         ,@body))))

(defun pos-tree-test-write (dir path text)
  "Write TEXT to PATH beneath DIR."
  (let ((file (expand-file-name path dir)))
    (make-directory (file-name-directory file) t)
    (write-region text nil file nil 'silent)))

(defun pos-tree-test-commit (dir &rest files)
  "Write FILES, a plist of path and text, beneath DIR and commit them."
  (cl-loop for (path text) on files by #'cddr
           do (pos-tree-test-write dir path text))
  (pos-test-git dir "add" "-A")
  (pos-test-git dir "commit" "-q" "--allow-empty" "-m" "Add files"))

(defun pos-tree-test-repository (dir &rest files)
  "Make a repository at DIR, on master, with FILES committed.
FILES is a plist of path and text.  Return DIR."
  (make-directory dir t)
  (pos-test-git dir "init" "-q" "-b" "master")
  (apply #'pos-tree-test-commit dir files)
  dir)

(defun pos-tree-test-skill (name)
  "Return the files of a repository's own skill NAME as a plist."
  (list (format ".agents/skills/%s/SKILL.md" name)
        (format "---\nname: %s\ndescription: A skill.\n---\n" name)))

(defun pos-tree-test-source (dir version &rest names)
  "Make at DIR a source to install from, of VERSION, and return DIR.
Each of NAMES that begins clankos- is a skill, and any other a command.
What was at DIR is replaced."
  (delete-directory dir t)
  (pos-tree-test-write dir "version" (concat version "\n"))
  (dolist (name names)
    (if (string-prefix-p "clankos-" name)
        (pos-tree-test-write
         dir (format "skills/%s/SKILL.md" name)
         (format "---\nname: %s\ndescription: A skill.\n---\n" name))
      (pos-tree-test-write dir (concat "bin/" name) "#!/bin/sh\n")
      (set-file-modes (expand-file-name (concat "bin/" name) dir) #o755)))
  dir)

(defun pos-tree-test-child (path remote &rest more)
  "Return a child entry of a config.yaml: PATH, REMOTE and MORE lines."
  (concat (format "  - path: %s\n    remote: %s\n" path remote)
          (mapconcat (lambda (line) (format "    %s\n" line)) more "")))

(defun pos-tree-test-config (&rest children)
  "Return a config.yaml's text declaring CHILDREN, each an entry's text."
  (concat "pos: 2\nprojects: projects/\narchives:\n  - scope: .\n    kept: committed\n"
          (if children (concat "children:\n" (apply #'concat children)) "children: []\n")))

;;;; Reading and doing plans

(defun pos-tree-test-summary (plan)
  "Return PLAN as a list of strings, its actions and then its findings."
  (append
   (mapcar (lambda (action)
             (let-alist action
               (pcase .do
                 ("exclude" (format "exclude %s %s" .repository .path))
                 ("archive-excludes" (format "archive-excludes %s" .repository))
                 ("clone" (format "clone %s" .path))
                 ("install" (format "install %s %s" .path .version))
                 ("link" (format "link %s -> %s" .path .target))
                 ("unlink" (format "unlink %s" .path))
                 ("note" (format "note %s" .path)))))
           (alist-get 'actions plan))
   (mapcar (lambda (finding)
             (let-alist finding (format "%s %s" .finding .path)))
           (alist-get 'findings plan))))

(defun pos-tree-test-settle (root &optional source)
  "Plan and apply in the tree at ROOT until a plan has no action.
SOURCE is the directory to install from, if any.  Return that last
plan.  Fail if ten rounds do not settle it."
  (let ((plan (pos-tree-plan root source)) (rounds 0))
    (while (not (seq-empty-p (alist-get 'actions plan)))
      (when (> (cl-incf rounds) 10) (error "The tree does not settle"))
      (setq plan (pos-tree-apply root plan source)))
    plan))

(defun pos-tree-test-status (dir)
  "Return what git status reports of the repository at DIR."
  (pos-test-git dir "status" "--porcelain"))

;;;; The file

(defun pos-tree-test-sorted (value)
  "Return VALUE with every alist in it sorted by key, keys as strings."
  (cond
   ((vectorp value) (vconcat (mapcar #'pos-tree-test-sorted value)))
   ((and (consp value) (consp (car value)))
    (sort (mapcar (lambda (pair)
                    (cons (format "%s" (car pair)) (pos-tree-test-sorted (cdr pair))))
                  value)
          (lambda (a b) (string< (car a) (car b)))))
   (t value)))

(ert-deftest pos-tree/a-config-is-read-as-the-fixtures-say ()
  "Each fixture's file gives its configuration, or its kind of refusal."
  (let ((fixtures (pos-fixtures "pos-directory")))
    (should (> (length fixtures) 20))
    (dolist (named fixtures)
      (ert-info ((car named) :prefix "fixture: ")
        (let-alist (cdr named)
          (if .refused
              (should (equal .refused
                             (condition-case err
                                 (progn (pos-tree-read-config .yaml) "accepted")
                               (pos-tree-refused (symbol-name (nth 1 err))))))
            (should (equal (pos-tree-test-sorted .config)
                           (pos-tree-test-sorted
                            (pos-tree-read-config .yaml))))))))))

;;;; Mounts

(ert-deftest pos-tree/a-repository-that-declares-nothing-needs-nothing ()
  "A repository that declares nothing plans no action and finds nothing."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          "README" "root\n")))
      (should (equal (pos-tree-plan root)
                     '((pos . 2) (actions . []) (findings . []) (warnings . [])))))))

(ert-deftest pos-tree/only-a-repository-is-planned ()
  "A directory that is not a repository is refused a plan."
  (pos-tree-test-with dir
    (should (eq 'not-a-repository
                (condition-case err (pos-tree-plan dir)
                  (pos-tree-refused (nth 1 err)))))))

(ert-deftest pos-tree/a-missing-child-is-excluded-and-then-cloned ()
  "A declared child not yet there is excluded and then cloned."
  (pos-tree-test-with dir
    (let ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                           "README" "child\n"))
          (root (pos-tree-test-repository (expand-file-name "root" dir)
                                          "README" "root\n")))
      (pos-tree-test-write root ".pos/config.yaml"
                           (pos-tree-test-config
                            (pos-tree-test-child "projects/child" child)))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("exclude . projects/child" "clone projects/child")))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root)) nil))
      (should (file-exists-p (expand-file-name "projects/child/README" root))))))

(ert-deftest pos-tree/a-child-off-its-branch-is-found-and-left ()
  "A child on another branch than declared is found and left as it is."
  (pos-tree-test-with dir
    (let* ((child (apply #'pos-tree-test-repository
                         (expand-file-name "origins/child" dir)
                         (pos-tree-test-skill "c")))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".pos/config.yaml"
                        (pos-tree-test-config
                         (pos-tree-test-child "projects/child" child))
                        (pos-tree-test-skill "r")))
           (in-child (expand-file-name "projects/child" root)))
      (pos-tree-test-settle root)
      (delete-file (expand-file-name ".agents/skills/r" in-child))
      (pos-test-git in-child "switch" "-q" "-c" "other")
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("off-branch projects/child"))))))

(ert-deftest pos-tree/a-path-taken-is-found-and-left ()
  "By a repository of another remote, a plain directory and a link."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (other (pos-tree-test-repository (expand-file-name "origins/other" dir)
                                            "README" "other\n"))
           (root (pos-tree-test-repository (expand-file-name "root" dir)
                                           "README" "root\n")))
      (pos-tree-test-write root ".pos/config.yaml"
                           (pos-tree-test-config
                            (pos-tree-test-child "projects/a" child)
                            (pos-tree-test-child "projects/b" child)
                            (pos-tree-test-child "projects/c" child)))
      (pos-test-git root "clone" "-q" other
                    (expand-file-name "projects/a" root))
      (pos-tree-test-write root "projects/b/notes" "not a repository\n")
      (make-symbolic-link "b" (expand-file-name "projects/c" root))
      (should (equal (seq-remove (lambda (line) (string-prefix-p "exclude" line))
                                 (pos-tree-test-summary (pos-tree-plan root)))
                     '("other-remote projects/a" "path-taken projects/b"
                       "path-taken projects/c"))))))

(ert-deftest pos-tree/an-undeclared-repository-is-found ()
  "Directly beneath, and beneath a scope that is not a repository."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          "README" "root\n")))
      (pos-tree-test-repository (expand-file-name "projects/a" root))
      (pos-tree-test-repository
       (expand-file-name "responsibilities/plain/projects/b" root))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("undeclared projects/a"
                       "undeclared responsibilities/plain/projects/b"))))))

(ert-deftest pos-tree/a-childs-config-is-read-as-committed ()
  "An edit to a child's config.yaml that is not committed mounts nothing."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (pos-tree-test-repository (expand-file-name "root" dir)
                                           "README" "root\n")))
      (pos-tree-test-write root ".pos/config.yaml"
                           (pos-tree-test-config
                            (pos-tree-test-child "projects/child" child)))
      (pos-tree-test-settle root)
      (pos-tree-test-write (expand-file-name "projects/child" root)
                           ".pos/config.yaml"
                           (pos-tree-test-config
                            (pos-tree-test-child "projects/more" child)))
      (should (equal (pos-tree-test-summary (pos-tree-plan root)) nil)))))

(ert-deftest pos-tree/a-refused-config-is-found-and-nothing-beneath-planned ()
  "A refused configuration is found, and nothing beneath it is planned.
A configuration the reader refuses is a finding; nothing is planned
in that repository or beneath it."
  (pos-tree-test-with dir
    (let* ((child (apply #'pos-tree-test-repository
                         (expand-file-name "origins/child" dir)
                         ".pos/config.yaml" "pos: 2\nprojects: p/\nmethodologies: m/\n"
                         (pos-tree-test-skill "c")))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        (pos-tree-test-skill "r"))))
      (pos-tree-test-write root ".pos/config.yaml"
                           (pos-tree-test-config
                            (pos-tree-test-child "projects/child" child)))
      (should (equal (seq-filter (lambda (line) (string-match-p "child" line))
                                 (pos-tree-test-summary (pos-tree-test-settle root)))
                     '("config-refused projects/child")))
      (pos-tree-test-write root ".pos/config.yaml" "pos: 2\nprojects: p/\nmethodologies: m/\n")
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("config-refused ."))))))

(ert-deftest pos-tree/an-unknown-key-is-a-warning-the-plan-carries ()
  "A key the reader does not know refuses nothing.
The configuration is read without it, and the plan names the node and
the key."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository
                 (expand-file-name "root" dir)
                 ".pos/config.yaml" "pos: 2\nprojects: projects/\ncolour: blue\n"
                 "work/.clanka/config.yml" "pos: 2\nprojects: projects/\nsweep: weekly\n")))
      (pos-tree-test-write root ".pos/config.yaml"
                           "pos: 2\nprojects: projects/\ncolour: blue\nchildren:\n  - path: work\n")
      (let ((plan (pos-tree-plan root)))
        (should (equal [".: unknown-key: colour" "work: unknown-key: sweep"]
                       (alist-get 'warnings plan)))
        (should (seq-empty-p (alist-get 'findings plan)))))))

;;;; Names, kinds and directories

(ert-deftest pos-tree/a-configuration-has-either-name ()
  "Any of the directory names and either file name is read, alike."
  (dolist (file '(".clanka/config.yaml" ".clanka/config.yml"
                  ".clankos/config.yaml" ".clankos/config.yml"
                  ".pos/config.yaml" ".pos/config.yml"))
    (pos-tree-test-with dir
      (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                              "README" "child\n"))
             (root (pos-tree-test-repository (expand-file-name "root" dir)
                                             "README" "root\n")))
        (pos-tree-test-write root file
                             (pos-tree-test-config
                              (pos-tree-test-child "work" child)))
        (ert-info (file :prefix "file: ")
          (should (equal (pos-tree-config-file root) file))
          (should (equal (pos-tree-test-summary (pos-tree-plan root))
                         '("exclude . work" "clone work"))))))))

(ert-deftest pos-tree/two-configurations-are-refused ()
  "Two directories, or both file names in one, and nothing is planned."
  (dolist (other '(".pos/config.yaml" ".clankos/config.yaml" ".clanka/config.yml"))
    (pos-tree-test-with dir
      (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                            "README" "root\n")))
        (pos-tree-test-write root ".clanka/config.yaml" "pos: 2\nprojects: projects/\n")
        (pos-tree-test-write root other "pos: 2\nprojects: projects/\n")
        (should (eq 'two-configurations
                    (condition-case err (pos-tree-config-file root)
                      (pos-tree-refused (nth 1 err)))))
        (should (equal (pos-tree-test-summary (pos-tree-plan root))
                       '("config-refused .")))
        ;; Every path in a plan is relative to the root.
        (should-not (string-match-p
                     (regexp-quote dir)
                     (decode-coding-string
                      (pos-ledger-json (pos-tree-plan root)) 'utf-8)))))))

(ert-deftest pos-tree/a-child-s-configuration-is-read-from-its-branch-by-either-name ()
  "A mounted child that names itself .clanka/config.yml declares as any other."
  (pos-tree-test-with dir
    (let* ((grandchild (pos-tree-test-repository
                        (expand-file-name "origins/grandchild" dir) "README" "g\n"))
           (child (pos-tree-test-repository
                   (expand-file-name "origins/child" dir)
                   ".clanka/config.yml"
                   (pos-tree-test-config (pos-tree-test-child "deeper" grandchild))))
           (root (pos-tree-test-repository (expand-file-name "root" dir)
                                           "README" "root\n")))
      (pos-tree-test-write root ".clanka/config.yaml"
                           (pos-tree-test-config (pos-tree-test-child "child" child)))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root)) nil))
      (should (file-exists-p (expand-file-name "child/deeper/README" root))))))

(ert-deftest pos-tree/a-node-that-names-neither-location-is-unconfigured ()
  "It is found, and what it declares is planned all the same."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (pos-tree-test-repository (expand-file-name "root" dir)
                                           "README" "root\n")))
      (pos-tree-test-write root ".clanka/config.yaml"
                           (concat "pos: 2\nchildren:\n"
                                   (pos-tree-test-child "work" child)))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("exclude . work" "clone work" "archive-excludes ." "unconfigured ."))))))

(ert-deftest pos-tree/a-child-with-no-remote-is-a-directory ()
  "The directory itself is not excluded or cloned; a missing one is found."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          "README" "root\n"
                                          "health/README" "health\n")))
      (pos-tree-test-write root ".clanka/config.yaml"
                           (concat "pos: 2\nprojects: projects/\nchildren:\n"
                                   "  - path: health\n  - path: wealth\n"
                                   "  - path: README\n"))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("archive-excludes ." "path-taken README" "missing wealth"))))))

(ert-deftest pos-tree/a-directory-declares-what-is-beneath-it ()
  "A directory's own configuration mounts a repository beneath it.
Which is excluded in the repository the directory is part of, and is
given none of that repository's skills."
  (pos-tree-test-with dir
    (let* ((product (pos-tree-test-repository (expand-file-name "origins/product" dir)
                                              "README" "product\n"))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        "employment/.clanka/config.yaml"
                        (pos-tree-test-config (pos-tree-test-child "widget" product))
                        (pos-tree-test-skill "r"))))
      (pos-tree-test-write root ".clanka/config.yaml"
                           "pos: 2\nprojects: projects/\nchildren:\n  - path: employment\n")
      (should (equal (seq-filter (lambda (line) (string-match-p "widget" line))
                                 (pos-tree-test-summary (pos-tree-plan root)))
                     '("exclude . employment/widget" "clone employment/widget")))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root)) nil))
      (should (file-exists-p (expand-file-name "employment/widget/README" root)))
      (should-not (file-exists-p (expand-file-name "employment/widget/.agents"
                                                   root))))))

(ert-deftest pos-tree/a-repository-declared-with-no-remote-is-found ()
  "A repository declared with no remote is found as a path taken."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          "README" "root\n")))
      (pos-tree-test-repository (expand-file-name "health" root) "README" "h\n")
      (pos-tree-test-write root ".clanka/config.yaml"
                           "pos: 2\nprojects: projects/\nchildren:\n  - path: health\n")
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("archive-excludes ." "path-taken health"))))))

(ert-deftest pos-tree/what-no-entry-declares-is-found-wherever-it-is ()
  "What no entry declares is found at any depth, outside excluded places.
A repository, and a directory with a configuration; but not in an
archive, an attic, or a hidden or underscore directory."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository
                 (expand-file-name "root" dir)
                 "README" "root\n"
                 "health/.clanka/config.yaml" "pos: 2\nprojects: projects/\n"
                 "health/diet/.pos/config.yml" "pos: 2\nprojects: projects/\n"
                 "stray/deep/.clanka/config.yaml" "pos: 2\nprojects: projects/\n"
                 "archives/old/.clanka/config.yaml" "pos: 2\nprojects: projects/\n"
                 "_work/x/.clanka/config.yaml" "pos: 2\nprojects: projects/\n")))
      ;; A submodule is tracked, and is not found.
      (let ((process-environment (cons "GIT_ALLOW_PROTOCOL=file" process-environment)))
        (pos-test-git root "submodule" "--quiet" "add"
                      (pos-tree-test-repository
                       (expand-file-name "origins/module" dir) "README" "m\n")
                      "tests/module")
        (pos-tree-test-commit root))
      (pos-tree-test-repository (expand-file-name "vendor/lib" root) "README" "l\n")
      (pos-tree-test-repository (expand-file-name "attic/lib" root) "README" "l\n")
      (pos-tree-test-write root ".clanka/config.yaml"
                           "pos: 2\nprojects: projects/\nchildren:\n  - path: health\n")
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("archive-excludes ." "undeclared health/diet" "undeclared stray/deep"
                       "undeclared vendor/lib"))))))

(ert-deftest pos-tree/exclusions-are-inherited-until-a-node-declares-its-own ()
  "A node's exclude replaces the default beneath it, and is inherited.
By name, glob or path; a local node without one inherits, and one with
its own replaces them beneath itself."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository
                 (expand-file-name "root" dir)
                 ".clanka/config.yaml"
                 (concat "pos: 2\nprojects: projects/\nchildren:\n  - path: work\n"
                         "  - path: lab\nexclude:\n  - vendor\n  - \"tmp*\"\n  - stray/deep\n")
                 "work/.clanka/config.yml" "pos: 2\nprojects: projects/\n"
                 "lab/.clanka/config.yml" "pos: 2\nprojects: projects/\nexclude:\n  - attic\n")))
      (dolist (path '("vendor/lib" "tmpfiles/lib" "stray/deep/lib" "attic/lib"
                      "work/vendor/lib" "work/attic/lib"
                      "lab/vendor/lib" "lab/attic/lib"))
        (pos-tree-test-repository (expand-file-name path root) "README" "l\n"))
      (should (equal (seq-filter (lambda (line) (string-prefix-p "undeclared" line))
                                 (pos-tree-test-summary (pos-tree-plan root)))
                     '("undeclared attic/lib" "undeclared lab/vendor/lib"
                       "undeclared work/attic/lib"))))))

(ert-deftest pos-tree/archive-scopes-are-not-looked-for-in-excluded-directories ()
  "An archives directory beneath an excluded one is no scope of the node.
One that is not excluded is, as before."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository
                 (expand-file-name "root" dir)
                 ".clanka/config.yaml" "pos: 2\nprojects: projects/\nexclude:\n  - old\n")))
      (pos-tree-test-write root "old/archives/evidence" "old\n")
      (pos-tree-test-write root "kept/archives/evidence" "kept\n")
      (let ((action (seq-find (lambda (a) (equal (alist-get 'do a) "archive-excludes"))
                              (alist-get 'actions (pos-tree-plan root)))))
        (should (equal ["archives" "kept/archives"] (alist-get 'paths action)))))))

(ert-deftest pos-tree/a-worktree-of-a-child-is-excluded-and-cloned ()
  "A declared worktree of a child is excluded and cloned on its branch."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yaml"
                  (concat (pos-tree-test-config
                           (pos-tree-test-child "responsibilities/it" child))
                          "worktrees:\n"
                          "  - path: projects/fix/_worktrees/do-the-thing\n"
                          "    of: responsibilities/it\n"
                          "    branch: do-the-thing\n"))))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("exclude . responsibilities/it" "clone responsibilities/it"
                       "exclude . projects/fix/_worktrees/do-the-thing"
                       "clone projects/fix/_worktrees/do-the-thing")))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root)) nil))
      (should (equal (pos-test-git
                      (expand-file-name "projects/fix/_worktrees/do-the-thing" root)
                      "symbolic-ref" "--short" "HEAD")
                     "do-the-thing"))
      (should (equal (pos-tree-test-status root) "")))))

;;;; The second step

(defun pos-tree-test-refusal (root plan)
  "Return the kind of refusal applying PLAN at ROOT gives, or nil."
  (condition-case err
      (progn (pos-tree-apply root plan) nil)
    (pos-tree-refused (nth 1 err))))

(defun pos-tree-test-exclude (dir)
  "Return the lines of the info/exclude of the repository at DIR."
  (let ((file (pos-tree--exclude-file dir)))
    (and (file-exists-p file)
         (split-string (with-temp-buffer (insert-file-contents file)
                                         (buffer-string))
                       "\n" t))))

(ert-deftest pos-tree/a-plan-the-tree-no-longer-gives-is-refused ()
  "And nothing of it is done."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yaml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/child" child))))
           (plan (pos-tree-plan root))
           (before (pos-tree-test-exclude root)))
      (pos-test-git root "clone" "-q" child
                    (expand-file-name "projects/child" root))
      (should (eq (pos-tree-test-refusal root plan) 'stale-plan))
      (should (equal (pos-tree-test-exclude root) before)))))

(ert-deftest pos-tree/a-plan-as-printed-is-applied ()
  "A plan read back from its JSON is the plan."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yaml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/child" child))))
           (printed (pos-ledger-json (pos-tree-plan root))))
      (should (equal (pos-tree-test-summary
                      (pos-tree-apply root (pos-ledger-parse printed)))
                     nil))
      (should (file-exists-p (expand-file-name "projects/child/README" root))))))

(ert-deftest pos-tree/a-clone-that-fails-is-refused-and-what-was-done-stays ()
  "A clone that fails refuses the plan, and what was done before stays."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository
                 (expand-file-name "root" dir)
                 ".pos/config.yaml"
                 (pos-tree-test-config
                  (pos-tree-test-child "projects/child"
                                       (expand-file-name "origins/absent" dir))))))
      (should (eq (pos-tree-test-refusal root (pos-tree-plan root)) 'failed))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("clone projects/child"))))))

(ert-deftest pos-tree/an-exclude-is-added-on-a-line-of-its-own ()
  "After what the file holds, which may lack its final newline."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yaml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/child" child)))))
      (write-region "*.log" nil (pos-tree--exclude-file root) nil 'silent)
      (pos-tree-test-settle root)
      (should (equal (pos-tree-test-exclude root)
                     '("*.log" "/projects/child")))
      (pos-tree-test-settle root)
      (should (equal (length (pos-tree-test-exclude root)) 2)))))

(ert-deftest pos-tree/a-worktree-takes-a-branch-the-child-has ()
  "A branch of the child's remote is checked out, not made afresh."
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (progn
                   (pos-test-git child "switch" "-q" "-c" "feature")
                   (pos-tree-test-commit child "FEATURE" "on the branch\n")
                   (pos-test-git child "switch" "-q" "master")
                   (pos-tree-test-repository
                    (expand-file-name "root" dir)
                    ".pos/config.yaml"
                    (concat (pos-tree-test-config
                             (pos-tree-test-child "projects/child" child))
                            "worktrees:\n"
                            "  - path: _worktrees/feature\n"
                            "    of: projects/child\n"
                            "    branch: feature\n")))))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root)) nil))
      (should (file-exists-p (expand-file-name "_worktrees/feature/FEATURE" root))))))

;;;; What is installed

(defun pos-tree-test-names (dir within)
  "Return the names in WITHIN, a directory of the repository at DIR."
  (let ((in (expand-file-name within dir)))
    (and (file-directory-p in)
         (directory-files in nil directory-files-no-dot-files-regexp))))

(defun pos-tree-test-text (file)
  "Return the text of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest pos-tree/each-configured-repository-has-the-same-installed ()
  "A root, a responsibility mounted in it, and a product mounted in that.
Once settled, the root and the responsibility each hold the source in
auto/ with a link to its skill, the root has a link to the command
where it says bin, and the product has nothing of the tool's.  Nothing
is left to do, and no repository sees a change to commit."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture" "pos-capture"))
           (product (apply #'pos-tree-test-repository
                           (expand-file-name "origins/product" dir)
                           (pos-tree-test-skill "release")))
           (child (pos-tree-test-repository
                   (expand-file-name "origins/child" dir)
                   ".clanka/config.yml"
                   (pos-tree-test-config
                    (pos-tree-test-child "products/product" product))))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml"
                  (concat (pos-tree-test-config
                           (pos-tree-test-child "responsibilities/child" child))
                          "bin: bin\n")))
           (in-child (expand-file-name "responsibilities/child" root))
           (in-product (expand-file-name "products/product" in-child)))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root source)) nil))
      (dolist (repository (list root in-child))
        (should (equal (pos-tree-test-text
                        (expand-file-name ".clanka/auto/version" repository))
                       "1\n"))
        (should (equal (file-symlink-p
                        (expand-file-name ".agents/skills/clankos-capture" repository))
                       "../../.clanka/auto/skills/clankos-capture"))
        (should (file-exists-p
                 (expand-file-name ".agents/skills/clankos-capture/SKILL.md"
                                   repository)))
        (should (equal (file-symlink-p (expand-file-name ".claude/skills" repository))
                       "../.agents/skills")))
      (should (equal (file-symlink-p (expand-file-name "bin/pos-capture" root))
                     "../.clanka/auto/bin/pos-capture"))
      (should (file-executable-p (expand-file-name "bin/pos-capture" root)))
      (should-not (file-exists-p (expand-file-name "bin" in-child)))
      (should (equal (pos-tree-test-names in-product ".agents/skills") '("release")))
      (should-not (file-symlink-p (expand-file-name ".claude/skills" in-product)))
      (should-not (file-exists-p (expand-file-name ".clanka" in-product)))
      (dolist (repository (list root in-child in-product))
        (should (equal (pos-tree-test-status repository) "")))
      (should (equal (pos-tree-test-summary (pos-tree-plan root source)) nil)))))

(ert-deftest pos-tree/with-no-source-nothing-is-installed ()
  "A configured repository given no source has nothing planned."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          ".clanka/config.yml"
                                          (pos-tree-test-config))))
      (should (equal (pos-tree-test-summary (pos-tree-plan root)) nil)))))

(ert-deftest pos-tree/a-repository-with-no-configuration-is-left-alone ()
  "Nothing is installed in a repository with no configuration.
Its own skills are not read, and no .claude/skills link is made."
  (pos-tree-test-with dir
    (let ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                        "clankos-capture"))
          (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                       (pos-tree-test-skill "own"))))
      (should (equal (pos-tree-test-summary (pos-tree-plan root source)) nil)))))

(ert-deftest pos-tree/what-is-installed-stays-until-the-source-changes ()
  "With the source as it was, or with none, links are kept as they are."
  (pos-tree-test-with dir
    (let ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                        "clankos-capture"))
          (root (pos-tree-test-repository (expand-file-name "root" dir)
                                          ".clanka/config.yml"
                                          (pos-tree-test-config))))
      (pos-tree-test-settle root source)
      (should (equal (pos-tree-test-summary (pos-tree-plan root)) nil))
      (should (equal (pos-tree-test-names root ".agents/skills")
                     '("clankos-capture"))))))

(ert-deftest pos-tree/a-newer-source-replaces-what-was-installed ()
  "A source of another version replaces auto/ whole.
The link to a skill it leaves out is removed, and a link to one it
adds is made."
  (pos-tree-test-with dir
    (let ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                        "clankos-capture" "clankos-old"))
          (root (pos-tree-test-repository (expand-file-name "root" dir)
                                          ".clanka/config.yml"
                                          (pos-tree-test-config))))
      (pos-tree-test-settle root source)
      (pos-tree-test-write root ".clanka/auto/scribble" "a person's\n")
      (pos-tree-test-source source "2" "clankos-capture" "clankos-new")
      (should (equal (sort (pos-tree-test-summary (pos-tree-plan root source))
                           #'string<)
                     '("exclude . .agents/skills/clankos-new"
                       "install .clanka/auto 2"
                       "link .agents/skills/clankos-new -> ../../.clanka/auto/skills/clankos-new"
                       "unlink .agents/skills/clankos-old")))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root source)) nil))
      (should (equal (pos-tree-test-names root ".agents/skills")
                     '("clankos-capture" "clankos-new")))
      (should (equal (pos-tree-test-names root ".clanka/auto/skills")
                     '("clankos-capture" "clankos-new")))
      (should-not (file-exists-p (expand-file-name ".clanka/auto/scribble" root)))
      (should (equal (pos-tree-test-status root) "")))))

(ert-deftest pos-tree/a-name-taken-is-found-and-left ()
  "Where something else has a link's name, it is left and found.
The finding says whether the repository tracks it, and the other
links are made."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture" "clankos-seal"
                                         "pos-capture"))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".clanka/config.yml"
                        (concat (pos-tree-test-config) "bin: bin\n")
                        (pos-tree-test-skill "clankos-capture")))
           (plan nil))
      (pos-tree-test-write root "bin/pos-capture" "#!/bin/sh\n")
      (setq plan (pos-tree-test-settle root source))
      (should (equal (pos-tree-test-summary plan)
                     '("name-taken .agents/skills/clankos-capture"
                       "name-taken bin/pos-capture")))
      (should (string-match-p
               "\\`tracked, added in [0-9a-f]+\\'"
               (alist-get 'detail (aref (alist-get 'findings plan) 0))))
      (should (equal (alist-get 'detail (aref (alist-get 'findings plan) 1))
                     "untracked"))
      (should-not (file-symlink-p
                   (expand-file-name ".agents/skills/clankos-capture" root)))
      (should (file-symlink-p (expand-file-name ".agents/skills/clankos-seal" root)))
      (should (equal (pos-tree-test-text (expand-file-name "bin/pos-capture" root))
                     "#!/bin/sh\n")))))

(ert-deftest pos-tree/a-skills-path-that-is-not-a-directory-is-found ()
  "Where .agents is a file no skill link is made, and the path is found."
  (pos-tree-test-with dir
    (let ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                        "clankos-capture"))
          (root (pos-tree-test-repository (expand-file-name "root" dir)
                                          ".clanka/config.yml"
                                          (pos-tree-test-config)
                                          ".agents" "a file\n")))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root source))
                     '("name-taken .agents")))
      (should (equal (pos-tree-test-text (expand-file-name ".agents" root))
                     "a file\n")))))

(ert-deftest pos-tree/an-ordinary-claude-skills-has-the-links-and-a-note ()
  "A real .claude/skills directory gets the skill links too, and a note.
The note names what in the directory is no skill.  Once the directory
is replaced by a link, the note is removed from where it was moved to."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture"))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml" (pos-tree-test-config)
                  ".claude/skills/old/SKILL.md" "---\nname: old\n---\n"
                  ".claude/skills/loose.md" "Not a skill.\n"))
           (claude (expand-file-name ".claude/skills" root))
           (note (expand-file-name "README.clankos" claude)))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root source)) nil))
      (dolist (within '(".agents/skills" ".claude/skills"))
        (should (equal (file-symlink-p
                        (expand-file-name (concat within "/clankos-capture") root))
                       "../../.clanka/auto/skills/clankos-capture")))
      (should (string-match-p "^  loose\\.md$" (pos-tree-test-text note)))
      (should-not (string-match-p "^  old$" (pos-tree-test-text note)))
      (should (equal (pos-tree-test-status root) ""))
      ;; The person moves everything and replaces the directory by a link.
      (rename-file note (expand-file-name ".agents/skills/README.clankos" root))
      (delete-directory claude t)
      (make-symbolic-link "../.agents/skills" claude)
      (should (equal (pos-tree-test-summary (pos-tree-plan root source))
                     '("unlink .agents/skills/README.clankos")))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root source)) nil))
      (should (equal (pos-tree-test-names root ".agents/skills")
                     '("clankos-capture"))))))

(ert-deftest pos-tree/a-claude-path-that-is-not-a-directory-is-left ()
  "A file, a link or a dangling link at .claude or .claude/skills is left.
The skills are installed all the same, the tree is clean after, and a
second plan has nothing to do."
  (dolist (path '(".claude" ".claude/skills"))
    (dolist (kind '(file link dangling))
      (pos-tree-test-with dir
        (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                             "clankos-capture"))
               (root (pos-tree-test-repository (expand-file-name "root" dir)
                                               ".clanka/config.yml"
                                               (pos-tree-test-config)))
               (at (expand-file-name path root))
               (was nil))
          (make-directory (file-name-directory at) t)
          (pcase kind
            ('file (pos-tree-test-write root path "existing\n"))
            ((or 'link 'dangling)
             (when (eq kind 'link)
               (pos-tree-test-write root "foreign/kept.md" "existing\n"))
             (make-symbolic-link (expand-file-name "foreign" root) at)))
          (pos-tree-test-commit root)
          (setq was (file-attributes at))
          (should (equal (pos-tree-test-summary (pos-tree-test-settle root source))
                         nil))
          (should (equal (file-attribute-type (file-attributes at))
                         (file-attribute-type was)))
          (should (file-symlink-p
                   (expand-file-name ".agents/skills/clankos-capture" root)))
          (should (equal (pos-tree-test-status root) ""))
          (should (equal (pos-tree-test-summary (pos-tree-plan root source)) nil)))))))

(ert-deftest pos-tree/a-link-an-earlier-tool-made-is-removed ()
  "A link from one repository's skills to another's is removed.
It is removed from a product too, since the tool made it."
  (pos-tree-test-with dir
    (let* ((product (pos-tree-test-repository
                     (expand-file-name "origins/product" dir) "README" "product\n"))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".clanka/config.yml"
                        (pos-tree-test-config
                         (pos-tree-test-child "products/product" product))
                        (pos-tree-test-skill "r")))
           (link (expand-file-name "products/product/.agents/skills/r" root)))
      (pos-tree-test-settle root)
      (make-directory (file-name-directory link) t)
      (make-symbolic-link "../../../../.agents/skills/r" link)
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("unlink products/product/.agents/skills/r")))
      (pos-tree-test-settle root)
      (should-not (file-symlink-p link)))))

(ert-deftest pos-tree/a-link-to-outside-the-tree-is-left ()
  "A link in a repository's skills to somewhere outside the tree is left."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture"))
           (root (pos-tree-test-repository (expand-file-name "root" dir)
                                           ".clanka/config.yml"
                                           (pos-tree-test-config)))
           (elsewhere (expand-file-name "elsewhere/.agents/skills/r" dir))
           (link (expand-file-name ".agents/skills/r" root)))
      (pos-tree-test-write elsewhere "SKILL.md" "---\nname: r\n---\n")
      (make-directory (file-name-directory link) t)
      (make-symbolic-link elsewhere link)
      (pos-tree-test-settle root source)
      (should (equal (file-symlink-p link) elsewhere)))))

(ert-deftest pos-tree/installing-alone-clones-nothing ()
  "Only what is installed is done, and the rest stays planned.
A declared child that is not there is excluded and stays to be cloned."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture"))
           (child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/child" child)))))
      (should (equal (pos-tree-test-summary (pos-tree-install root source))
                     '("clone projects/child")))
      (should (file-exists-p
               (expand-file-name ".agents/skills/clankos-capture/SKILL.md" root)))
      (should-not (file-exists-p (expand-file-name "projects/child" root)))
      (should (equal (pos-tree-test-status root) "")))))

(ert-deftest pos-tree/a-source-that-is-not-one-is-refused ()
  "A source with no version, or a skill not named clankos-NAME, is refused."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          ".clanka/config.yml"
                                          (pos-tree-test-config)))
          (refusal (lambda (root source)
                     (condition-case err (progn (pos-tree-plan root source) nil)
                       (pos-tree-refused (nth 1 err))))))
      (pos-tree-test-write dir "unversioned/skills/clankos-a/SKILL.md" "---\n---\n")
      (should (eq 'bad-source
                  (funcall refusal root (expand-file-name "unversioned" dir))))
      (pos-tree-test-write dir "misnamed/version" "1\n")
      (pos-tree-test-write dir "misnamed/skills/capture/SKILL.md" "---\n---\n")
      (should (eq 'bad-source
                  (funcall refusal root (expand-file-name "misnamed" dir)))))))

(ert-deftest pos-tree/archives-follow-each-scope-and-policy-changes ()
  "Archive excludes follow each scope and change with its policy.
A scope whose configuration says nothing has its archives/ ignored at
any depth; one that keeps them committed has its own tracked; a line
the exclude file held before stays."
  (pos-tree-test-with dir
    (let* ((root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml" (pos-tree-test-config "  - path: work\n")
                  "work/.clanka/config.yml" "pos: 2\nprojects: projects/\n"
                  "work/archive-integrity/README" "The ledger stays tracked.\n"))
           (file (pos-tree--exclude-file root)))
      (pos-tree-test-write root "archives/evidence" "root\n")
      (pos-tree-test-write root "work/archives/evidence" "child\n")
      (pos-tree-test-write root "work/older/archives/evidence" "older\n")
      (write-region "/private-file\n" nil file nil 'silent)
      (pos-tree-test-settle root)
      (should (eq 0 (car (pos-tree--git root "check-ignore" "-q" "work/archives/evidence"))))
      (should (eq 0 (car (pos-tree--git root "check-ignore" "-q" "work/older/archives/evidence"))))
      (should (eq 1 (car (pos-tree--git root "check-ignore" "-q" "archives/evidence"))))
      (should (pos-tree--tracked-p root "work/archive-integrity/README"))
      (pos-tree-test-write root "work/.clanka/config.yml"
                           (pos-tree-test-config))
      (pos-tree-ignore-archives root)
      (should (eq 1 (car (pos-tree--git root "check-ignore" "-q" "work/archives/evidence"))))
      (should (eq 0 (car (pos-tree--git root "check-ignore" "-q" "work/older/archives/evidence"))))
      (should (file-exists-p (expand-file-name "work/archives/evidence" root)))
      (should (string-prefix-p "/private-file\n"
                               (with-temp-buffer (insert-file-contents file) (buffer-string)))))))

(ert-deftest pos-tree/archive-rules-preserve-damaged-blocks ()
  "A damaged archive block in the exclude file is left as it is.
The scope is found config-refused and the file is not rewritten."
  (pos-tree-test-with dir
    (let* ((root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yml" "pos: 2\nprojects: projects/\n"))
           (file (pos-tree--exclude-file root)))
      (write-region pos-tree--archive-begin nil file nil 'silent)
      (should (equal (pos-tree-test-summary (pos-tree-ignore-archives root))
                     '("config-refused .")))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                     pos-tree--archive-begin)))))

;;;; Methodologies

(defun pos-tree-test-method (name &rest commands)
  "Return the files of a methodology NAME as a plist.
It has the skill NAME-greet and each of COMMANDS, a command."
  (append
   (list (format "skills/%s-greet/SKILL.md" name)
         (format "---\nname: %s-greet\ndescription: A skill.\n---\n" name))
   (cl-loop for command in commands
            append (list (concat "bin/" command) "#!/bin/sh\n"))))

(defun pos-tree-test-project (&rest children)
  "Return a project's config.yaml, with bin, declaring CHILDREN."
  (concat "pos: 2\nmethodologies: methodologies\nbin: bin\n"
          (if children (concat "children:\n" (apply #'concat children)) "children: []\n")))

(ert-deftest pos-tree/a-mounted-methodology-is-linked-in-place ()
  "A project's methodology supplies its skill and command by links to it.
The links are made beside the source's, nothing of it is copied into
auto/, nothing in it changes, and a second plan is empty."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture" "pos-capture"))
           (hello (apply #'pos-tree-test-repository
                         (expand-file-name "origins/hello" dir)
                         (pos-tree-test-method "hello" "hello-greet")))
           (project (pos-tree-test-repository
                     (expand-file-name "origins/project" dir)
                     ".clanka/config.yml"
                     (pos-tree-test-project
                      (pos-tree-test-child "methodologies/hello" hello))))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/p" project))))
           (in-project (expand-file-name "projects/p" root)))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root source)) nil))
      (should (equal (file-symlink-p
                      (expand-file-name ".agents/skills/hello-greet" in-project))
                     "../../methodologies/hello/skills/hello-greet"))
      (should (equal (file-symlink-p
                      (expand-file-name ".agents/skills/clankos-capture" in-project))
                     "../../.clanka/auto/skills/clankos-capture"))
      (should (equal (file-symlink-p (expand-file-name "bin/hello-greet" in-project))
                     "../methodologies/hello/bin/hello-greet"))
      (should (equal (file-symlink-p (expand-file-name "bin/pos-capture" in-project))
                     "../.clanka/auto/bin/pos-capture"))
      (should (file-exists-p
               (expand-file-name ".agents/skills/hello-greet/SKILL.md" in-project)))
      (should (equal (pos-tree-test-names in-project ".clanka/auto/skills")
                     '("clankos-capture")))
      (should (equal (file-symlink-p (expand-file-name ".claude/skills" in-project))
                     "../.agents/skills"))
      (dolist (repository (list root in-project
                                (expand-file-name "methodologies/hello" in-project)))
        (should (equal (pos-tree-test-status repository) "")))
      (should (equal (pos-tree-test-summary (pos-tree-plan root source)) nil)))))

(ert-deftest pos-tree/a-directory-project-links-its-local-methodology ()
  "A project that is a directory of a repository has its links made there.
Its methodology is a directory of the same repository, with no
remote; the links are relative to the project and excluded."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture"))
           (root (apply #'pos-tree-test-repository
                        (expand-file-name "root" dir)
                        ".clanka/config.yml"
                        (pos-tree-test-config "  - path: projects/p\n")
                        "projects/p/.clanka/config.yml"
                        (pos-tree-test-project "  - path: methodologies/notes\n")
                        (cl-loop for (path text)
                                 on (pos-tree-test-method "notes" "notes-take")
                                 by #'cddr
                                 append (list (concat "projects/p/methodologies/notes/"
                                                      path)
                                              text))))
           (in-project (expand-file-name "projects/p" root)))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root source)) nil))
      (should (equal (file-symlink-p
                      (expand-file-name ".agents/skills/notes-greet" in-project))
                     "../../methodologies/notes/skills/notes-greet"))
      (should (equal (file-symlink-p (expand-file-name "bin/notes-take" in-project))
                     "../methodologies/notes/bin/notes-take"))
      (should (equal (file-symlink-p (expand-file-name ".claude/skills" in-project))
                     "../.agents/skills"))
      (should (equal (pos-tree-test-names in-project ".agents/skills") '("notes-greet")))
      (should (equal (pos-tree-test-names root ".agents/skills") '("clankos-capture")))
      (should (equal (pos-tree-test-status root) ""))
      (should (equal (pos-tree-test-summary (pos-tree-plan root source)) nil)))))

(ert-deftest pos-tree/a-skill-without-its-methodologys-prefix-is-refused ()
  "A methodology with a skill not named after it is found and supplies nothing.
The source's links are made all the same."
  (pos-tree-test-with dir
    (let* ((source (pos-tree-test-source (expand-file-name "source" dir) "1"
                                         "clankos-capture"))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml"
                  (pos-tree-test-project "  - path: methodologies/hello\n")
                  "methodologies/hello/skills/greet/SKILL.md"
                  "---\nname: greet\ndescription: A skill.\n---\n"
                  "methodologies/hello/bin/hello-greet" "#!/bin/sh\n"))
           (plan (pos-tree-test-settle root source)))
      (should (equal (pos-tree-test-summary plan)
                     '("methodology-refused methodologies/hello")))
      (should (equal (alist-get 'detail (aref (alist-get 'findings plan) 0))
                     "a skill not named hello-NAME: greet"))
      (should (equal (pos-tree-test-names root ".agents/skills") '("clankos-capture")))
      (should-not (file-exists-p (expand-file-name "bin" root))))))

(ert-deftest pos-tree/a-methodology-no-longer-declared-loses-its-links ()
  "When a methodology's entry goes, its links are removed and it is left.
Its repository, still there, is found as undeclared."
  (pos-tree-test-with dir
    (let* ((hello (apply #'pos-tree-test-repository
                         (expand-file-name "origins/hello" dir)
                         (pos-tree-test-method "hello" "hello-greet")))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml"
                  (pos-tree-test-project
                   (pos-tree-test-child "methodologies/hello" hello)))))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root)) nil))
      (pos-tree-test-commit root ".clanka/config.yml" (pos-tree-test-project))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("unlink .agents/skills/hello-greet"
                       "unlink bin/hello-greet"
                       "undeclared methodologies/hello")))
      (pos-tree-test-settle root)
      (should-not (file-symlink-p (expand-file-name ".agents/skills/hello-greet" root)))
      (should (file-exists-p
               (expand-file-name "methodologies/hello/skills/hello-greet/SKILL.md"
                                 root))))))

(ert-deftest pos-tree/the-same-repository-is-a-product-elsewhere ()
  "One repository is a methodology in one place and a product in others.
A project mounts it beneath its methodologies path and, on another
branch, as a product; a responsibility mounts it as a product.  Only
the methodology mount is read; nothing is written in a product."
  (pos-tree-test-with dir
    (let* ((hello (apply #'pos-tree-test-repository
                         (expand-file-name "origins/hello" dir)
                         (pos-tree-test-method "hello" "hello-greet")))
           (project (pos-tree-test-repository
                     (expand-file-name "origins/project" dir)
                     ".clanka/config.yml"
                     (pos-tree-test-project
                      (pos-tree-test-child "methodologies/hello" hello)
                      (pos-tree-test-child "products/hello" hello "branch: next"))))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".clanka/config.yml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/p" project)
                   (pos-tree-test-child "products/hello" hello))))
           (in-project (expand-file-name "projects/p" root)))
      (pos-test-git hello "branch" "next")
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root)) nil))
      (should (equal (file-symlink-p
                      (expand-file-name ".agents/skills/hello-greet" in-project))
                     "../../methodologies/hello/skills/hello-greet"))
      (dolist (product (list (expand-file-name "products/hello" in-project)
                             (expand-file-name "products/hello" root)))
        (should-not (file-exists-p (expand-file-name ".agents" product)))
        (should-not (file-exists-p (expand-file-name ".clanka" product)))
        (should (equal (pos-tree-test-status product) "")))
      (should (equal (pos-test-git (expand-file-name "products/hello" in-project)
                                   "symbolic-ref" "--short" "HEAD")
                     "next")))))

(ert-deftest pos-tree/a-methodology-named-clankos-is-refused ()
  "The reader refuses a methodology named clankos, so nothing is planned."
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository
                 (expand-file-name "root" dir)
                 ".clanka/config.yml"
                 (pos-tree-test-project
                  (pos-tree-test-child "methodologies/clankos"
                                       "git@example.org:clankos.git")))))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("config-refused ."))))))

(ert-deftest pos-tree/two-methodologies-supplying-one-name-are-found ()
  "Where two methodologies supply one command, the first is linked.
The second is found as a name taken, named by more than one."
  (pos-tree-test-with dir
    (let* ((root (apply #'pos-tree-test-repository
                        (expand-file-name "root" dir)
                        ".clanka/config.yml"
                        (pos-tree-test-project "  - path: methodologies/alpha\n"
                                               "  - path: methodologies/beta\n")
                        (append
                         (cl-loop for (path text) on (pos-tree-test-method "alpha" "greet")
                                  by #'cddr
                                  append (list (concat "methodologies/alpha/" path) text))
                         (cl-loop for (path text) on (pos-tree-test-method "beta" "greet")
                                  by #'cddr
                                  append (list (concat "methodologies/beta/" path) text)))))
           (plan (pos-tree-test-settle root)))
      (should (equal (pos-tree-test-summary plan) '("name-taken bin/greet")))
      (should (equal (alist-get 'detail (aref (alist-get 'findings plan) 0))
                     "named by more than one"))
      (should (equal (file-symlink-p (expand-file-name "bin/greet" root))
                     "../methodologies/alpha/bin/greet"))
      (should (equal (pos-tree-test-names root ".agents/skills")
                     '("alpha-greet" "beta-greet"))))))

(provide 'pos-tree-test)
;;; pos-tree-test.el ends here
