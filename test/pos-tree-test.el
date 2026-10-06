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
(require 'pos-fixtures
         (expand-file-name "pos-fixtures"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

;;;; Making trees

(defmacro pos-tree-test-with (dir &rest body)
  "Evaluate BODY with DIR bound to a new temporary directory.
Git runs without the person's configuration and with a fixed author."
  (declare (indent 1))
  `(let ((,dir (file-truename (make-temp-file "pos-tree" t)))
         (process-environment
          (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null"
                    "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=test@example.org"
                    "GIT_COMMITTER_NAME=Test"
                    "GIT_COMMITTER_EMAIL=test@example.org")
                  process-environment)))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun pos-tree-test-git (dir &rest args)
  "Run git in DIR with ARGS, and fail if git does."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (unless (eq 0 (apply #'process-file "git" nil t nil args))
        (error "git %s: %s" args (buffer-string))))))

(defun pos-tree-test-write (dir path text)
  "Write TEXT to PATH beneath DIR."
  (let ((file (expand-file-name path dir)))
    (make-directory (file-name-directory file) t)
    (write-region text nil file nil 'silent)))

(defun pos-tree-test-commit (dir &rest files)
  "Write FILES, a plist of path and text, beneath DIR and commit them."
  (cl-loop for (path text) on files by #'cddr
           do (pos-tree-test-write dir path text))
  (pos-tree-test-git dir "add" "-A")
  (pos-tree-test-git dir "commit" "-q" "--allow-empty" "-m" "Add files"))

(defun pos-tree-test-repository (dir &rest files)
  "Make a repository at DIR, on master, with FILES committed.
FILES is a plist of path and text.  Return DIR."
  (make-directory dir t)
  (pos-tree-test-git dir "init" "-q" "-b" "master")
  (apply #'pos-tree-test-commit dir files)
  dir)

(defun pos-tree-test-skill (name &optional local)
  "Return the files of a skill NAME, as a plist; LOCAL marks it .pos-local."
  (append (list (format ".agents/skills/%s/SKILL.md" name)
                (format "---\nname: %s\ndescription: A skill.\n---\n" name))
          (and local (list (format ".agents/skills/%s/.pos-local" name) ""))))

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
                 ("link" (format "link %s -> %s" .path .target))
                 ("unlink" (format "unlink %s" .path)))))
           (alist-get 'actions plan))
   (mapcar (lambda (finding)
             (let-alist finding (format "%s %s" .finding .path)))
           (alist-get 'findings plan))))

(defun pos-tree-test-settle (root)
  "Plan and apply in the tree at ROOT until a plan has no action.
Return that last plan.  Fail if ten rounds do not settle it."
  (let ((plan (pos-tree-plan root)) (rounds 0))
    (while (not (seq-empty-p (alist-get 'actions plan)))
      (when (> (cl-incf rounds) 10) (error "The tree does not settle"))
      (setq plan (pos-tree-apply root plan)))
    plan))

(defun pos-tree-test-status (dir)
  "Return what git status reports of the repository at DIR."
  (pos-tree--git-line dir "status" "--porcelain"))

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
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          "README" "root\n")))
      (should (equal (pos-tree-plan root)
                     '((pos . 2) (actions . []) (findings . [])))))))

(ert-deftest pos-tree/only-a-repository-is-planned ()
  (pos-tree-test-with dir
    (should (eq 'not-a-repository
                (condition-case err (pos-tree-plan dir)
                  (pos-tree-refused (nth 1 err)))))))

(ert-deftest pos-tree/a-missing-child-is-excluded-and-then-cloned ()
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

(ert-deftest pos-tree/a-tree-settles-with-its-skills-linked-down ()
  "A root, a child and a child of that child, each with a skill of its own.
Once settled, each repository has its containers' skills as links that
resolve, every repository has its .claude/skills link, nothing is left
to do, and no repository sees a change to commit."
  (pos-tree-test-with dir
    (let* ((grandchild (apply #'pos-tree-test-repository
                              (expand-file-name "origins/grandchild" dir)
                              (pos-tree-test-skill "g")))
           (child (apply #'pos-tree-test-repository
                         (expand-file-name "origins/child" dir)
                         ".pos/config.yaml"
                         (pos-tree-test-config
                          (pos-tree-test-child "projects/grandchild" grandchild))
                         (pos-tree-test-skill "c")))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".pos/config.yaml"
                        (pos-tree-test-config
                         (pos-tree-test-child "responsibilities/child" child))
                        (pos-tree-test-skill "r")))
           (plan (pos-tree-test-settle root))
           (in-child (expand-file-name "responsibilities/child" root))
           (in-grandchild (expand-file-name "projects/grandchild" in-child)))
      (should (equal (pos-tree-test-summary plan) nil))
      (should (equal (file-symlink-p (expand-file-name ".agents/skills/r" in-child))
                     "../../../../.agents/skills/r"))
      (dolist (name '("r" "c"))
        (should (file-exists-p (expand-file-name
                                (format ".agents/skills/%s/SKILL.md" name)
                                in-grandchild))))
      (dolist (repository (list root in-child in-grandchild))
        (should (equal (file-symlink-p (expand-file-name ".claude/skills" repository))
                       "../.agents/skills"))
        (should (equal (pos-tree-test-status repository) ""))))))

(ert-deftest pos-tree/a-child-off-its-branch-is-found-and-left ()
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
      (pos-tree-test-git in-child "switch" "-q" "-c" "other")
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
      (pos-tree-test-git root "clone" "-q" other
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
  (pos-tree-test-with dir
    (let* ((child (apply #'pos-tree-test-repository
                         (expand-file-name "origins/child" dir)
                         ".pos/config.yaml" "pos: 2\nteam: []\n"
                         (pos-tree-test-skill "c")))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        (pos-tree-test-skill "r"))))
      (pos-tree-test-write root ".pos/config.yaml"
                           (pos-tree-test-config
                            (pos-tree-test-child "projects/child" child)))
      (should (equal (seq-filter (lambda (line) (string-match-p "child" line))
                                 (pos-tree-test-summary (pos-tree-test-settle root)))
                     '("config-refused projects/child")))
      (pos-tree-test-write root ".pos/config.yaml" "pos: 2\nteam: []\n")
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("config-refused ."))))))

;;;; Names, kinds and directories

(ert-deftest pos-tree/a-configuration-has-either-name ()
  "Either directory and either file name is read, alike."
  (dolist (file '(".clanka/config.yaml" ".clanka/config.yml"
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
  "Both directories, or both file names in one, and nothing is planned."
  (dolist (other '(".pos/config.yaml" ".clanka/config.yml"))
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
  "A directory's own configuration mounts a repository beneath it, which
is excluded in the repository the directory is part of, and is given
that repository's skills."
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
      (should (file-exists-p (expand-file-name
                              "employment/widget/.agents/skills/r/SKILL.md" root))))))

(ert-deftest pos-tree/a-repository-declared-with-no-remote-is-found ()
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository (expand-file-name "root" dir)
                                          "README" "root\n")))
      (pos-tree-test-repository (expand-file-name "health" root) "README" "h\n")
      (pos-tree-test-write root ".clanka/config.yaml"
                           "pos: 2\nprojects: projects/\nchildren:\n  - path: health\n")
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("archive-excludes ." "path-taken health"))))))

(ert-deftest pos-tree/what-no-entry-declares-is-found-wherever-it-is ()
  "A repository, and a directory with a configuration, at any depth; but
not in an archive, an attic, or a hidden or underscore directory."
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
        (pos-tree-test-git root "submodule" "--quiet" "add"
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

(ert-deftest pos-tree/a-worktree-of-a-child-is-excluded-and-cloned ()
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
      (should (equal (pos-tree--git-line
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
      (pos-tree-test-git root "clone" "-q" child
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
                      (pos-tree-apply root (pos-ledger--parse printed)))
                     nil))
      (should (file-exists-p (expand-file-name "projects/child/README" root))))))

(ert-deftest pos-tree/a-clone-that-fails-is-refused-and-what-was-done-stays ()
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
                   (pos-tree-test-git child "switch" "-q" "-c" "feature")
                   (pos-tree-test-commit child "FEATURE" "on the branch\n")
                   (pos-tree-test-git child "switch" "-q" "master")
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

;;;; Skills

(defun pos-tree-test-skill-names (dir)
  "Return the names in the skills directory of the repository at DIR."
  (pos-tree--entries dir))

(ert-deftest pos-tree/the-nearer-skill-has-a-name ()
  "A repository's own skill before a link, a nearer container's before a farther."
  (pos-tree-test-with dir
    (let* ((grandchild (pos-tree-test-repository
                        (expand-file-name "origins/grandchild" dir)
                        "README" "grandchild\n"))
           (child (apply #'pos-tree-test-repository
                         (expand-file-name "origins/child" dir)
                         ".pos/config.yaml"
                         (pos-tree-test-config
                          (pos-tree-test-child "projects/grandchild" grandchild))
                         (append (pos-tree-test-skill "x")
                                 (list ".agents/skills/x/whose" "child's\n"))))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".pos/config.yaml"
                        (pos-tree-test-config
                         (pos-tree-test-child "projects/child" child))
                        (append (pos-tree-test-skill "x")
                                (list ".agents/skills/x/whose" "root's\n"))))
           (in-child (expand-file-name "projects/child" root))
           (whose (lambda (repository)
                    (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name ".agents/skills/x/whose" repository))
                      (buffer-string)))))
      (pos-tree-test-settle root)
      (should-not (file-symlink-p (expand-file-name ".agents/skills/x" in-child)))
      (should (equal (funcall whose in-child) "child's\n"))
      (should (equal (funcall whose (expand-file-name "projects/grandchild" in-child))
                     "child's\n")))))

(ert-deftest pos-tree/a-local-skill-is-not-linked ()
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".pos/config.yaml"
                        (pos-tree-test-config
                         (pos-tree-test-child "projects/child" child))
                        (append (pos-tree-test-skill "shared")
                                (pos-tree-test-skill "mine" t)))))
      (pos-tree-test-settle root)
      (should (equal (pos-tree-test-skill-names (expand-file-name "projects/child" root))
                     '("shared"))))))

(ert-deftest pos-tree/skills-go-up-only-from-a-child-marked-for-it ()
  "And a skill linked up is not linked on to another child."
  (pos-tree-test-with dir
    (let* ((marked (apply #'pos-tree-test-repository
                          (expand-file-name "origins/marked" dir)
                          (pos-tree-test-skill "m")))
           (unmarked (apply #'pos-tree-test-repository
                            (expand-file-name "origins/unmarked" dir)
                            (pos-tree-test-skill "u")))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yaml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/marked" marked "skills-up: true")
                   (pos-tree-test-child "projects/unmarked" unmarked)))))
      (pos-tree-test-settle root)
      (should (equal (pos-tree-test-skill-names root) '("m")))
      (should (equal (file-symlink-p (expand-file-name ".agents/skills/m" root))
                     "../../projects/marked/.agents/skills/m"))
      (should (equal (pos-tree-test-skill-names
                      (expand-file-name "projects/unmarked" root))
                     '("u")))
      (should (equal (pos-tree-test-status root) "")))))

(ert-deftest pos-tree/a-child-off-its-branch-keeps-its-skills-linked-up ()
  "Nothing is done about a child with a finding, in its container either."
  (pos-tree-test-with dir
    (let* ((child (apply #'pos-tree-test-repository
                         (expand-file-name "origins/child" dir)
                         (pos-tree-test-skill "c")))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yaml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/child" child "skills-up: true")))))
      (pos-tree-test-settle root)
      (should (equal (pos-tree-test-skill-names root) '("c")))
      (pos-tree-test-git (expand-file-name "projects/child" root)
                         "switch" "-q" "-c" "other")
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("off-branch projects/child"))))))

(ert-deftest pos-tree/two-childrens-skills-of-one-name-are-found-and-left ()
  (pos-tree-test-with dir
    (let* ((a (apply #'pos-tree-test-repository (expand-file-name "origins/a" dir)
                     (pos-tree-test-skill "same")))
           (b (apply #'pos-tree-test-repository (expand-file-name "origins/b" dir)
                     (pos-tree-test-skill "same")))
           (root (pos-tree-test-repository
                  (expand-file-name "root" dir)
                  ".pos/config.yaml"
                  (pos-tree-test-config
                   (pos-tree-test-child "projects/a" a "skills-up: true")
                   (pos-tree-test-child "projects/b" b "skills-up: true")))))
      (should (equal (pos-tree-test-summary (pos-tree-test-settle root))
                     '("name-clash .agents/skills/same")))
      (should (equal (pos-tree-test-skill-names root) nil)))))

(ert-deftest pos-tree/a-link-whose-skill-is-gone-is-removed ()
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".pos/config.yaml"
                        (pos-tree-test-config
                         (pos-tree-test-child "projects/child" child))
                        (pos-tree-test-skill "r"))))
      (pos-tree-test-settle root)
      (delete-directory (expand-file-name ".agents/skills/r" root) t)
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("unlink projects/child/.agents/skills/r")))
      (pos-tree-test-settle root)
      (should (equal (pos-tree-test-skill-names (expand-file-name "projects/child" root))
                     nil)))))

(ert-deftest pos-tree/a-link-to-outside-the-tree-is-left-and-keeps-its-name ()
  (pos-tree-test-with dir
    (let* ((child (pos-tree-test-repository (expand-file-name "origins/child" dir)
                                            "README" "child\n"))
           (root (apply #'pos-tree-test-repository (expand-file-name "root" dir)
                        ".pos/config.yaml"
                        (pos-tree-test-config
                         (pos-tree-test-child "projects/child" child))
                        (pos-tree-test-skill "r")))
           (elsewhere (expand-file-name "elsewhere/.agents/skills/r" dir))
           (link (expand-file-name "projects/child/.agents/skills/r" root)))
      (pos-tree-test-write elsewhere "SKILL.md" "---\nname: r\n---\n")
      (pos-tree-test-git root "clone" "-q" child (expand-file-name "projects/child" root))
      (make-directory (file-name-directory link) t)
      (make-symbolic-link elsewhere link)
      (pos-tree-test-settle root)
      (should (equal (file-symlink-p link) elsewhere)))))

(ert-deftest pos-tree/skills-beneath-claude-are-found ()
  (pos-tree-test-with dir
    (let ((root (pos-tree-test-repository
                 (expand-file-name "root" dir)
                 ".claude/skills/old/SKILL.md" "---\nname: old\n---\n")))
      (should (equal (pos-tree-test-summary (pos-tree-plan root))
                     '("claude-skills .claude/skills"))))))

(ert-deftest pos-tree/archives-follow-each-scope-and-policy-changes ()
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

(provide 'pos-tree-test)
;;; pos-tree-test.el ends here
