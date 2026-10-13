;;; pos-tree.el --- A tree of repositories, as it is declared -*- lexical-binding: t; -*-

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

;; A repository declares in .pos/config.yaml the repositories mounted
;; beneath it, as doc/pos-directory.txt specifies.  These are the two
;; steps that bring a tree to what its .pos files declare.  The first
;; makes the plan, changes nothing and uses no network.  The second is
;; given a plan, does what it holds if the tree still gives that plan,
;; and returns the plan that remains.
;;
;; A plan holds the children and worktrees to clone, the paths to
;; exclude, what to install and the links to make and remove, with
;; what it found and will not act on.  The agent files of section 5 are
;; not planned: a server is read and checked, and nothing is done with
;; it.
;;
;; What is installed comes from a source the tool is given, a
;; directory of skills and commands.  It is copied into auto/ in the
;; configuration directory of each repository that has a configuration,
;; and links to it are made where a coding agent and a person look.
;; Nothing is written in a repository with no configuration, a product.
;;
;; The configurations are read by pos-node.el, which refuses what
;; doc/pos-directory.txt does not allow; a refusal is a finding of the
;; plan, and nothing beneath the node is planned.
;;
;; - `pos-tree-plan': the plan for the tree at a root.
;; - `pos-tree-apply': do a plan, and return the plan that remains.
;; - `pos-tree-install': do only what a plan installs and links.
;; - `pos-tree-batch': the command line, in pos-cli.el.
;; - `pos-tree-refuse', `pos-tree-refused': a refusal of a plan or its
;;   doing, and its signal.

;;; Code:

(require 'cl-lib)
(require 'pos-bytes)
(require 'pos-git)
(require 'pos-node)
(require 'pos-path)
(require 'seq)
(require 'subr-x)

(define-error 'pos-tree-refused "Tree plan refused")

(defun pos-tree-refuse (kind format &rest args)
  "Signal a refusal of KIND, with a message from FORMAT and ARGS."
  (signal 'pos-tree-refused (list kind (apply #'format format args))))

;;;; Git

(defun pos-tree--repository-p (dir)
  "Return non-nil if DIR is the root of a repository's working tree."
  (file-exists-p (expand-file-name ".git" dir)))

(defun pos-tree--tracked-p (dir path)
  "Return non-nil if the repository at DIR tracks PATH."
  (eq 0 (car (pos-git dir "ls-files" "--error-unmatch" "--" path))))

(defun pos-tree--exclude-file (dir)
  "Return the info/exclude file of the repository at DIR, or nil."
  (when-let* ((file (pos-git-line dir "rev-parse" "--git-path" "info/exclude")))
    (expand-file-name file dir)))

(defun pos-tree--excluded-p (dir path)
  "Return non-nil if the repository at DIR excludes PATH.
That is, if its info/exclude holds the line this tool writes for PATH."
  (when-let* ((file (pos-tree--exclude-file dir)))
    (and (file-readable-p file)
         (member (concat "/" path)
                 (split-string (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string))
                               "\n")))))

(cl-defstruct (pos-tree--plan (:constructor pos-tree--plan-make) (:copier nil))
  "The plan being made for a tree.
ROOT is the tree's root, as a directory name, and SOURCE the directory
the plan installs from, or nil for none.  ACTIONS and FINDINGS are
those of the plan, newest first.  WARNINGS are those of the
configurations read for it, newest first, each naming its node
relative to ROOT."
  root source actions findings warnings)

(defun pos-tree--rel (plan path)
  "Return PATH relative to the root of the tree PLAN is for."
  (file-relative-name (directory-file-name path) (pos-tree--plan-root plan)))

(defun pos-tree--config (plan dir branch)
  "Return the configuration of the node at DIR, or nil if it has none.
With BRANCH, DIR is a repository and it is read from that branch as
committed; with nil, from the working tree.  Its warnings are added to
PLAN.  Signal `pos-node-refused' for a file that is refused, and for
two configurations."
  (when-let* ((text (if branch
                       (when-let* ((file (pos-node-one-config
                                          (split-string
                                           (or (apply #'pos-git-line
                                                      dir "ls-tree" "--name-only"
                                                      "-r" branch "--"
                                                      (pos-path-config-paths))
                                               "")
                                           "\n" t))))
                         (pos-git-line dir "show" (concat branch ":" file)))
                     (when-let* ((file (pos-node-config-file dir)))
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name file dir))
                         (buffer-string))))))
    (let ((config (pos-node-read-config text)))
      (seq-doseq (warning (alist-get 'warnings config))
        (push (format "%s: %s" (pos-tree--rel plan dir) warning)
              (pos-tree--plan-warnings plan)))
      config)))

;;;; The plan

(cl-defstruct (pos-tree--node (:constructor pos-tree--node))
  "A repository of the tree that is mounted as declared.
CHILDREN are its children mounted as declared, each (ENTRY . NODE).
HELD are the directories of its children that are there and are not
planned, because of a finding.  LOCALS are the directories of the
repository that have a configuration of their own, each (DIR . CONFIG).
The rest is what planning the repository has found in it, with paths
relative to DIR.  MOUNTED are the declared paths of repositories and
LOCAL those of directories.  LOCAL-EXCLUSIONS is an alist of the path
of each local node that declares exclusions and the list; a node that
declares none is absent, and inherits.  ARCHIVE-PATHS are its
uncommitted archive paths, and ARCHIVE-REFUSED is non-nil if an
invalid node prevents updating its archive rules."
  dir config children held locals
  mounted local local-exclusions archive-paths archive-refused)

(defun pos-tree--act (plan do &rest pairs)
  "Add to PLAN the action DO, with PAIRS, an alist."
  (push `((do . ,do) ,@pairs) (pos-tree--plan-actions plan)))

(defun pos-tree--find (plan finding path &optional detail)
  "Add to PLAN the FINDING at PATH, with DETAIL if any."
  (push `((finding . ,finding) (path . ,path)
          ,@(and detail `((detail . ,detail))))
        (pos-tree--plan-findings plan)))

(defun pos-tree--exclude (plan dir path)
  "Add to PLAN excluding PATH in the repository at DIR, unless it is excluded."
  (unless (pos-tree--excluded-p dir path)
    (pos-tree--act plan "exclude" `(repository . ,(pos-tree--rel plan dir)) `(path . ,path))))

(defconst pos-tree--archive-begin "# BEGIN ClankOS archive excludes\n"
  "Opening marker of the archive rules this tool maintains.")

(defconst pos-tree--archive-end "# END ClankOS archive excludes\n"
  "Closing marker of the archive rules this tool maintains.")

(defun pos-tree--archive-block (text)
  "Return the bounds of our block in TEXT, or nil; refuse damaged markers."
  (let ((offset 0) starts ends)
    (dolist (line (split-string text "\n"))
      (when (equal (concat line "\n") pos-tree--archive-begin) (push offset starts))
      (when (equal (concat line "\n") pos-tree--archive-end)
        (push (+ offset (length line) 1) ends))
      (setq offset (+ offset (length line) 1)))
    (when (or starts ends)
      (unless (and (= (length starts) 1) (= (length ends) 1)
                   (< (car starts) (car ends)))
        (pos-tree-refuse 'archive-excludes "Damaged archive exclude block"))
      (cons (car starts) (min (length text) (car ends))))))

(defun pos-tree--archive-text (dir paths)
  "Return the old exclude text and its replacement for DIR and archive PATHS.
Preserve rules outside our block.  Quote glob characters and spaces in
anchored, directory-only Git patterns."
  (let* ((file (pos-tree--exclude-file dir))
         (old (with-temp-buffer
                (when (file-symlink-p file)
                  (pos-tree-refuse 'archive-excludes "The exclude file is a symbolic link"))
                (when (file-exists-p file) (insert-file-contents file))
                (buffer-string)))
         (bounds (pos-tree--archive-block old))
         (block (if (seq-empty-p paths) ""
                  (concat pos-tree--archive-begin
                          (mapconcat
                           (lambda (path)
                             (concat "/" (mapconcat
                                          (lambda (c)
                                            (concat (if (memq c '(?\\ ?* ?? ?\[ ?\] ?\s)) "\\" "")
                                                    (char-to-string c))) path "") "/\n"))
                           paths "")
                          pos-tree--archive-end))))
    (cons old (if bounds
                  (concat (substring old 0 (car bounds)) block (substring old (cdr bounds)))
                (concat old (if (and (not (string-empty-p old))
                                     (not (string-suffix-p "\n" old))
                                     (not (string-empty-p block))) "\n" "") block)))))

(defun pos-tree--archive-scopes (dir boundaries exclusions &optional prefix)
  "Find archive scopes below DIR, stopping at BOUNDARIES and configurations.
EXCLUSIONS are the node's, as `pos-node-exclusions' gives them: a
directory they name is not looked into, though one named archives is
a scope wherever it lies.  PREFIX is the path relative to the node,
or nil at its root."
  (let (scopes)
    (dolist (name (directory-files dir nil directory-files-no-dot-files-regexp))
      (let ((full (expand-file-name name dir)) (path (concat prefix name)))
        (unless (or (file-symlink-p full) (not (file-directory-p full))
                    (seq-some (lambda (boundary)
                                (or (equal path boundary)
                                    (string-prefix-p (concat boundary "/") path))) boundaries))
          (cond
           ((equal name "archives") (push (if prefix (directory-file-name prefix) ".") scopes))
           ((and (not (pos-node-unwalked-p path exclusions))
                 (not (pos-tree--repository-p full)) (not (pos-path-config-files full)))
            (setq scopes (append (pos-tree--archive-scopes full boundaries exclusions
                                                           (concat path "/"))
                                 scopes)))))))
    scopes))

(defun pos-tree--collect-archives (plan repo base config)
  "Collect for PLAN the uncommitted archives of CONFIG at BASE in REPO.
REPO is the repository's node."
  (let* ((dir (pos-tree--node-dir repo))
         (entries (append (alist-get 'archives config) nil))
         (boundaries (mapcar (lambda (e) (alist-get 'path e))
                             (append (alist-get 'children config) (alist-get 'worktrees config) nil)))
         (scopes (sort (delete-dups
                        (append '(".") (mapcar (lambda (e) (alist-get 'scope e)) entries)
                                (pos-tree--archive-scopes
                                 base boundaries (pos-node-exclusions config))))
                       #'string<)))
    (dolist (scope scopes)
      (unless (let ((at base) boundary)
                (dolist (part (unless (equal scope ".") (split-string scope "/")))
                  (setq at (expand-file-name part at))
                  (when (or (pos-tree--repository-p at) (pos-path-config-files at))
                    (setq boundary t)))
                boundary)
        (let* ((path (expand-file-name "archives" (expand-file-name scope base)))
               (within (file-relative-name path dir))
               (entry (seq-find (lambda (e) (equal (alist-get 'scope e) scope)) entries)))
          (cond
           ((pos-tree--through-link-p dir within)
            (pos-tree--find plan "path-taken" (pos-tree--rel plan path) "a symbolic link")
            (setf (pos-tree--node-archive-refused repo) t))
           ((equal (or (alist-get 'kept entry) "uncommitted") "uncommitted")
            (push within (pos-tree--node-archive-paths repo)))))))))

(defun pos-tree--archive-excludes (plan repo)
  "Add to PLAN the managed archive exclusions of REPO, a repository's node.
None if a node of it was refused."
  (unless (pos-tree--node-archive-refused repo)
    (let ((dir (pos-tree--node-dir repo))
          (paths (vconcat (sort (delete-dups (pos-tree--node-archive-paths repo))
                                #'string<))))
      (condition-case err
          (let ((texts (pos-tree--archive-text dir paths)))
            (unless (equal (car texts) (cdr texts))
              (pos-tree--act plan "archive-excludes" `(repository . ,(pos-tree--rel plan dir))
                             `(paths . ,paths))))
        ((pos-node-refused pos-tree-refused)
         (pos-tree--find plan "config-refused" (pos-tree--rel plan dir)
                         (format "%s: %s" (nth 1 err) (nth 2 err))))))))

(defun pos-tree--through-link-p (dir path)
  "Return non-nil if PATH, beneath DIR, is or passes through a symbolic link."
  (let ((at dir) found)
    (dolist (part (split-string path "/") found)
      (setq at (expand-file-name part at))
      (when (file-symlink-p at) (setq found t)))))

(defun pos-tree--empty-p (path)
  "Return non-nil if PATH is absent or an empty directory."
  (or (not (file-exists-p path))
      (and (file-directory-p path)
           (null (directory-files path nil directory-files-no-dot-files-regexp)))))

(defun pos-tree--skills-in (dir)
  "Return the skills of the directory DIR, each (NAME . FILE).
A skill is an entry of its .agents/skills that holds a SKILL.md, a
link to one among them, and FILE is that SKILL.md."
  (let ((in (expand-file-name ".agents/skills" dir)))
    (and (file-directory-p in)
         (delq nil
               (mapcar (lambda (name)
                         (let ((file (expand-file-name (concat name "/SKILL.md") in)))
                           (and (file-regular-p file) (cons name file))))
                       (directory-files in nil directory-files-no-dot-files-regexp))))))

(defun pos-tree--repeated (plan here above)
  "Find for PLAN each skill of HERE with the name of another in ABOVE.
Each is an alist as `pos-tree--skills-in' gives it.  Two with one
name and one text are the same skill, and are not found."
  (pcase-dolist (`(,name . ,file) here)
    (when-let* ((other (cdr (assoc name above))))
      (unless (equal (pos-bytes-read file) (pos-bytes-read other))
        (pos-tree--find plan "name-repeated"
                        (pos-tree--rel plan (file-name-directory file))
                        (pos-tree--rel plan (file-name-directory other)))))))

(defun pos-tree--undeclared (plan repo exclusions &optional prefix node above)
  "Find for PLAN what is beneath the repository REPO that no entry declares.
A repository, and a directory holding a configuration.  REPO is the
repository's node: the declared paths of repositories it has mounted
are not looked into, and those of its local directories are.
EXCLUSIONS are those in force, declared by the node at NODE, a prefix
with its final slash or nil for REPO; a local node that declares its
own replaces them beneath it.  PREFIX is the directory being looked
in, with its final slash, or nil for REPO.

Where REPO has a configuration, find as well each skill of a
directory that has the name of another skill in a directory above it
\(name-repeated).  ABOVE are the skills of the directories above
PREFIX in the repository, the nearest first."
  (let* ((dir (pos-tree--node-dir repo))
         (here (and (pos-tree--node-config repo)
                    (pos-tree--skills-in (expand-file-name (or prefix "") dir))))
         (above (progn (pos-tree--repeated plan here above)
                       (append here above))))
    (dolist (name (directory-files (expand-file-name (or prefix "") dir) nil
                                   directory-files-no-dot-files-regexp))
      (let* ((path (concat prefix name))
             (full (expand-file-name path dir)))
        (cond
         ((member path (pos-tree--node-mounted repo)))
         ((or (file-symlink-p full) (not (file-directory-p full))))
         ((pos-node-unwalked-p (string-remove-prefix (or node "") path) exclusions))
         ((member path (pos-tree--node-local repo))
          (let ((own (assoc path (pos-tree--node-local-exclusions repo))))
            (pos-tree--undeclared plan repo (if own (cdr own) exclusions)
                                  (concat path "/") (if own (concat path "/") node)
                                  above)))
         ;; A submodule is the repository's own, tracked and declared by git.
         ((pos-tree--repository-p full)
          (unless (pos-tree--tracked-p dir path)
            (pos-tree--find plan "undeclared" (pos-tree--rel plan full))))
         ((pos-path-config-files full)
          (pos-tree--find plan "undeclared" (pos-tree--rel plan full) "a configuration"))
         (t (pos-tree--undeclared plan repo exclusions
                                  (concat path "/") node above)))))))

(defun pos-tree--mount-child (plan repo base child)
  "Add to PLAN the declared CHILD, with a remote, of the node at BASE in REPO.
REPO is the repository's node.  Return (NODE . HELD): NODE is
\(ENTRY . NODE) for a child mounted as declared, else nil; HELD is the
child's directory when it is there and not planned, because of a
finding, else nil."
  (let-alist child
    (let* ((path (expand-file-name .path base))
           (dir (pos-tree--node-dir repo))
           (within (file-relative-name path dir))
           (shown (pos-tree--rel plan path)))
      (push within (pos-tree--node-mounted repo))
      (pos-tree--exclude plan dir within)
      (cond
       ((pos-tree--through-link-p dir within)
        (pos-tree--find plan "path-taken" shown "a symbolic link")
        nil)
       ((pos-tree--empty-p path)
        (pos-tree--act plan "clone" `(path . ,shown) `(remote . ,.remote)
                       `(branch . ,.branch))
        nil)
       ((not (pos-tree--repository-p path))
        (pos-tree--find plan "path-taken" shown "not a repository")
        nil)
       ((not (equal (pos-git-line path "config" "--get" "remote.origin.url")
                    .remote))
        (pos-tree--find plan "other-remote" shown)
        (cons nil path))
       ((not (equal (pos-git-line path "symbolic-ref" "--short" "-q" "HEAD")
                    .branch))
        (pos-tree--find plan "off-branch" shown (concat "declared " .branch))
        (cons nil path))
       (t (condition-case err
              (cons (cons `((path . ,within) ,@(assq-delete-all 'path (copy-alist child)))
                          (pos-tree--mounts plan path (pos-tree--config plan path .branch)))
                    nil)
            ((pos-node-refused pos-tree-refused)
             (pos-tree--find plan "config-refused" shown
                             (format "%s: %s" (nth 1 err) (nth 2 err)))
             (cons nil path))))))))

(defun pos-tree--declare-local (plan repo base child)
  "Add to PLAN the declared CHILD, a directory of the node at BASE in REPO.
REPO is the repository's node, and what the directory's own
configuration declares is planned as the repository's.  Return
\(NODES . HELD) as `pos-tree--declared' does, for what lies beneath."
  (let-alist child
    (let* ((path (expand-file-name .path base))
           (dir (pos-tree--node-dir repo))
           (within (file-relative-name path dir))
           (shown (pos-tree--rel plan path)))
      ;; A finding plans nothing beneath: nil.
      (cond
       ((pos-tree--through-link-p dir within)
        (pos-tree--find plan "path-taken" shown "a symbolic link") nil)
       ((not (file-exists-p path))
        (pos-tree--find plan "missing" shown) nil)
       ((not (file-directory-p path))
        (pos-tree--find plan "path-taken" shown "not a directory") nil)
       ((pos-tree--repository-p path)
        (push within (pos-tree--node-mounted repo))
        (pos-tree--find plan "path-taken" shown "a repository, declared with no remote") nil)
       (t
        (push within (pos-tree--node-local repo))
        (condition-case err
            (when-let* ((own (pos-tree--config plan path nil)))
              (push (cons path own) (pos-tree--node-locals repo))
              (unless (eq (alist-get 'exclude own) :null)
                (push (cons within (pos-node-exclusions own))
                      (pos-tree--node-local-exclusions repo)))
              (pos-tree--declared plan repo path own))
          ((pos-node-refused pos-tree-refused)
           (setf (pos-tree--node-archive-refused repo) t)
           (pos-tree--find plan "config-refused" shown
                           (format "%s: %s" (nth 1 err) (nth 2 err)))
           nil)))))))

(defun pos-tree--declare-worktree (plan repo base worktree)
  "Add to PLAN the declared WORKTREE of the node at BASE in REPO.
REPO is the repository's node."
  (let-alist worktree
    (let* ((path (expand-file-name .path base))
           (dir (pos-tree--node-dir repo))
           (within (file-relative-name path dir))
           (shown (pos-tree--rel plan path)))
      (push within (pos-tree--node-mounted repo))
      (pos-tree--exclude plan dir within)
      (cond
       ((pos-tree--through-link-p dir within)
        (pos-tree--find plan "path-taken" shown "a symbolic link"))
       ((pos-tree--empty-p path)
        (pos-tree--act plan "clone" `(path . ,shown)
                       (if .of
                           `(of . ,(pos-tree--rel plan (expand-file-name .of base)))
                         `(remote . ,.remote))
                       `(branch . ,.branch)))
       ((not (pos-tree--repository-p path))
        (pos-tree--find plan "path-taken" shown "not a repository"))))))

(defun pos-tree--declared (plan repo base config)
  "Add to PLAN what CONFIG declares, the configuration of the node at BASE.
BASE is the directory of REPO, a repository's node, or a directory
within it.  Return (NODES . HELD): a node for each repository mounted
as declared, as (ENTRY . NODE) with ENTRY's path relative to the
repository, and the directories of those that are there and are not
planned."
  (let ((by-path (lambda (a b) (string< (alist-get 'path a) (alist-get 'path b))))
        nodes held)
    (when config (pos-tree--collect-archives plan repo base config))
    (dolist (child (sort (append (alist-get 'children config) nil) by-path))
      (if (stringp (alist-get 'remote child))
          (pcase-let ((`(,node . ,taken) (pos-tree--mount-child plan repo base child)))
            (when node (push node nodes))
            (when taken (push taken held)))
        (pcase-let ((`(,below . ,taken) (pos-tree--declare-local plan repo base child)))
          (setq nodes (append (reverse below) nodes)
                held (append taken held)))))
    (dolist (worktree (sort (append (alist-get 'worktrees config) nil) by-path))
      (pos-tree--declare-worktree plan repo base worktree))
    (cons (nreverse nodes) held)))

(defun pos-tree--mounts (plan dir config)
  "Add to PLAN what is mounted in the repository at DIR, of CONFIG.
Return its node, with a node for each repository beneath it that is
mounted as declared, by this repository or by a directory of it."
  (let* ((repo (pos-tree--node :dir dir :config config))
         (declared (pos-tree--declared plan repo dir config)))
    (pos-tree--undeclared plan repo (pos-node-exclusions config))
    (when config (pos-tree--archive-excludes plan repo))
    (setf (pos-tree--node-children repo) (car declared)
          (pos-tree--node-held repo) (cdr declared)
          (pos-tree--node-locals repo) (nreverse (pos-tree--node-locals repo)))
    repo))

;;;; What is installed

(defconst pos-tree--note "README.clankos"
  "The name of the note the tool writes in an ordinary .claude/skills.")

(defconst pos-tree--convention
  "https://agentskills.io/client-implementation/adding-skills-support"
  "Where the convention of keeping skills in .agents/skills/ is described.")

(defun pos-tree--version (dir)
  "Return the version of DIR, a source or an auto directory, or nil.
It is the first line of the file version there."
  (let ((file (expand-file-name "version" dir)))
    (when (file-regular-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((line (string-trim (buffer-substring (point-min) (line-end-position)))))
          (and (not (string-empty-p line)) line))))))

(defun pos-tree--held (dir kind)
  "Return the names of KIND in DIR, a source or an auto directory.
KIND is \"skills\", for each directory holding a SKILL.md, or \"bin\",
for each file."
  (let ((in (expand-file-name kind dir)))
    (and (file-directory-p in)
         (seq-filter
          (lambda (name)
            (let ((path (expand-file-name name in)))
              (if (equal kind "skills")
                  (file-exists-p (expand-file-name "SKILL.md" path))
                (file-regular-p path))))
          (directory-files in nil directory-files-no-dot-files-regexp)))))

(defun pos-tree--check-source (source)
  "Refuse SOURCE, a directory to install from, unless it is one.
It names its version, and each of its skills is named clankos-NAME."
  (unless (pos-tree--version source)
    (pos-tree-refuse 'bad-source "The source names no version: %s" source))
  (dolist (name (pos-tree--held source "skills"))
    (unless (string-prefix-p "clankos-" name)
      (pos-tree-refuse 'bad-source "A skill's name does not begin clankos-: %s"
                       name))))

(defun pos-tree--auto (dir)
  "Return the auto directory of the repository at DIR, or nil.
It is in the configuration directory at DIR, which a repository whose
working tree has no configuration lacks."
  (when-let* ((file (pos-node-config-file dir)))
    (expand-file-name "auto" (expand-file-name (file-name-directory file) dir))))

(defun pos-tree--own-link-p (link owned)
  "Return non-nil if LINK is a symbolic link whose target is in one of OWNED.
OWNED are directories the tool's links point into: auto/, and the
methodologies of a node."
  (when-let* ((to (file-symlink-p link)))
    (let ((target (expand-file-name to (file-name-directory link))))
      (seq-some (lambda (dir) (string-prefix-p (file-name-as-directory dir) target))
                owned))))

(defun pos-tree--closed (dir path)
  "Return the first part of PATH under DIR that links cannot be made beneath.
That is one that is there and is a symbolic link or not a directory,
as a path relative to DIR; nil if every part is a directory or absent."
  (let ((at dir) (within nil) closed)
    (dolist (part (split-string path "/" t))
      (setq at (expand-file-name part at)
            within (if within (concat within "/" part) part))
      (when (and (not closed)
                 (or (file-symlink-p at)
                     (and (file-exists-p at) (not (file-directory-p at)))))
        (setq closed within)))
    closed))

(defun pos-tree--taken (plan dir path)
  "Find for PLAN that PATH, in the repository at DIR, is not the tool's.
Something else holds it.  The detail says whether the repository
tracks it and, if so, the commit that added it."
  (pos-tree--find plan
   "name-taken" (pos-tree--rel plan (expand-file-name path dir))
   (if (pos-tree--tracked-p dir path)
       (let ((added (pos-git-line dir "log" "--diff-filter=A"
                                        "--format=%h" "--" path)))
         (if (member added '(nil ""))
             "tracked, not yet committed"
           (concat "tracked, added in " (car (last (split-string added "\n"))))))
     "untracked")))

(defun pos-tree--link-in (plan dir within links owned)
  "Add to PLAN the tool's links in WITHIN, of the repository at DIR.
WITHIN is a directory of the repository.  LINKS is an alist
\(NAME . TARGETS): NAME is to be a link to the entry of that name in
TARGETS, a directory in one of OWNED, the directories the tool's links
point into.  A name held by something else is found and left; a name
LINKS gives twice is found for its second; and another of the tool's
links there is removed."
  (let* ((in (expand-file-name within dir))
         (closed (pos-tree--closed dir within))
         (names nil))
    (if closed
        (when links (pos-tree--taken plan dir closed))
      (pcase-dolist (`(,name . ,targets) links)
        (let* ((path (concat within "/" name))
               (link (expand-file-name name in))
               (shown (pos-tree--rel plan link))
               (target (file-relative-name (expand-file-name name targets) in)))
          (cond
           ((member name names)
            (pos-tree--find plan "name-taken" shown "named by more than one"))
           ((pos-tree--own-link-p link owned)
            (push name names)
            (pos-tree--exclude plan dir path)
            (unless (equal (file-symlink-p link) target)
              (pos-tree--act plan "unlink" `(path . ,shown))
              (pos-tree--act plan "link" `(path . ,shown) `(target . ,target))))
           ((or (file-symlink-p link) (file-exists-p link))
            (push name names)
            (pos-tree--taken plan dir path))
           (t
            (push name names)
            (pos-tree--exclude plan dir path)
            (pos-tree--act plan "link" `(path . ,shown) `(target . ,target))))))
      (when (file-directory-p in)
        (dolist (name (directory-files in nil directory-files-no-dot-files-regexp))
          (let ((link (expand-file-name name in)))
            (when (and (not (member name names)) (pos-tree--own-link-p link owned))
              (pos-tree--act plan "unlink" `(path . ,(pos-tree--rel plan link))))))))))

(defun pos-tree--not-skills (in)
  "Return the names in IN, a skills directory, that are no skill.
A skill is a directory holding a SKILL.md.  The tool's note is left out."
  (seq-remove
   (lambda (name)
     (or (equal name pos-tree--note)
         (file-exists-p (expand-file-name "SKILL.md" (expand-file-name name in)))))
   (directory-files in nil directory-files-no-dot-files-regexp)))

(defun pos-tree--note-text (in)
  "Return the text of the note for IN, an ordinary .claude/skills."
  (let ((others (pos-tree--not-skills in)))
    (concat
     "ClankOS made the symbolic links in this directory, and this note.\n\n"
     "Coding agents share skills from .agents/skills/. This directory's\n"
     "skills can be moved there, and .claude/skills replaced by a symbolic\n"
     "link to ../.agents/skills. ClankOS then makes its links in the one\n"
     "place and removes this note.\n"
     (and others
          (concat "\nA skill is a directory holding a SKILL.md. These entries are not,\n"
                  "and no agent reads them as skills:\n\n"
                  (mapconcat (lambda (name) (concat "  " name "\n")) others "")))
     "\nThe convention is described at\n" pos-tree--convention "\n")))

(defun pos-tree--earlier-link-p (plan dir name)
  "Return non-nil if NAME, in the skills at DIR, is a link an earlier tool made.
That is an untracked symbolic link whose target is a skill's directory
in another repository of the tree PLAN is for."
  (let* ((skills (file-name-as-directory (expand-file-name ".agents/skills" dir)))
         (to (file-symlink-p (expand-file-name name skills)))
         (target (and (stringp to) (expand-file-name to skills))))
    (and target
         (string-prefix-p (pos-tree--plan-root plan) target)
         (string-suffix-p "/.agents/skills/" (file-name-directory target))
         (not (equal (file-name-directory target) skills))
         (not (pos-tree--tracked-p dir (concat ".agents/skills/" name))))))

(defun pos-tree--agents-entries (dir &optional path)
  "Return the names in PATH, .agents/skills by default, of the repository at DIR.
None where a part of that path is not an ordinary directory."
  (let* ((path (or path ".agents/skills"))
         (skills (expand-file-name path dir)))
    (and (not (pos-tree--closed dir path))
         (file-directory-p skills)
         (directory-files skills nil directory-files-no-dot-files-regexp))))

(defun pos-tree--claude (plan dir prefix links owned)
  "Add to PLAN .claude/skills beneath PREFIX in the repository at DIR.
It is planned for LINKS.  PREFIX is a directory of the repository,
relative and ending in a slash, or empty for the repository itself.
LINKS and OWNED are as `pos-tree--link-in' takes them.  Absent,
.claude/skills is to be a link to ../.agents/skills when there are
skills there.  An ordinary directory has the tool's links made in it
too, and its note.  A file or a symbolic link, there or at .claude, is
left."
  (let* ((skills-path (concat prefix ".claude/skills"))
         (agents-path (concat prefix ".agents/skills"))
         (in (expand-file-name skills-path dir))
         (stray (expand-file-name (concat agents-path "/" pos-tree--note) dir))
         (ordinary (and (not (pos-tree--closed dir skills-path))
                        (file-directory-p in))))
    (cond
     ((pos-tree--closed dir skills-path))
     (ordinary
      (pos-tree--link-in plan dir skills-path links owned)
      (let ((path (concat skills-path "/" pos-tree--note))
            (note (expand-file-name pos-tree--note in)))
        (cond
         ((not links))
         ((pos-tree--tracked-p dir path) (pos-tree--taken plan dir path))
         (t (pos-tree--exclude plan dir path)
            (unless (and (file-regular-p note)
                         (equal (pos-tree--note-text in)
                                (with-temp-buffer (insert-file-contents note)
                                                  (buffer-string))))
              (pos-tree--act plan "note" `(path . ,(pos-tree--rel plan note))))))))
     ((or links (pos-tree--agents-entries dir agents-path))
      (pos-tree--exclude plan dir skills-path)
      (pos-tree--act plan "link" `(path . ,(pos-tree--rel plan in))
                     '(target . "../.agents/skills"))))
    ;; The note went with the directory's entries when they were moved.
    (when (and (not ordinary) (file-regular-p stray)
               (not (pos-tree--tracked-p
                     dir (concat agents-path "/" pos-tree--note))))
      (pos-tree--act plan "unlink" `(path . ,(pos-tree--rel plan stray))))))

(defun pos-tree--methodology-links (plan base config)
  "Return what the methodologies CONFIG declares at BASE supply.
A cons (SKILLS . COMMANDS), each an alist (NAME . DIRECTORY) as
`pos-tree--link-in' takes, by methodology and then by name.  A
methodology not there is left for its clone; one with a skill not named
after it is found for PLAN (methodology-refused) and supplies nothing."
  (let (skills commands)
    (pcase-dolist (`(,name . ,path) (pos-node-methodologies config))
      (let* ((dir (expand-file-name path base))
             (held (and (file-directory-p dir) (pos-tree--held dir "skills")))
             (bad (seq-find (lambda (skill)
                              (not (string-prefix-p (concat name "-") skill)))
                            held)))
        (cond
         ((not (file-directory-p dir)))
         (bad (pos-tree--find plan "methodology-refused" (pos-tree--rel plan dir)
                              (format "a skill not named %s-NAME: %s" name bad)))
         (t (dolist (skill held)
              (push (cons skill (expand-file-name "skills" dir)) skills))
            (dolist (command (pos-tree--held dir "bin"))
              (push (cons command (expand-file-name "bin" dir)) commands))))))
    (cons (nreverse skills) (nreverse commands))))

(defun pos-tree--in-directory (plan dir base config)
  "Add to PLAN the links to the methodologies of the node at BASE.
They are made in DIR.  BASE is a directory of the repository at DIR,
with CONFIG its own."
  (when (stringp (alist-get 'methodologies config))
    (let ((prefix (file-name-as-directory (file-relative-name base dir)))
          (owned (list (expand-file-name (alist-get 'methodologies config) base)))
          (bin (alist-get 'bin config)))
      (pcase-let ((`(,skills . ,commands) (pos-tree--methodology-links plan base config)))
        (pos-tree--link-in plan dir (concat prefix ".agents/skills") skills owned)
        (when (stringp bin)
          (pos-tree--link-in plan dir (concat prefix bin) commands owned))
        (pos-tree--claude plan dir prefix skills owned)))))

(defun pos-tree--installs (plan node)
  "Add to PLAN what is installed in NODE and in the repositories beneath it.
A repository with a configuration has auto/ in its configuration
directory, replaced from the source when its version is another, and
links to what auto/ holds and to what its methodologies supply.  A
node that is a directory of it has links to its own methodologies.
One with no configuration, a product, has nothing planned in it but
the removal of links an earlier tool made."
  (let* ((dir (pos-tree--node-dir node))
         (config (pos-tree--node-config node))
         (auto (and config (pos-tree--auto dir))))
    (dolist (name (pos-tree--agents-entries dir))
      (when (pos-tree--earlier-link-p plan dir name)
        (pos-tree--act plan "unlink"
                       `(path . ,(pos-tree--rel plan
                                  (expand-file-name (concat ".agents/skills/" name)
                                                    dir))))))
    (when auto
      (let* ((source (pos-tree--plan-source plan))
             (version (and source (pos-tree--version source)))
             (stale (and version (not (equal version (pos-tree--version auto)))))
             (from (if stale source auto))
             (at (alist-get 'methodologies config))
             (owned (if (stringp at) (list auto (expand-file-name at dir)) (list auto)))
             (supplied (pos-tree--methodology-links plan dir config))
             (skills (append (mapcar (lambda (name)
                                       (cons name (expand-file-name "skills" auto)))
                                     (pos-tree--held from "skills"))
                             (car supplied)))
             (commands (append (mapcar (lambda (name)
                                         (cons name (expand-file-name "bin" auto)))
                                       (pos-tree--held from "bin"))
                               (cdr supplied)))
             (bin (alist-get 'bin config)))
        (when (or stale (file-symlink-p auto) (file-exists-p auto))
          (pos-tree--exclude plan dir (file-relative-name auto dir)))
        (when stale
          (pos-tree--act plan "install" `(path . ,(pos-tree--rel plan auto))
                         `(version . ,version)))
        (pos-tree--link-in plan dir ".agents/skills" skills owned)
        (when (stringp bin)
          (pos-tree--link-in plan dir bin commands owned))
        (pos-tree--claude plan dir "" skills owned)))
    (pcase-dolist (`(,base . ,own) (pos-tree--node-locals node))
      (pos-tree--in-directory plan dir base own))
    (dolist (child (pos-tree--node-children node))
      (pos-tree--installs plan (cdr child)))))

(defun pos-tree-plan (root &optional source)
  "Return the plan for the tree at ROOT, a repository.
SOURCE, if given, is the directory to install from.  An alist of pos,
the version; actions, a vector of what needs to be done, in an order
it can be done in; findings, a vector of what was
found and is not acted on; and warnings, a vector of what the
configurations read said that this reader does not know, each a
string naming the node.  Paths in it are relative to ROOT.
Nothing is changed and no network is used.  Signal `pos-tree-refused'
if ROOT is not a repository, or SOURCE not a source."
  (let* ((dir (directory-file-name (expand-file-name root)))
         (source (and source (expand-file-name source)))
         (plan (pos-tree--plan-make :root (file-name-as-directory dir)
                                    :source source)))
    (unless (pos-tree--repository-p dir)
      (pos-tree-refuse 'not-a-repository "Not a repository: %s" root))
    (when source (pos-tree--check-source source))
    (condition-case err
        (pos-tree--installs plan (pos-tree--mounts plan dir (pos-tree--config plan dir nil)))
      ((pos-node-refused pos-tree-refused)
       (pos-tree--find plan "config-refused" "."
                       (format "%s: %s" (nth 1 err) (nth 2 err)))))
    `((pos . 2)
      (actions . ,(vconcat (reverse (pos-tree--plan-actions plan))))
      (findings . ,(vconcat (reverse (pos-tree--plan-findings plan))))
      (warnings . ,(vconcat (reverse (pos-tree--plan-warnings plan)))))))

;;;; The second step

(defun pos-tree--run (dir &rest args)
  "Run git in DIR with ARGS, never prompting; refuse if git fails."
  (let* ((process-environment (cons "GIT_TERMINAL_PROMPT=0" process-environment))
         (result (apply #'pos-git dir args)))
    (unless (eq (car result) 0)
      (pos-tree-refuse 'failed "git %s: %s" (string-join args " ") (cdr result)))))

(defun pos-tree--do (root action &optional source)
  "Do ACTION, of a plan for the tree at ROOT, a directory name.
SOURCE is the directory an install copies."
  (let-alist action
    (pcase .do
      ("install"
       (let ((auto (expand-file-name .path root)))
         (unless source
           (pos-tree-refuse 'failed "No source to install from: %s" .path))
         (cond
          ((and (file-directory-p auto) (not (file-symlink-p auto)))
           (delete-directory auto t))
          ((or (file-symlink-p auto) (file-exists-p auto))
           (delete-file auto)))
         (make-directory (file-name-directory auto) t)
         (copy-directory source auto nil nil t)))
      ("note"
       (let ((note (expand-file-name .path root)))
         (write-region (pos-tree--note-text (file-name-directory note))
                       nil note nil 'silent)))
      ("archive-excludes"
       (let* ((dir (expand-file-name .repository root))
              (file (pos-tree--exclude-file dir))
              (text (cdr (pos-tree--archive-text dir .paths))))
         (make-directory (file-name-directory file) t)
         (write-region text nil file nil 'silent)))
      ("exclude"
       (let ((file (pos-tree--exclude-file (expand-file-name .repository root))))
         (make-directory (file-name-directory file) t)
         (with-temp-buffer
           (when (file-exists-p file) (insert-file-contents file))
           (goto-char (point-max))
           (unless (or (bobp) (eq (char-before) ?\n)) (insert "\n"))
           (insert "/" .path "\n")
           (write-region nil nil file nil 'silent))))
      ("clone"
       (let ((path (expand-file-name .path root)))
         (make-directory (file-name-directory path) t)
         (if .of
             (let ((of (expand-file-name .of root)))
               (if (or (pos-git-line of "rev-parse" "--verify" "--quiet"
                                           (concat "refs/heads/" .branch))
                       (pos-git-line of "rev-parse" "--verify" "--quiet"
                                           (concat "refs/remotes/origin/" .branch)))
                   (pos-tree--run of "worktree" "add" "--quiet" path .branch)
                 (pos-tree--run of "worktree" "add" "--quiet" "-b" .branch path)))
           (pos-tree--run root "clone" "--quiet" "--branch" .branch "--"
                          .remote path))))
      ("link"
       (let ((link (expand-file-name .path root)))
         (when (or (file-symlink-p link) (file-exists-p link))
           (pos-tree-refuse 'failed "Something is at %s" .path))
         (make-directory (file-name-directory link) t)
         (condition-case err
             (make-symbolic-link .target link)
           (file-error
            (pos-tree-refuse 'failed "Cannot make a symbolic link at %s: %s"
                             .path (error-message-string err))))))
      ("unlink"
       (let ((link (expand-file-name .path root)))
         (unless (or (file-symlink-p link)
                     (equal (file-name-nondirectory link) pos-tree--note))
           (pos-tree-refuse 'failed "Not a link or a note: %s" .path))
         (delete-file link)))
      (_ (pos-tree-refuse 'failed "Not an action this tool does: %s" .do)))))

(defun pos-tree-apply (root plan &optional source)
  "Do the actions of PLAN in the tree at ROOT, and return the plan that remains.
PLAN is as `pos-tree-plan' returns it with SOURCE, the directory to
install from if any, or as parsed from its JSON.
Signal `pos-tree-refused' with stale-plan, having done nothing, if the
tree no longer gives PLAN; and with failed if an action cannot be done,
in which case what was done before it stays done.  Cloning uses the
network."
  (let ((dir (file-name-as-directory (expand-file-name root)))
        (fresh (pos-tree-plan root source)))
    (unless (equal (pos-bytes-json fresh) (pos-bytes-json plan))
      (pos-tree-refuse 'stale-plan "The tree no longer gives this plan"))
    (seq-doseq (action (alist-get 'actions fresh))
      (pos-tree--do dir action (and source (expand-file-name source))))
    (pos-tree-plan root source)))

(defun pos-tree-ignore-archives (root)
  "Apply only archive exclusions at ROOT; return the remaining tree plan.
Read a fresh plan.  Clone no repository, install nothing and change
no link."
  (seq-doseq (action (alist-get 'actions (pos-tree-plan root)))
    (when (equal (alist-get 'do action) "archive-excludes")
      (pos-tree--do (expand-file-name root) action)))
  (pos-tree-plan root))

(defun pos-tree-install (root source)
  "Apply only what is installed at ROOT from SOURCE; return the remaining plan.
Read a fresh plan made with SOURCE.  Do its exclude, install, link,
unlink and note actions.  Clone no repository and change no archive
rule."
  (let ((dir (file-name-as-directory (expand-file-name root)))
        (from (expand-file-name source)))
    (seq-doseq (action (alist-get 'actions (pos-tree-plan root source)))
      (when (member (alist-get 'do action)
                    '("exclude" "install" "link" "unlink" "note"))
        (pos-tree--do dir action from)))
    (pos-tree-plan root source)))

(autoload 'pos-tree-batch "pos-cli")

(provide 'pos-tree)
;;; pos-tree.el ends here
