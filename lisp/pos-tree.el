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
;; exclude and the skill links to make and remove, with what it found
;; and will not act on.  The agent files of section 5 are not planned:
;; a server is read and checked, and nothing is done with it.
;;
;; - `pos-tree-read-config': a config.yaml's text, checked.
;; - `pos-tree-plan': the plan for the tree at a root.
;; - `pos-tree-apply': do a plan, and return the plan that remains.
;; - `pos-tree-batch': the command line.

;;; Code:

(require 'cl-lib)
(require 'pos-ledger)
(require 'seq)
(require 'subr-x)
(require 'yaml)

(define-error 'pos-tree-refused "Tree configuration refused")

(defun pos-tree--refuse (kind format &rest args)
  "Signal a refusal of KIND, with a message from FORMAT and ARGS."
  (signal 'pos-tree-refused (list kind (apply #'format format args))))

;;;; The file

(defconst pos-tree--uuid-regexp
  (concat "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-"
          "[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'")
  "Matches a UUID in canonical form, when letters' case is not folded.")

(defun pos-tree--path-p (path)
  "Return non-nil if PATH is a path beneath a node: relative and going down.
It has no empty part, no part that is . or .., and no backslash."
  (and (not (string-empty-p path))
       (not (string-match-p "\\\\" path))
       (not (seq-some (lambda (part) (member part '("" "." "..")))
                      (split-string path "/")))))

(defun pos-tree--location (value what)
  "Return VALUE, the path of a WHAT, less a final slash; nil for nil.
Refuse a path that does not stay beneath its node."
  (when value
    (let ((path (if (and (> (length value) 1) (string-suffix-p "/" value))
                    (substring value 0 -1)
                  value)))
      (unless (pos-tree--path-p path)
        (pos-tree--refuse 'bad-path "Not a path for %s: %s" what value))
      path)))

(defun pos-tree--within-p (path container)
  "Return non-nil if PATH is CONTAINER or beneath it."
  (or (equal path container) (string-prefix-p (concat container "/") path)))

(defun pos-tree--mapping-p (value)
  "Return non-nil if VALUE, as parsed from YAML, is a mapping."
  (and (listp value)
       (seq-every-p (lambda (pair) (and (consp pair) (stringp (car pair))))
                    value)))

(defun pos-tree--typed (value type)
  "Return a list of VALUE read as TYPE, or nil if it is not of TYPE.
VALUE is as parsed from YAML, every scalar the text that was written.
TYPE is one of string, boolean, integer, sequence and mapping.  A
string is any text but the empty one and the two words for null; a
boolean is true or false, returned as t or :false; an integer is a
whole number in digits, returned as a number."
  (pcase type
    ('string (and (stringp value)
                  (not (member value '("" "null" "~")))
                  (list value)))
    ('boolean (pcase value ("true" (list t)) ("false" (list :false))))
    ('integer (and (stringp value)
                   (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" value)
                   (list (string-to-number value))))
    ('sequence (and (vectorp value) (list value)))
    ('mapping (and (pos-tree--mapping-p value) (list value)))))

(defun pos-tree--mapping (value what keys)
  "Return the mapping VALUE as an alist of symbols, checked against KEYS.
WHAT names the mapping in a refusal.  KEYS is a list of (NAME TYPE
REQUIRED): a key not among them, a value not of its TYPE and a
REQUIRED key that is absent are each refused."
  (unless (pos-tree--mapping-p value)
    (pos-tree--refuse 'not-a-mapping "%s is not a mapping" what))
  (dolist (pair value)
    (unless (assoc (car pair) keys)
      (pos-tree--refuse 'unknown-key "%s has a key this version does not define: %s"
                        what (car pair))))
  (mapcan (lambda (key)
            (pcase-let* ((`(,name ,type ,required) key)
                         (pair (assoc name value))
                         (typed (and pair (pos-tree--typed (cdr pair) type))))
              (cond
               (typed (list (cons (intern name) (car typed))))
               (pair (pos-tree--refuse 'wrong-type "%s: %s is not a %s"
                                       what name type))
               (required (pos-tree--refuse 'missing-key "%s lacks %s" what name)))))
          keys))

(defun pos-tree--distinct (paths what)
  "Refuse if PATHS, each naming a WHAT, hold one path twice."
  (let ((seen nil))
    (dolist (path paths)
      (when (member path seen)
        (pos-tree--refuse 'bad-path "%s is named twice: %s" what path))
      (push path seen))))

(defun pos-tree--children (entries)
  "Return the children in ENTRIES, a sequence, checked, defaults filled in."
  (let ((children
         (mapcar
          (lambda (entry)
            (let-alist (pos-tree--mapping
                        entry "A child"
                        '(("path" string t) ("remote" string nil)
                          ("branch" string nil) ("skills-up" boolean nil)))
              (unless (pos-tree--path-p .path)
                (pos-tree--refuse 'bad-path "Not a child's path: %s" .path))
              ;; A branch and skills are a repository's: a child with
              ;; no remote is a directory of this one.
              (when (and (not .remote) (or .branch .skills-up))
                (pos-tree--refuse 'bad-value
                                  "A child with no remote has no branch or skills-up: %s"
                                  .path))
              `((path . ,.path) (remote . ,(or .remote :null))
                (branch . ,(if .remote (or .branch "master") :null))
                (skills-up . ,(or .skills-up :false)))))
          entries)))
    (dolist (a children)
      (dolist (b children)
        (when (and (not (eq a b))
                   (pos-tree--within-p (alist-get 'path b) (alist-get 'path a)))
          (pos-tree--refuse 'bad-path "A child's path is another's or beneath it: %s"
                            (alist-get 'path b)))))
    children))

(defun pos-tree--own-scope (path children what)
  "Refuse if PATH, a WHAT's scope, is one of CHILDREN or beneath one."
  (dolist (child children)
    (when (pos-tree--within-p path (alist-get 'path child))
      (pos-tree--refuse 'bad-path "%s is in a child, which declares its own: %s"
                        what path))))

(defun pos-tree--worktrees (entries children)
  "Return the worktrees in ENTRIES, a sequence, checked, defaults filled in.
CHILDREN are the repository's children, already checked."
  (let ((worktrees
         (mapcar
          (lambda (entry)
            (let-alist (pos-tree--mapping
                        entry "A worktree"
                        '(("path" string t) ("of" string nil)
                          ("remote" string nil) ("branch" string nil)))
              (unless (and (pos-tree--path-p .path)
                           (string-match "\\`\\(?:\\(.+\\)/\\)?_worktrees/[^/]+\\'"
                                         .path))
                (pos-tree--refuse 'bad-path "Not a worktree's path: %s" .path))
              (when-let* ((scope (match-string 1 .path)))
                (pos-tree--own-scope scope children "A worktree"))
              (unless (eq (null .of) (not (null .remote)))
                (pos-tree--refuse 'bad-value "A worktree has one of of and remote: %s"
                                  .path))
              (cond
               (.remote `((path . ,.path) (remote . ,.remote)
                          (branch . ,(or .branch "master"))))
               ((not (seq-some (lambda (child)
                                 (and (equal (alist-get 'path child) .of)
                                      (stringp (alist-get 'remote child))))
                               children))
                (pos-tree--refuse 'bad-value
                                  "A worktree is of no declared child with a remote: %s"
                                  .of))
               ((not .branch)
                (pos-tree--refuse 'missing-key "A worktree of a child lacks branch"))
               (t `((path . ,.path) (of . ,.of) (branch . ,.branch))))))
          entries)))
    (pos-tree--distinct (mapcar (lambda (w) (alist-get 'path w)) worktrees)
                        "A worktree")
    worktrees))

(defun pos-tree--archives (entries children)
  "Return the archive settings in ENTRIES, a sequence, checked.
CHILDREN are the repository's children, already checked."
  (let ((archives
         (mapcar
          (lambda (entry)
            (let-alist (pos-tree--mapping
                        entry "An archive"
                        '(("scope" string t) ("kept" string t)
                          ("ledger" string nil) ("url" string nil)))
              (unless (equal .scope ".")
                (unless (pos-tree--path-p .scope)
                  (pos-tree--refuse 'bad-path "Not a scope's path: %s" .scope))
                (pos-tree--own-scope .scope children "An archive"))
              (unless (member .kept '("committed" "uncommitted" "remote"))
                (pos-tree--refuse 'bad-value "An archive is not kept %s" .kept))
              (when (and .ledger
                         (not (let ((case-fold-search nil))
                                (string-match-p pos-tree--uuid-regexp .ledger))))
                (pos-tree--refuse 'bad-value "Not a ledger's id: %s" .ledger))
              (let ((ledger (and .ledger `((ledger . ,.ledger)))))
                (cond
                 ((not (equal .kept "remote"))
                  (when .url
                    (pos-tree--refuse 'bad-value "Only a remote archive has a url: %s"
                                      .scope))
                  `((scope . ,.scope) (kept . ,.kept) ,@ledger))
                 (.url `((scope . ,.scope) (kept . ,.kept) ,@ledger (url . ,.url)))
                 (t (pos-tree--refuse 'missing-key "A remote archive lacks url"))))))
          entries)))
    (pos-tree--distinct (mapcar (lambda (a) (alist-get 'scope a)) archives)
                        "An archive's scope")
    (let ((seen nil))
      (dolist (archive archives)
        (when-let* ((ledger (alist-get 'ledger archive)))
          (when (member ledger seen)
            (pos-tree--refuse 'bad-value "A ledger is named twice: %s" ledger))
          (push ledger seen))))
    archives))

(defun pos-tree-read-config (text)
  "Return the configuration in TEXT, a config.yaml's, checked.
An alist of kind, projects, methodologies, image, children,
worktrees, archives and server, with defaults filled in: sequences are
vectors, an absent value is :null and false is :false.  kind is
\"responsibility\" for a node that says where its projects belong,
\"project\" for one that says where its methodologies belong, and :null
for one that says neither, which is yet to be configured.  Every
scalar is read as the text written, and the keys' types decide what
it is.  Signal `pos-tree-refused', with the refusal's kind and a
message, for a file doc/pos-directory.txt does not allow."
  (let ((parsed (condition-case nil
                    (yaml-parse-string text :object-type 'alist
                                       :object-key-type 'string
                                       :sequence-type 'array
                                       :string-values t)
                  (error (pos-tree--refuse 'not-yaml "Not readable as YAML")))))
    (unless (pos-tree--mapping-p parsed)
      (pos-tree--refuse 'not-a-mapping "The file is not a mapping"))
    (let ((version (car (pos-tree--typed (cdr (assoc "pos" parsed)) 'integer))))
      (when (and version (/= version 2))
        (pos-tree--refuse 'unknown-version "Not a version this reader knows: %s"
                          version)))
    (let* ((top (pos-tree--mapping
                 parsed "The file"
                 '(("pos" integer t) ("projects" string nil)
                   ("methodologies" string nil) ("image" string nil)
                   ("children" sequence nil)
                   ("worktrees" sequence nil) ("archives" sequence nil)
                   ("server" mapping nil))))
           (projects (pos-tree--location (alist-get 'projects top) "projects"))
           (methodologies (pos-tree--location (alist-get 'methodologies top)
                                              "methodologies"))
           (image (alist-get 'image top))
           (children (pos-tree--children (alist-get 'children top)))
           (server (assq 'server top)))
      (when (and projects methodologies)
        (pos-tree--refuse 'bad-value
                          "A node says where its projects belong or its methodologies, not both"))
      ;; A tool with no YAML reader finds the image by its line.
      (when (and image
                 (not (let ((case-fold-search nil))
                        (and (string-match-p "\\`[^ \t\n\"'#]+\\'" image)
                             (string-match-p
                              (concat "^image: " (regexp-quote image) "$")
                              text)))))
        (pos-tree--refuse 'bad-value
                          "The image is not on one line, as image: NAME, unquoted"))
      `((kind . ,(cond (projects "responsibility") (methodologies "project")
                       (t :null)))
        (projects . ,(or projects :null))
        (methodologies . ,(or methodologies :null))
        (image . ,(or image :null))
        (children . ,(vconcat children))
        (worktrees . ,(vconcat (pos-tree--worktrees (alist-get 'worktrees top)
                                                    children)))
        (archives . ,(vconcat (pos-tree--archives (alist-get 'archives top)
                                                  children)))
        (server . ,(if server
                       (pos-tree--mapping
                        (cdr (assoc "server" parsed)) "The server"
                        '(("name" string t) ("mcp" string t)))
                     :null))))))

;;;; Git

(defun pos-tree--git (dir &rest args)
  "Run git in DIR with ARGS; return (STATUS . OUTPUT).
OUTPUT is what git printed, less its final newline."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (status (apply #'process-file "git" nil (list t nil) nil args)))
      (cons status (string-trim-right (buffer-string) "\n")))))

(defun pos-tree--git-line (dir &rest args)
  "Return what git printed in DIR with ARGS, or nil if it failed."
  (let ((result (apply #'pos-tree--git dir args)))
    (and (eq (car result) 0) (cdr result))))

(defun pos-tree--repository-p (dir)
  "Return non-nil if DIR is the root of a repository's working tree."
  (file-exists-p (expand-file-name ".git" dir)))

(defun pos-tree--tracked-p (dir path)
  "Return non-nil if the repository at DIR tracks PATH."
  (eq 0 (car (pos-tree--git dir "ls-files" "--error-unmatch" "--" path))))

(defun pos-tree--exclude-file (dir)
  "Return the info/exclude file of the repository at DIR, or nil."
  (when-let* ((file (pos-tree--git-line dir "rev-parse" "--git-path" "info/exclude")))
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

(defun pos-tree--one-config (files)
  "Return the one of FILES, the configuration paths found in a node, or nil.
Signal `pos-tree-refused' if there are two."
  (when (cdr files)
    (pos-tree--refuse 'two-configurations "Two configurations: %s"
                      (string-join files ", ")))
  (car files))

(defun pos-tree-config-file (dir)
  "Return the configuration file of the node at DIR, relative, or nil.
Signal `pos-tree-refused' if it has two."
  (pos-tree--one-config
   (pos-ledger-config-files dir)))

(defun pos-tree--config (dir branch)
  "Return the configuration of the node at DIR, or nil if it has none.
With BRANCH, DIR is a repository and it is read from that branch as
committed; with nil, from the working tree.  Signal `pos-tree-refused'
for a file that is refused, and for two configurations."
  (when-let* ((text (if branch
                       (when-let* ((file (pos-tree--one-config
                                          (split-string
                                           (or (apply #'pos-tree--git-line
                                                      dir "ls-tree" "--name-only"
                                                      "-r" branch "--"
                                                      (pos-ledger-config-paths))
                                               "")
                                           "\n" t))))
                         (pos-tree--git-line dir "show" (concat branch ":" file)))
                     (when-let* ((file (pos-tree-config-file dir)))
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name file dir))
                         (buffer-string))))))
    (pos-tree-read-config text)))

;;;; The plan

(defvar pos-tree--root nil
  "The root of the tree being planned, as a directory name.")

(defvar pos-tree--actions nil
  "The actions of the plan being made, newest first.")

(defvar pos-tree--findings nil
  "The findings of the plan being made, newest first.")

(cl-defstruct (pos-tree--node (:constructor pos-tree--node))
  "A repository of the tree that is mounted as declared.
CHILDREN are its children mounted as declared, each (ENTRY . NODE).
HELD are the directories of its children that are there and are not
planned, because of a finding."
  dir config children held)

(defun pos-tree--rel (path)
  "Return PATH relative to the root of the tree being planned."
  (file-relative-name (directory-file-name path) pos-tree--root))

(defun pos-tree--act (do &rest pairs)
  "Add to the plan the action DO, with PAIRS, an alist."
  (push `((do . ,do) ,@pairs) pos-tree--actions))

(defun pos-tree--find (finding path &optional detail)
  "Add to the plan the FINDING at PATH, with DETAIL if any."
  (push `((finding . ,finding) (path . ,path)
          ,@(and detail `((detail . ,detail))))
        pos-tree--findings))

(defun pos-tree--exclude (dir path)
  "Plan to exclude PATH in the repository at DIR, unless it is excluded."
  (unless (pos-tree--excluded-p dir path)
    (pos-tree--act "exclude" `(repository . ,(pos-tree--rel dir)) `(path . ,path))))

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

(defconst pos-tree--unwalked '("archives" "attic" "node_modules")
  "Names of directories not looked into for what is undeclared.
Nor are hidden directories and those beginning with an underscore.")

(defun pos-tree--undeclared (repo mounted local &optional prefix)
  "Find what is beneath the repository at REPO that no entry declares.
A repository, and a directory holding a configuration.  MOUNTED are the
declared paths of repositories, which are not looked into, and LOCAL
those of directories, which are; both relative to REPO.  PREFIX is the
directory being looked in, with its final slash, or nil for REPO."
  (dolist (name (directory-files (expand-file-name (or prefix "") repo) nil
                                 directory-files-no-dot-files-regexp))
    (let* ((path (concat prefix name))
           (full (expand-file-name path repo)))
      (cond
       ((member path mounted))
       ((or (file-symlink-p full) (not (file-directory-p full))))
       ((or (member name pos-tree--unwalked)
            (string-prefix-p "." name) (string-prefix-p "_" name)))
       ((member path local)
        (pos-tree--undeclared repo mounted local (concat path "/")))
       ;; A submodule is the repository's own, tracked and declared by git.
       ((pos-tree--repository-p full)
        (unless (pos-tree--tracked-p repo path)
          (pos-tree--find "undeclared" (pos-tree--rel full))))
       ((pos-ledger-config-files full)
        (pos-tree--find "undeclared" (pos-tree--rel full) "a configuration"))
       (t (pos-tree--undeclared repo mounted local (concat path "/")))))))

(defvar pos-tree--mounted nil
  "The declared paths of repositories in the repository being planned.")

(defvar pos-tree--local nil
  "The declared paths of directories in the repository being planned.")

(defun pos-tree--declared (repo base config)
  "Plan what CONFIG declares, the configuration of the node at BASE.
BASE is the repository at REPO or a directory of it.  Return
\(NODES . HELD): a node for each repository mounted as declared, as
\(ENTRY . NODE) with ENTRY's path relative to REPO, and the directories
of those that are there and are not planned."
  (let ((children (sort (append (alist-get 'children config) nil)
                        (lambda (a b) (string< (alist-get 'path a) (alist-get 'path b)))))
        (worktrees (sort (append (alist-get 'worktrees config) nil)
                         (lambda (a b) (string< (alist-get 'path a) (alist-get 'path b)))))
        nodes held)
    (when (eq (alist-get 'kind config) :null)
      (pos-tree--find "unconfigured" (pos-tree--rel base)))
    (dolist (child children)
      (let-alist child
        (let* ((path (expand-file-name .path base))
               (within (file-relative-name path repo))
               (shown (pos-tree--rel path)))
          (cond
           ((stringp .remote)
            (push within pos-tree--mounted)
            (pos-tree--exclude repo within)
            (cond
             ((pos-tree--through-link-p repo within)
              (pos-tree--find "path-taken" shown "a symbolic link"))
             ((pos-tree--empty-p path)
              (pos-tree--act "clone" `(path . ,shown) `(remote . ,.remote)
                             `(branch . ,.branch)))
             ((not (pos-tree--repository-p path))
              (pos-tree--find "path-taken" shown "not a repository"))
             ((not (equal (pos-tree--git-line path "config" "--get" "remote.origin.url")
                          .remote))
              (push path held)
              (pos-tree--find "other-remote" shown))
             ((not (equal (pos-tree--git-line path "symbolic-ref" "--short" "-q" "HEAD")
                          .branch))
              (push path held)
              (pos-tree--find "off-branch" shown (concat "declared " .branch)))
             (t (condition-case err
                    (push (cons `((path . ,within) ,@(assq-delete-all 'path
                                                                     (copy-alist child)))
                                (pos-tree--mounts
                                 path (pos-tree--config path .branch)))
                          nodes)
                  (pos-tree-refused
                   (push path held)
                   (pos-tree--find "config-refused" shown
                                   (format "%s: %s" (nth 1 err) (nth 2 err))))))))
           ;; A directory of this repository: what it declares is
           ;; planned as this repository's.
           ((pos-tree--through-link-p repo within)
            (pos-tree--find "path-taken" shown "a symbolic link"))
           ((not (file-exists-p path))
            (pos-tree--find "missing" shown))
           ((not (file-directory-p path))
            (pos-tree--find "path-taken" shown "not a directory"))
           ((pos-tree--repository-p path)
            (push within pos-tree--mounted)
            (pos-tree--find "path-taken" shown "a repository, declared with no remote"))
           (t
            (push within pos-tree--local)
            (condition-case err
                (when-let* ((own (pos-tree--config path nil)))
                  (let ((below (pos-tree--declared repo path own)))
                    (setq nodes (append (reverse (car below)) nodes)
                          held (append (cdr below) held))))
              (pos-tree-refused
               (pos-tree--find "config-refused" shown
                               (format "%s: %s" (nth 1 err) (nth 2 err))))))))))
    (dolist (worktree worktrees)
      (let-alist worktree
        (let* ((path (expand-file-name .path base))
               (within (file-relative-name path repo))
               (shown (pos-tree--rel path)))
          (push within pos-tree--mounted)
          (pos-tree--exclude repo within)
          (cond
           ((pos-tree--through-link-p repo within)
            (pos-tree--find "path-taken" shown "a symbolic link"))
           ((pos-tree--empty-p path)
            (pos-tree--act "clone" `(path . ,shown)
                           (if .of
                               `(of . ,(pos-tree--rel (expand-file-name .of base)))
                             `(remote . ,.remote))
                           `(branch . ,.branch)))
           ((not (pos-tree--repository-p path))
            (pos-tree--find "path-taken" shown "not a repository"))))))
    (cons (nreverse nodes) held)))

(defun pos-tree--mounts (dir config)
  "Plan what is mounted in the repository at DIR, whose configuration is CONFIG.
Return its node, with a node for each repository beneath it that is
mounted as declared, by this repository or by a directory of it."
  (let* ((pos-tree--mounted nil)
         (pos-tree--local nil)
         (declared (pos-tree--declared dir dir config)))
    (pos-tree--undeclared dir pos-tree--mounted pos-tree--local)
    (pos-tree--node :dir dir :config config :children (car declared)
                    :held (cdr declared))))

;;;; Skills

(defun pos-tree--skills-dir (dir)
  "Return the directory of the skills of the repository at DIR."
  (expand-file-name ".agents/skills" dir))

(defun pos-tree--entries (dir)
  "Return the names in the skills directory of the repository at DIR."
  (let ((skills (pos-tree--skills-dir dir)))
    (and (file-directory-p skills)
         (directory-files skills nil directory-files-no-dot-files-regexp))))

(defun pos-tree--linkable (dir)
  "Return the skills of the repository at DIR that are linked elsewhere.
An alist of (NAME . DIRECTORY): its own skills, which are directories
and not links, less any holding a .pos-local file."
  (let ((skills (pos-tree--skills-dir dir)))
    (delq nil
          (mapcar (lambda (name)
                    (let ((skill (expand-file-name name skills)))
                      (and (not (file-symlink-p skill))
                           (file-exists-p (expand-file-name "SKILL.md" skill))
                           (not (file-exists-p (expand-file-name ".pos-local" skill)))
                           (cons name skill))))
                  (pos-tree--entries dir)))))

(defun pos-tree--tool-link-p (dir name)
  "Return non-nil if NAME, in the skills at DIR, is a link this tool made.
That is an untracked symbolic link whose target is a skill's directory
in another repository of the tree."
  (let* ((skills (file-name-as-directory (pos-tree--skills-dir dir)))
         (to (file-symlink-p (expand-file-name name skills)))
         (target (and (stringp to) (expand-file-name to skills))))
    (and target
         (string-prefix-p pos-tree--root target)
         (string-suffix-p "/.agents/skills/" (file-name-directory target))
         (not (equal (file-name-directory target) skills))
         (not (pos-tree--tracked-p dir (concat ".agents/skills/" name))))))

(defun pos-tree--claude-link (dir wanted)
  "Plan the .claude/skills link of the repository at DIR.
It is made only if WANTED, which says the repository has skills."
  (let* ((link (expand-file-name ".claude/skills" dir))
         (to (file-symlink-p link)))
    (cond
     ((and (stringp to) (equal (directory-file-name to) "../.agents/skills")))
     (to (pos-tree--find "claude-skills" (pos-tree--rel link) "a link elsewhere"))
     ((file-exists-p link)
      (pos-tree--find "claude-skills" (pos-tree--rel link) "not a link"))
     (wanted
      (pos-tree--exclude dir ".claude/skills")
      (pos-tree--act "link" `(path . ,(pos-tree--rel link))
                     '(target . "../.agents/skills"))))))

(defun pos-tree--links (node containers)
  "Plan the skill links of NODE and of the repositories beneath it.
CONTAINERS are the repositories above it, nearest first, each an alist
of its linkable skills."
  (let* ((dir (pos-tree--node-dir node))
         (skills (file-name-as-directory (pos-tree--skills-dir dir)))
         (entries (pos-tree--entries dir))
         (made (seq-filter (lambda (name) (pos-tree--tool-link-p dir name)) entries))
         (taken (seq-difference entries made))
         desired offered)
    ;; Down: each container's own skills, the nearer first.
    (dolist (container containers)
      (dolist (skill container)
        (unless (or (member (car skill) taken) (assoc (car skill) desired))
          (push skill desired))))
    ;; Up: the own skills of each child marked for it.
    (dolist (child (pos-tree--node-children node))
      (when (eq (alist-get 'skills-up (car child)) t)
        (dolist (skill (pos-tree--linkable (pos-tree--node-dir (cdr child))))
          (unless (or (member (car skill) taken) (assoc (car skill) desired))
            (push skill offered)))))
    (dolist (skill (reverse offered))
      (cond
       ((= 1 (seq-count (lambda (other) (equal (car other) (car skill))) offered))
        (push skill desired))
       ((eq skill (seq-find (lambda (other) (equal (car other) (car skill)))
                            (reverse offered)))
        (pos-tree--find "name-clash"
                        (pos-tree--rel (expand-file-name (car skill) skills))))))
    (setq desired (sort desired (lambda (a b) (string< (car a) (car b)))))
    (dolist (skill desired)
      (let* ((name (car skill))
             (link (expand-file-name name skills))
             (target (file-relative-name (cdr skill) skills)))
        (pos-tree--exclude dir (concat ".agents/skills/" name))
        (cond
         ((not (member name made))
          (pos-tree--act "link" `(path . ,(pos-tree--rel link)) `(target . ,target)))
         ((not (equal (file-symlink-p link) target))
          (pos-tree--act "unlink" `(path . ,(pos-tree--rel link)))
          (pos-tree--act "link" `(path . ,(pos-tree--rel link)) `(target . ,target))))))
    ;; A link into a child that is held is left: nothing is done about a
    ;; child with a finding, its skills in this repository included.
    (dolist (name made)
      (let ((target (expand-file-name
                     (file-symlink-p (expand-file-name name skills)) skills)))
        (unless (or (assoc name desired)
                    (seq-some (lambda (dir)
                                (string-prefix-p (file-name-as-directory dir) target))
                              (pos-tree--node-held node)))
          (pos-tree--act "unlink"
                         `(path . ,(pos-tree--rel (expand-file-name name skills)))))))
    (pos-tree--claude-link dir (or desired entries))
    (let ((below (cons (pos-tree--linkable dir) containers)))
      (dolist (child (pos-tree--node-children node))
        (pos-tree--links (cdr child) below)))))

(defun pos-tree-plan (root)
  "Return the plan for the tree at ROOT, a repository.
An alist of pos, the version; actions, a vector of what needs to be
done, in an order it can be done in; and findings, a vector of what
was found and is not acted on.  Paths in it are relative to ROOT.
Nothing is changed and no network is used.  Signal `pos-tree-refused'
if ROOT is not a repository."
  (let* ((dir (directory-file-name (expand-file-name root)))
         (pos-tree--root (file-name-as-directory dir))
         (pos-tree--actions nil)
         (pos-tree--findings nil))
    (unless (pos-tree--repository-p dir)
      (pos-tree--refuse 'not-a-repository "Not a repository: %s" root))
    (condition-case err
        (pos-tree--links (pos-tree--mounts dir (pos-tree--config dir nil)) nil)
      (pos-tree-refused
       (pos-tree--find "config-refused" "."
                       (format "%s: %s" (nth 1 err) (nth 2 err)))))
    `((pos . 2)
      (actions . ,(vconcat (nreverse pos-tree--actions)))
      (findings . ,(vconcat (nreverse pos-tree--findings))))))

;;;; The second step

(defun pos-tree--run (dir &rest args)
  "Run git in DIR with ARGS, never prompting; refuse if git fails."
  (let* ((process-environment (cons "GIT_TERMINAL_PROMPT=0" process-environment))
         (result (apply #'pos-tree--git dir args)))
    (unless (eq (car result) 0)
      (pos-tree--refuse 'failed "git %s: %s" (string-join args " ") (cdr result)))))

(defun pos-tree--do (root action)
  "Do ACTION, of a plan for the tree at ROOT, a directory name."
  (let-alist action
    (pcase .do
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
               (if (or (pos-tree--git-line of "rev-parse" "--verify" "--quiet"
                                           (concat "refs/heads/" .branch))
                       (pos-tree--git-line of "rev-parse" "--verify" "--quiet"
                                           (concat "refs/remotes/origin/" .branch)))
                   (pos-tree--run of "worktree" "add" "--quiet" path .branch)
                 (pos-tree--run of "worktree" "add" "--quiet" "-b" .branch path)))
           (pos-tree--run root "clone" "--quiet" "--branch" .branch "--"
                          .remote path))))
      ("link"
       (let ((link (expand-file-name .path root)))
         (when (or (file-symlink-p link) (file-exists-p link))
           (pos-tree--refuse 'failed "Something is at %s" .path))
         (make-directory (file-name-directory link) t)
         (make-symbolic-link .target link)))
      ("unlink"
       (let ((link (expand-file-name .path root)))
         (unless (file-symlink-p link)
           (pos-tree--refuse 'failed "Not a link: %s" .path))
         (delete-file link)))
      (_ (pos-tree--refuse 'failed "Not an action this tool does: %s" .do)))))

(defun pos-tree-apply (root plan)
  "Do the actions of PLAN in the tree at ROOT, and return the plan that remains.
PLAN is as `pos-tree-plan' returns it, or as parsed from its JSON.
Signal `pos-tree-refused' with stale-plan, having done nothing, if the
tree no longer gives PLAN; and with failed if an action cannot be done,
in which case what was done before it stays done.  Cloning uses the
network."
  (let ((dir (file-name-as-directory (expand-file-name root)))
        (fresh (pos-tree-plan root)))
    (unless (equal (pos-ledger-json fresh) (pos-ledger-json plan))
      (pos-tree--refuse 'stale-plan "The tree no longer gives this plan"))
    (seq-doseq (action (alist-get 'actions fresh))
      (pos-tree--do dir action))
    (pos-tree-plan root)))

;;;; Command line

(defconst pos-tree-usage
  "Usage: COMMAND ...  (help prints this; Emacs itself takes --help)

  plan ROOT
      print what needs to be done for the tree at ROOT to be as its
      configurations declare, as JSON; change nothing
  apply ROOT PLAN
      do what the plan in the file PLAN holds, or - for standard input,
      if the tree at ROOT still gives it; print the plan that remains

Exit 0 nothing to do, 1 something to do or to report, 2 refused.
"
  "The command line's usage.")

(defun pos-tree--print (plan)
  "Print PLAN as JSON and exit, with 0 for an empty plan and 1 otherwise."
  (princ (decode-coding-string (pos-ledger-json plan) 'utf-8))
  (kill-emacs (if (and (seq-empty-p (alist-get 'actions plan))
                       (seq-empty-p (alist-get 'findings plan)))
                  0
                1)))

(defun pos-tree-batch ()
  "Run a command from `command-line-args-left', as in `pos-tree-usage'."
  (condition-case err
      (pcase (prog1 command-line-args-left (setq command-line-args-left nil))
        (`("plan" ,root) (pos-tree--print (pos-tree-plan root)))
        (`("apply" ,root ,file)
         (pos-tree--print
          (pos-tree-apply
           root
           (pos-ledger--parse
            (pos-ledger--read (if (equal file "-") "/dev/stdin" file))))))
        (`(,(or "help" "-h" "--help")) (princ pos-tree-usage))
        (_ (message "%s" pos-tree-usage)
           (kill-emacs 2)))
    (pos-tree-refused
     (message "%s: %s" (nth 1 err) (nth 2 err))
     (kill-emacs 2))
    (json-error
     (message "plan: Not a readable plan")
     (kill-emacs 2))))

(provide 'pos-tree)
;;; pos-tree.el ends here
