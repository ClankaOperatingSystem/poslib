;;; pos-tree.el --- A tree of repositories, as .pos declares it -*- lexical-binding: t; -*-

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
;; beneath it, as doc/pos-directory.txt specifies.  This is the first of
;; the two steps that bring a tree to what its .pos files declare: it
;; makes the plan, changes nothing and uses no network.  The second
;; step, which does what a plan holds, is not written.
;;
;; A plan holds the children and worktrees to clone, the paths to
;; exclude and the skill links to make and remove, with what it found
;; and will not act on.  The agent files of section 5 are not planned:
;; a server is read and checked, and nothing is done with it.
;;
;; - `pos-tree-read-config': a config.yaml's text, checked.
;; - `pos-tree-plan': the plan for the tree at a root.
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

(defconst pos-tree-config-file ".pos/config.yaml"
  "A repository's configuration, relative to its root.")

(defconst pos-tree--scope-regexp
  (let ((pair "\\(?:projects\\|responsibilities\\)/[^/]+"))
    (concat pair "\\(?:/" pair "\\)*"))
  "Matches a scope's path: pairs of projects/NAME or responsibilities/NAME.")

(defun pos-tree--scope-path-p (path)
  "Return non-nil if PATH is a scope's path beneath a repository's root."
  (and (string-match-p (concat "\\`" pos-tree--scope-regexp "\\'") path)
       (not (seq-some (lambda (part) (member part '("." "..")))
                      (split-string path "/")))))

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
                        '(("path" string t) ("remote" string t)
                          ("branch" string nil) ("skills-up" boolean nil)))
              (unless (pos-tree--scope-path-p .path)
                (pos-tree--refuse 'bad-path "Not a child's path: %s" .path))
              `((path . ,.path) (remote . ,.remote)
                (branch . ,(or .branch "master"))
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
              (unless (and (string-match
                            (concat "\\`\\(?:\\(" pos-tree--scope-regexp
                                    "\\)/\\)?_worktrees/\\([^/]+\\)\\'")
                            .path)
                           (not (member (match-string 2 .path) '("." ".."))))
                (pos-tree--refuse 'bad-path "Not a worktree's path: %s" .path))
              (when-let* ((scope (match-string 1 .path)))
                (unless (pos-tree--scope-path-p scope)
                  (pos-tree--refuse 'bad-path "Not a worktree's path: %s" .path))
                (pos-tree--own-scope scope children "A worktree"))
              (unless (eq (null .of) (not (null .remote)))
                (pos-tree--refuse 'bad-value "A worktree has one of of and remote: %s"
                                  .path))
              (cond
               (.remote `((path . ,.path) (remote . ,.remote)
                          (branch . ,(or .branch "master"))))
               ((not (seq-some (lambda (child) (equal (alist-get 'path child) .of))
                               children))
                (pos-tree--refuse 'bad-value "A worktree is of no declared child: %s"
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
                        '(("scope" string t) ("kept" string t) ("url" string nil)))
              (unless (equal .scope ".")
                (unless (pos-tree--scope-path-p .scope)
                  (pos-tree--refuse 'bad-path "Not a scope's path: %s" .scope))
                (pos-tree--own-scope .scope children "An archive"))
              (unless (member .kept '("committed" "uncommitted" "remote"))
                (pos-tree--refuse 'bad-value "An archive is not kept %s" .kept))
              (cond
               ((not (equal .kept "remote"))
                (when .url
                  (pos-tree--refuse 'bad-value "Only a remote archive has a url: %s"
                                    .scope))
                `((scope . ,.scope) (kept . ,.kept)))
               (.url `((scope . ,.scope) (kept . ,.kept) (url . ,.url)))
               (t (pos-tree--refuse 'missing-key "A remote archive lacks url")))))
          entries)))
    (pos-tree--distinct (mapcar (lambda (a) (alist-get 'scope a)) archives)
                        "An archive's scope")
    archives))

(defun pos-tree-read-config (text)
  "Return the configuration in TEXT, a config.yaml's, checked.
An alist of children, worktrees, archives and server, with defaults
filled in: sequences are vectors, an absent server is :null and false
is :false.  Every scalar is read as the text written, and the keys'
types decide what it is.  Signal `pos-tree-refused', with the
refusal's kind and a message, for a file doc/pos-directory.txt does
not allow."
  (let ((parsed (condition-case nil
                    (yaml-parse-string text :object-type 'alist
                                       :object-key-type 'string
                                       :sequence-type 'array
                                       :string-values t)
                  (error (pos-tree--refuse 'not-yaml "Not readable as YAML")))))
    (unless (pos-tree--mapping-p parsed)
      (pos-tree--refuse 'not-a-mapping "The file is not a mapping"))
    (let ((version (car (pos-tree--typed (cdr (assoc "pos" parsed)) 'integer))))
      (when (and version (/= version 1))
        (pos-tree--refuse 'unknown-version "Not a version this reader knows: %s"
                          version)))
    (let* ((top (pos-tree--mapping
                 parsed "The file"
                 '(("pos" integer t) ("children" sequence nil)
                   ("worktrees" sequence nil) ("archives" sequence nil)
                   ("server" mapping nil))))
           (children (pos-tree--children (alist-get 'children top)))
           (server (assq 'server top)))
      `((children . ,(vconcat children))
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

(defun pos-tree--excluded-p (dir path)
  "Return non-nil if the repository at DIR excludes PATH.
That is, if its info/exclude holds the line this tool writes for PATH."
  (when-let* ((file (pos-tree--git-line dir "rev-parse" "--git-path" "info/exclude")))
    (setq file (expand-file-name file dir))
    (and (file-readable-p file)
         (member (concat "/" path)
                 (split-string (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string))
                               "\n")))))

(defun pos-tree--config (dir branch)
  "Return the configuration of the repository at DIR, or nil if it has none.
With BRANCH, read it from that branch as committed; with nil, from the
working tree.  Signal `pos-tree-refused' for a file that is refused."
  (when-let* ((text (if branch
                       (pos-tree--git-line
                        dir "show" (concat branch ":" pos-tree-config-file))
                     (let ((file (expand-file-name pos-tree-config-file dir)))
                       (and (file-readable-p file)
                            (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string)))))))
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

(defun pos-tree--undeclared (dir declared &optional prefix)
  "Find the repositories beneath DIR that are not among DECLARED paths.
PREFIX is the scope being looked in, with its final slash, or nil for
DIR itself.  A scope that is not a repository is looked into."
  (dolist (kind '("projects" "responsibilities"))
    (let ((base (expand-file-name (concat prefix kind) dir)))
      (when (and (file-directory-p base) (not (file-symlink-p base)))
        (dolist (name (directory-files base nil directory-files-no-dot-files-regexp))
          (let* ((path (concat prefix kind "/" name))
                 (full (expand-file-name path dir)))
            (cond
             ((member path declared))
             ((or (file-symlink-p full) (not (file-directory-p full))))
             ((pos-tree--repository-p full)
              (pos-tree--find "undeclared" (pos-tree--rel full)))
             (t (pos-tree--undeclared dir declared (concat path "/"))))))))))

(defun pos-tree--mounts (dir config)
  "Plan what is mounted in the repository at DIR, whose configuration is CONFIG.
Return its node, with a node for each child that is mounted as declared."
  (let ((children (sort (append (alist-get 'children config) nil)
                        (lambda (a b) (string< (alist-get 'path a) (alist-get 'path b)))))
        (worktrees (sort (append (alist-get 'worktrees config) nil)
                         (lambda (a b) (string< (alist-get 'path a) (alist-get 'path b)))))
        nodes held)
    (dolist (child children)
      (let-alist child
        (let* ((path (expand-file-name .path dir))
               (shown (pos-tree--rel path)))
          (pos-tree--exclude dir .path)
          (cond
           ((pos-tree--through-link-p dir .path)
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
                  (push (cons child (pos-tree--mounts
                                     path (pos-tree--config path .branch)))
                        nodes)
                (pos-tree-refused
                 (push path held)
                 (pos-tree--find "config-refused" shown
                                 (format "%s: %s" (nth 1 err) (nth 2 err))))))))))
    (dolist (worktree worktrees)
      (let-alist worktree
        (let* ((path (expand-file-name .path dir))
               (shown (pos-tree--rel path)))
          (pos-tree--exclude dir .path)
          (cond
           ((pos-tree--through-link-p dir .path)
            (pos-tree--find "path-taken" shown "a symbolic link"))
           ((pos-tree--empty-p path)
            (pos-tree--act "clone" `(path . ,shown)
                           (if .of
                               `(of . ,(pos-tree--rel (expand-file-name .of dir)))
                             `(remote . ,.remote))
                           `(branch . ,.branch)))
           ((not (pos-tree--repository-p path))
            (pos-tree--find "path-taken" shown "not a repository"))))))
    (pos-tree--undeclared dir (mapcar (lambda (child) (alist-get 'path child))
                                      children))
    (pos-tree--node :dir dir :config config :children (nreverse nodes)
                    :held held)))

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
    `((pos . 1)
      (actions . ,(vconcat (nreverse pos-tree--actions)))
      (findings . ,(vconcat (nreverse pos-tree--findings))))))

;;;; Command line

(defconst pos-tree-usage
  "Usage: COMMAND ...  (help prints this; Emacs itself takes --help)

  plan ROOT
      print what needs to be done for the tree at ROOT to be as its
      .pos files declare, as JSON; change nothing

Exit 0 nothing to do, 1 something to do or to report, 2 refused.
"
  "The command line's usage.")

(defun pos-tree-batch ()
  "Run a command from `command-line-args-left', as in `pos-tree-usage'."
  (condition-case err
      (pcase (prog1 command-line-args-left (setq command-line-args-left nil))
        (`("plan" ,root)
         (let ((plan (pos-tree-plan root)))
           (princ (decode-coding-string (pos-ledger-json plan) 'utf-8))
           (kill-emacs (if (and (seq-empty-p (alist-get 'actions plan))
                                (seq-empty-p (alist-get 'findings plan)))
                           0
                         1))))
        (`(,(or "help" "-h" "--help")) (princ pos-tree-usage))
        (_ (message "%s" pos-tree-usage)
           (kill-emacs 2)))
    (pos-tree-refused
     (message "%s: %s" (nth 1 err) (nth 2 err))
     (kill-emacs 2))))

(provide 'pos-tree)
;;; pos-tree.el ends here
