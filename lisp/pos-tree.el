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
;; A reader warns of a key it does not know and reads the rest, so a
;; configuration written for a newer reader still works with this one,
;; with the newer key's default.  Which directories a walk does not
;; enter is the node's to declare, under exclude, and is inherited by
;; what lies beneath it until a node declares its own.
;;
;; - `pos-tree-read-config': a config.yaml's text, checked, with its
;;   warnings.
;; - `pos-tree-exclusions', `pos-tree-unwalked-p': what a walk of a
;;   node does not enter.
;; - `pos-tree-plan': the plan for the tree at a root.
;; - `pos-tree-apply': do a plan, and return the plan that remains.
;; - `pos-tree-install': do only what a plan installs and links.
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
TYPE is one of string, integer, sequence and mapping.  A string is
any text but the empty one and the two words for null; an integer is
a whole number in digits, returned as a number."
  (pcase type
    ('string (and (stringp value)
                  (not (member value '("" "null" "~")))
                  (list value)))
    ('integer (and (stringp value)
                   (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" value)
                   (list (string-to-number value))))
    ('sequence (and (vectorp value) (list value)))
    ('mapping (and (pos-tree--mapping-p value) (list value)))))

(defvar pos-tree--warnings nil
  "The warnings of the configuration being read, newest first.")

(defun pos-tree--mapping (value what keys &optional where)
  "Return the mapping VALUE as an alist of symbols, checked against KEYS.
WHAT names the mapping in a refusal.  KEYS is a list of (NAME TYPE
REQUIRED): a value not of its TYPE and a REQUIRED key that is absent
are each refused.  A key not among them is a warning, which names it
after WHERE, the mapping's place in the file, and the key is left
unread: a file written for a newer reader is read with that key's
default."
  (unless (pos-tree--mapping-p value)
    (pos-tree--refuse 'not-a-mapping "%s is not a mapping" what))
  (dolist (pair value)
    (unless (assoc (car pair) keys)
      (push (format "unknown-key: %s%s" (if where (concat where ".") "") (car pair))
            pos-tree--warnings)))
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
         (seq-map-indexed
          (lambda (entry index)
            (let-alist (pos-tree--mapping
                        entry "A child"
                        '(("path" string t) ("remote" string nil)
                          ("branch" string nil))
                        (format "children[%d]" index))
              (unless (pos-tree--path-p .path)
                (pos-tree--refuse 'bad-path "Not a child's path: %s" .path))
              ;; A branch is a repository's: a child with no remote is
              ;; a directory of this one.
              (when (and (not .remote) .branch)
                (pos-tree--refuse 'bad-value
                                  "A child with no remote has no branch: %s"
                                  .path))
              `((path . ,.path) (remote . ,(or .remote :null))
                (branch . ,(if .remote (or .branch "master") :null)))))
          entries)))
    (dolist (a children)
      (dolist (b children)
        (when (and (not (eq a b))
                   (pos-tree--within-p (alist-get 'path b) (alist-get 'path a)))
          (pos-tree--refuse 'bad-path "A child's path is another's or beneath it: %s"
                            (alist-get 'path b)))))
    children))

(defun pos-tree--check-methodologies (at children)
  "Refuse one of CHILDREN beneath AT, the methodologies path, not a methodology.
A methodology's path is AT and one more part, its name, which is not
clankos."
  (dolist (child children)
    (let ((path (alist-get 'path child)))
      (when (pos-tree--within-p path at)
        (let ((name (and (> (length path) (length at))
                         (substring path (1+ (length at))))))
          (when (or (not name) (string-match-p "/" name))
            (pos-tree--refuse 'bad-path "A methodology is one part beneath %s: %s"
                              at path))
          (when (equal name "clankos")
            (pos-tree--refuse 'bad-value "The name clankos is reserved: %s" path)))))))

(defun pos-tree-methodologies (config)
  "Return the methodologies CONFIG declares, each (NAME . PATH), by name.
A methodology is a child directly beneath the path CONFIG says its
methodologies belong at; its NAME is the last part of its PATH, which
is relative to the node.  None for a node that is no project."
  (let ((at (alist-get 'methodologies config)))
    (when (stringp at)
      (sort (delq nil
                  (mapcar (lambda (child)
                            (let ((path (alist-get 'path child)))
                              (when (pos-tree--within-p path at)
                                (cons (substring path (1+ (length at))) path))))
                          (append (alist-get 'children config) nil)))
            (lambda (a b) (string< (car a) (car b)))))))

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
         (seq-map-indexed
          (lambda (entry index)
            (let-alist (pos-tree--mapping
                        entry "A worktree"
                        '(("path" string t) ("of" string nil)
                          ("remote" string nil) ("branch" string nil))
                        (format "worktrees[%d]" index))
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
         (seq-map-indexed
          (lambda (entry index)
            (let-alist (pos-tree--mapping
                        entry "An archive"
                        '(("scope" string t) ("kept" string t)
                          ("ledger" string nil) ("url" string nil)
                          ("sweep" string nil) ("path" string nil))
                        (format "archives[%d]" index))
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
              (when (and .sweep (not (member .sweep '("weekly" "sealed"))))
                (pos-tree--refuse 'bad-value "Done items are swept weekly or sealed, not %s"
                                  .sweep))
              (when (and .path (not (equal .sweep "weekly")))
                (pos-tree--refuse 'bad-value "Only a weekly sweep has a path: %s" .scope))
              (let ((ledger (and .ledger `((ledger . ,.ledger))))
                    (sweep (append (and .sweep `((sweep . ,.sweep)))
                                   (and .path `((path . ,(pos-tree--location .path "path")))))))
                (cond
                 ((not (equal .kept "remote"))
                  (when .url
                    (pos-tree--refuse 'bad-value "Only a remote archive has a url: %s"
                                      .scope))
                  `((scope . ,.scope) (kept . ,.kept) ,@ledger ,@sweep))
                 (.url `((scope . ,.scope) (kept . ,.kept) ,@ledger (url . ,.url) ,@sweep))
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

(defconst pos-tree-default-exclude '("archives" "attic" "node_modules" "_*" ".*")
  "The directories a walk does not enter when no node declares exclude.
Archives and attics, Node's modules, and names beginning with an
underscore or a dot, as doc/pos-directory.txt has it.")

(defun pos-tree--exclusions (value)
  "Return VALUE, a configuration's exclude sequence, checked, as a vector.
Each entry is a directory's name or a glob over one, with * for any
text, or a path beneath the node when it holds a slash; a final slash
is dropped.  Nil for nil."
  (when value
    (vconcat
     (seq-map (lambda (entry)
                (unless (pos-tree--typed entry 'string)
                  (pos-tree--refuse 'wrong-type "exclude: an entry is not a string"))
                (if (string-match-p "/" entry)
                    (pos-tree--location entry "exclude")
                  entry))
              value))))

(defun pos-tree-exclusions (config)
  "Return the exclusions in force at the node whose configuration is CONFIG.
A list: its exclude entries, or `pos-tree-default-exclude' when it
declares none, or when CONFIG is nil."
  (let ((declared (alist-get 'exclude config)))
    (if (and declared (not (eq declared :null)))
        (append declared nil)
      pos-tree-default-exclude)))

(defun pos-tree-unwalked-p (path exclusions)
  "Return non-nil if PATH is a directory EXCLUSIONS keep a walk out of.
PATH is relative to the node whose EXCLUSIONS these are, a list as
`pos-tree-exclusions' gives.  An entry with a slash names a path and
excludes it and what lies beneath it; any other names a directory,
with * for any text, wherever it lies beneath the node."
  (let ((name (file-name-nondirectory path)))
    (seq-some (lambda (entry)
                (if (string-match-p "/" entry)
                    (pos-tree--within-p path entry)
                  (string-match-p (wildcard-to-regexp entry) name)))
              exclusions)))

(defun pos-tree-read-config (text)
  "Return the configuration in TEXT, a config.yaml's, checked.
An alist of kind, projects, methodologies, image, bin, exclude,
children, worktrees, archives, server and warnings, with defaults
filled in: sequences are vectors and an absent value is :null.
warnings is a vector of strings, one for each key this reader does
not know, which it leaves unread.  kind is
\"responsibility\" for a node that says where its projects belong,
\"project\" for one that says where its methodologies belong, and :null
for one that says neither, which is yet to be configured.  Every
scalar is read as the text written, and the keys' types decide what
it is.  Signal `pos-tree-refused', with the refusal's kind and a
message, for a file doc/pos-directory.txt does not allow."
  (let ((pos-tree--warnings nil)
        (parsed (condition-case nil
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
                   ("bin" string nil) ("exclude" sequence nil) ("children" sequence nil)
                   ("worktrees" sequence nil) ("archives" sequence nil)
                   ("server" mapping nil))))
           (projects (pos-tree--location (alist-get 'projects top) "projects"))
           (methodologies (pos-tree--location (alist-get 'methodologies top)
                                              "methodologies"))
           (image (alist-get 'image top))
           (bin (pos-tree--location (alist-get 'bin top) "bin"))
           (children (pos-tree--children (alist-get 'children top)))
           (server (assq 'server top)))
      (when (and projects methodologies)
        (pos-tree--refuse 'bad-value
                          "A node says where its projects belong or its methodologies, not both"))
      (when methodologies
        (pos-tree--check-methodologies methodologies children))
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
        (bin . ,(or bin :null))
        (exclude . ,(or (pos-tree--exclusions (alist-get 'exclude top)) :null))
        (children . ,(vconcat children))
        (worktrees . ,(vconcat (pos-tree--worktrees (alist-get 'worktrees top)
                                                    children)))
        (archives . ,(vconcat (pos-tree--archives (alist-get 'archives top)
                                                  children)))
        (server . ,(if server
                       (pos-tree--mapping
                        (cdr (assoc "server" parsed)) "The server"
                        '(("name" string t) ("mcp" string t)) "server")
                     :null))
        (warnings . ,(vconcat (nreverse pos-tree--warnings)))))))

;;;; The declaration

(defconst pos-tree-methodology-file "methodology.yaml"
  "The declaration at a methodology's root, doc/pos-methodology.txt.")

(defun pos-tree-url-p (value)
  "Return non-nil if VALUE is a URL: a scheme, then ://."
  (string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*://" value))

(defun pos-tree--declared-path (value what)
  "Return VALUE, where a WHAT of a kind is, as written.
A path beneath the project, refused if it does not stay beneath it,
with a final slash allowed, which marks a directory of instances; or
a URL, for a kind kept outside the repository, which is taken as
written."
  (unless (or (pos-tree-url-p value)
              (pos-tree--path-p (if (and (> (length value) 1) (string-suffix-p "/" value))
                                    (substring value 0 -1)
                                  value)))
    (pos-tree--refuse 'bad-path "Not a path or a URL for %s: %s" what value))
  value)

(defun pos-tree--kinds (entries)
  "Return the kinds of canon in ENTRIES, a sequence, checked, defaults filled in."
  (let ((names nil))
    (seq-map-indexed
     (lambda (entry index)
       (let-alist (pos-tree--mapping
                   entry "A kind"
                   '(("kind" string t) ("at" string t) ("format" string nil)
                     ("derived" string nil) ("entrance" string nil))
                   (format "canon[%d]" index))
         (when (member .kind names)
           (pos-tree--refuse 'bad-value "A kind is declared twice: %s" .kind))
         (push .kind names)
         (unless (member .derived '(nil "true" "false"))
           (pos-tree--refuse 'bad-value "derived is true or false: %s" .kind))
         `((kind . ,.kind)
           (at . ,(pos-tree--declared-path .at "at"))
           (format . ,(or .format :null))
           (derived . ,(if (equal .derived "true") t :false))
           (entrance . ,(if .entrance
                            (pos-tree--declared-path .entrance "entrance")
                          .at)))))
     entries)))

(defun pos-tree--checks (names)
  "Return NAMES, a sequence of check names, checked."
  (seq-map (lambda (name)
             (let ((typed (pos-tree--typed name 'string)))
               (unless typed
                 (pos-tree--refuse 'wrong-type "A check is not a string"))
               (when (string-match-p "/" (car typed))
                 (pos-tree--refuse 'bad-value
                                   "A check is a name in bin/, not a path: %s"
                                   (car typed)))
               (car typed)))
           names))

(defun pos-tree-read-methodology (text)
  "Return the declaration in TEXT, a methodology.yaml's, checked.
An alist of methodology, the version; canon, a vector of kinds, each
an alist of kind, at, format, derived and entrance with defaults
filled in; checks, a vector of names; and warnings, a vector of
strings, one for each key this reader does not know.  Signal
`pos-tree-refused' for a file doc/pos-methodology.txt does not allow."
  (let ((pos-tree--warnings nil)
        (parsed (condition-case nil
                    (yaml-parse-string text :object-type 'alist
                                       :object-key-type 'string
                                       :sequence-type 'array
                                       :string-values t)
                  (error (pos-tree--refuse 'not-yaml "Not readable as YAML")))))
    (unless (pos-tree--mapping-p parsed)
      (pos-tree--refuse 'not-a-mapping "The declaration is not a mapping"))
    (let ((version (car (pos-tree--typed (cdr (assoc "methodology" parsed)) 'integer))))
      (when (and version (/= version 1))
        (pos-tree--refuse 'unknown-version "Not a version this reader knows: %s"
                          version)))
    (let ((top (pos-tree--mapping
                parsed "The declaration"
                '(("methodology" integer t) ("canon" sequence nil)
                  ("checks" sequence nil)))))
      `((methodology . 1)
        (canon . ,(vconcat (pos-tree--kinds (alist-get 'canon top))))
        (checks . ,(vconcat (pos-tree--checks (alist-get 'checks top))))
        (warnings . ,(vconcat (nreverse pos-tree--warnings)))))))

(defun pos-tree-read-methodology-file (dir)
  "Return the declaration of the methodology at DIR, or nil if it has none.
Signal `pos-tree-refused' for one this reader does not allow."
  (let ((file (expand-file-name pos-tree-methodology-file dir)))
    (when (file-regular-p file)
      (pos-tree-read-methodology
       (with-temp-buffer (insert-file-contents file) (buffer-string))))))

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

(defvar pos-tree--warnings-found nil
  "The warnings of the configurations read for the plan being made.
Each names the node, relative to the root, and the warning; newest first.")

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
    (let ((config (pos-tree-read-config text)))
      (seq-doseq (warning (alist-get 'warnings config))
        (push (format "%s: %s" (pos-tree--rel dir) warning) pos-tree--warnings-found))
      config)))

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
planned, because of a finding.  LOCALS are the directories of the
repository that have a configuration of their own, each (DIR . CONFIG)."
  dir config children held locals)

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
        (pos-tree--refuse 'archive-excludes "Damaged archive exclude block"))
      (cons (car starts) (min (length text) (car ends))))))

(defun pos-tree--archive-text (dir paths)
  "Return the old exclude text and its replacement for DIR and archive PATHS.
Preserve rules outside our block.  Quote glob characters and spaces in
anchored, directory-only Git patterns."
  (let* ((file (pos-tree--exclude-file dir))
         (old (with-temp-buffer
                (when (file-symlink-p file)
                  (pos-tree--refuse 'archive-excludes "The exclude file is a symbolic link"))
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

(defvar pos-tree--archive-paths nil
  "Uncommitted archive paths in the repository being planned.")

(defvar pos-tree--archive-refused nil
  "Whether an invalid node prevents updating this repository's archive rules.")

(defun pos-tree--archive-scopes (dir boundaries exclusions &optional prefix)
  "Find archive scopes below DIR, stopping at BOUNDARIES and configurations.
EXCLUSIONS are the node's, as `pos-tree-exclusions' gives them: a
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
           ((and (not (pos-tree-unwalked-p path exclusions))
                 (not (pos-tree--repository-p full)) (not (pos-ledger-config-files full)))
            (setq scopes (append (pos-tree--archive-scopes full boundaries exclusions
                                                           (concat path "/"))
                                 scopes)))))))
    scopes))

(defun pos-tree--plan-archives (repo base config)
  "Collect uncommitted archives for CONFIG at BASE in REPO."
  (let* ((entries (append (alist-get 'archives config) nil))
         (boundaries (mapcar (lambda (e) (alist-get 'path e))
                             (append (alist-get 'children config) (alist-get 'worktrees config) nil)))
         (scopes (sort (delete-dups
                        (append '(".") (mapcar (lambda (e) (alist-get 'scope e)) entries)
                                (pos-tree--archive-scopes
                                 base boundaries (pos-tree-exclusions config))))
                       #'string<)))
    (dolist (scope scopes)
      (unless (let ((at base) boundary)
                (dolist (part (unless (equal scope ".") (split-string scope "/")))
                  (setq at (expand-file-name part at))
                  (when (or (pos-tree--repository-p at) (pos-ledger-config-files at))
                    (setq boundary t)))
                boundary)
        (let* ((path (expand-file-name "archives" (expand-file-name scope base)))
               (within (file-relative-name path repo))
               (entry (seq-find (lambda (e) (equal (alist-get 'scope e) scope)) entries)))
          (cond
           ((pos-tree--through-link-p repo within)
            (pos-tree--find "path-taken" (pos-tree--rel path) "a symbolic link")
            (setq pos-tree--archive-refused t))
           ((equal (or (alist-get 'kept entry) "uncommitted") "uncommitted")
            (push within pos-tree--archive-paths))))))))

(defun pos-tree--archive-excludes (repo)
  "Plan the managed archive exclusions of REPO, unless a node was refused."
  (unless pos-tree--archive-refused
    (let ((paths (vconcat (sort (delete-dups pos-tree--archive-paths) #'string<))))
      (condition-case err
          (let ((texts (pos-tree--archive-text repo paths)))
            (unless (equal (car texts) (cdr texts))
              (pos-tree--act "archive-excludes" `(repository . ,(pos-tree--rel repo))
                             `(paths . ,paths))))
        (pos-tree-refused
         (pos-tree--find "config-refused" (pos-tree--rel repo)
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

(defvar pos-tree--local-exclusions nil
  "The exclusions each local node of the repository being planned declares.
An alist of its path relative to the repository and the list; a node
that declares none is absent, and inherits.")

(defun pos-tree--undeclared (repo mounted local exclusions &optional prefix node)
  "Find what is beneath the repository at REPO that no entry declares.
A repository, and a directory holding a configuration.  MOUNTED are the
declared paths of repositories, which are not looked into, and LOCAL
those of directories, which are; both relative to REPO.  EXCLUSIONS
are those in force, declared by the node at NODE, a prefix with its
final slash or nil for REPO; a local node that declares its own
replaces them beneath it.  PREFIX is the directory being looked in,
with its final slash, or nil for REPO."
  (dolist (name (directory-files (expand-file-name (or prefix "") repo) nil
                                 directory-files-no-dot-files-regexp))
    (let* ((path (concat prefix name))
           (full (expand-file-name path repo)))
      (cond
       ((member path mounted))
       ((or (file-symlink-p full) (not (file-directory-p full))))
       ((pos-tree-unwalked-p (string-remove-prefix (or node "") path) exclusions))
       ((member path local)
        (let ((own (assoc path pos-tree--local-exclusions)))
          (pos-tree--undeclared repo mounted local (if own (cdr own) exclusions)
                                (concat path "/") (if own (concat path "/") node))))
       ;; A submodule is the repository's own, tracked and declared by git.
       ((pos-tree--repository-p full)
        (unless (pos-tree--tracked-p repo path)
          (pos-tree--find "undeclared" (pos-tree--rel full))))
       ((pos-ledger-config-files full)
        (pos-tree--find "undeclared" (pos-tree--rel full) "a configuration"))
       (t (pos-tree--undeclared repo mounted local exclusions
                                (concat path "/") node))))))

(defvar pos-tree--mounted nil
  "The declared paths of repositories in the repository being planned.")

(defvar pos-tree--local-configs nil
  "The configured directories of the repository being planned, (DIR . CONFIG).")

(defvar pos-tree--local nil
  "The declared paths of directories in the repository being planned.")

(defun pos-tree--mount-child (repo base child)
  "Plan CHILD, a declared child with a remote, of the node at BASE in REPO.
Return (NODE . HELD): NODE is (ENTRY . NODE) for a child mounted as
declared, else nil; HELD is the child's directory when it is there
and not planned, because of a finding, else nil."
  (let-alist child
    (let* ((path (expand-file-name .path base))
           (within (file-relative-name path repo))
           (shown (pos-tree--rel path)))
      (push within pos-tree--mounted)
      (pos-tree--exclude repo within)
      (cond
       ((pos-tree--through-link-p repo within)
        (pos-tree--find "path-taken" shown "a symbolic link")
        nil)
       ((pos-tree--empty-p path)
        (pos-tree--act "clone" `(path . ,shown) `(remote . ,.remote)
                       `(branch . ,.branch))
        nil)
       ((not (pos-tree--repository-p path))
        (pos-tree--find "path-taken" shown "not a repository")
        nil)
       ((not (equal (pos-tree--git-line path "config" "--get" "remote.origin.url")
                    .remote))
        (pos-tree--find "other-remote" shown)
        (cons nil path))
       ((not (equal (pos-tree--git-line path "symbolic-ref" "--short" "-q" "HEAD")
                    .branch))
        (pos-tree--find "off-branch" shown (concat "declared " .branch))
        (cons nil path))
       (t (condition-case err
              (cons (cons `((path . ,within) ,@(assq-delete-all 'path (copy-alist child)))
                          (pos-tree--mounts path (pos-tree--config path .branch)))
                    nil)
            (pos-tree-refused
             (pos-tree--find "config-refused" shown
                             (format "%s: %s" (nth 1 err) (nth 2 err)))
             (cons nil path))))))))

(defun pos-tree--declare-local (repo base child)
  "Plan CHILD, a declared directory of the node at BASE in REPO.
What its own configuration declares is planned as REPO's.  Return
\(NODES . HELD) as `pos-tree--declared' does, for what lies beneath."
  (let-alist child
    (let* ((path (expand-file-name .path base))
           (within (file-relative-name path repo))
           (shown (pos-tree--rel path)))
      ;; A finding plans nothing beneath: nil.
      (cond
       ((pos-tree--through-link-p repo within)
        (pos-tree--find "path-taken" shown "a symbolic link") nil)
       ((not (file-exists-p path))
        (pos-tree--find "missing" shown) nil)
       ((not (file-directory-p path))
        (pos-tree--find "path-taken" shown "not a directory") nil)
       ((pos-tree--repository-p path)
        (push within pos-tree--mounted)
        (pos-tree--find "path-taken" shown "a repository, declared with no remote") nil)
       (t
        (push within pos-tree--local)
        (condition-case err
            (when-let* ((own (pos-tree--config path nil)))
              (push (cons path own) pos-tree--local-configs)
              (unless (eq (alist-get 'exclude own) :null)
                (push (cons within (pos-tree-exclusions own)) pos-tree--local-exclusions))
              (pos-tree--declared repo path own))
          (pos-tree-refused
           (setq pos-tree--archive-refused t)
           (pos-tree--find "config-refused" shown
                           (format "%s: %s" (nth 1 err) (nth 2 err)))
           nil)))))))

(defun pos-tree--declare-worktree (repo base worktree)
  "Plan WORKTREE, a declared working tree of the node at BASE in REPO."
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

(defun pos-tree--declared (repo base config)
  "Plan what CONFIG declares, the configuration of the node at BASE.
BASE is the repository at REPO or a directory of it.  Return
\(NODES . HELD): a node for each repository mounted as declared, as
\(ENTRY . NODE) with ENTRY's path relative to REPO, and the directories
of those that are there and are not planned."
  (let ((by-path (lambda (a b) (string< (alist-get 'path a) (alist-get 'path b))))
        nodes held)
    (when config (pos-tree--plan-archives repo base config))
    (when (eq (alist-get 'kind config) :null)
      (pos-tree--find "unconfigured" (pos-tree--rel base)))
    (dolist (child (sort (append (alist-get 'children config) nil) by-path))
      (if (stringp (alist-get 'remote child))
          (pcase-let ((`(,node . ,taken) (pos-tree--mount-child repo base child)))
            (when node (push node nodes))
            (when taken (push taken held)))
        (pcase-let ((`(,below . ,taken) (pos-tree--declare-local repo base child)))
          (setq nodes (append (reverse below) nodes)
                held (append taken held)))))
    (dolist (worktree (sort (append (alist-get 'worktrees config) nil) by-path))
      (pos-tree--declare-worktree repo base worktree))
    (cons (nreverse nodes) held)))

(defun pos-tree--mounts (dir config)
  "Plan what is mounted in the repository at DIR, whose configuration is CONFIG.
Return its node, with a node for each repository beneath it that is
mounted as declared, by this repository or by a directory of it."
  (let* ((pos-tree--mounted nil)
         (pos-tree--local nil)
         (pos-tree--local-configs nil)
         (pos-tree--local-exclusions nil)
         (pos-tree--archive-paths nil)
         (pos-tree--archive-refused nil)
         (declared (pos-tree--declared dir dir config)))
    (pos-tree--undeclared dir pos-tree--mounted pos-tree--local
                          (pos-tree-exclusions config))
    (when config (pos-tree--archive-excludes dir))
    (pos-tree--node :dir dir :config config :children (car declared)
                    :held (cdr declared) :locals (nreverse pos-tree--local-configs))))

;;;; What is installed

(defvar pos-tree--source nil
  "The directory the plan being made installs from, or nil for none.")

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
    (pos-tree--refuse 'bad-source "The source names no version: %s" source))
  (dolist (name (pos-tree--held source "skills"))
    (unless (string-prefix-p "clankos-" name)
      (pos-tree--refuse 'bad-source "A skill's name does not begin clankos-: %s"
                        name))))

(defun pos-tree--auto (dir)
  "Return the auto directory of the repository at DIR, or nil.
It is in the configuration directory at DIR, which a repository whose
working tree has no configuration lacks."
  (when-let* ((file (pos-tree-config-file dir)))
    (expand-file-name "auto" (expand-file-name (file-name-directory file) dir))))

(defun pos-tree--own-link-p (link owned)
  "Return non-nil if LINK is a symbolic link whose target is in one of OWNED.
OWNED are directories the tool's links point into: auto/, and the
methodologies of a project."
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

(defun pos-tree--taken (dir path)
  "Find PATH, in the repository at DIR, held by something not the tool's.
The detail says whether the repository tracks it and, if so, the
commit that added it."
  (pos-tree--find
   "name-taken" (pos-tree--rel (expand-file-name path dir))
   (if (pos-tree--tracked-p dir path)
       (let ((added (pos-tree--git-line dir "log" "--diff-filter=A"
                                        "--format=%h" "--" path)))
         (if (member added '(nil ""))
             "tracked, not yet committed"
           (concat "tracked, added in " (car (last (split-string added "\n"))))))
     "untracked")))

(defun pos-tree--link-in (dir within links owned)
  "Plan the tool's links in WITHIN, a directory of the repository at DIR.
LINKS is an alist (NAME . TARGETS): NAME is to be a link to the entry
of that name in TARGETS, a directory in one of OWNED, the directories
the tool's links point into.  A name held by something else is found
and left; a name LINKS gives twice is found for its second; and another
of the tool's links there is removed."
  (let* ((in (expand-file-name within dir))
         (closed (pos-tree--closed dir within))
         (names nil))
    (if closed
        (when links (pos-tree--taken dir closed))
      (pcase-dolist (`(,name . ,targets) links)
        (let* ((path (concat within "/" name))
               (link (expand-file-name name in))
               (shown (pos-tree--rel link))
               (target (file-relative-name (expand-file-name name targets) in)))
          (cond
           ((member name names)
            (pos-tree--find "name-taken" shown "named by more than one"))
           ((pos-tree--own-link-p link owned)
            (push name names)
            (pos-tree--exclude dir path)
            (unless (equal (file-symlink-p link) target)
              (pos-tree--act "unlink" `(path . ,shown))
              (pos-tree--act "link" `(path . ,shown) `(target . ,target))))
           ((or (file-symlink-p link) (file-exists-p link))
            (push name names)
            (pos-tree--taken dir path))
           (t
            (push name names)
            (pos-tree--exclude dir path)
            (pos-tree--act "link" `(path . ,shown) `(target . ,target))))))
      (when (file-directory-p in)
        (dolist (name (directory-files in nil directory-files-no-dot-files-regexp))
          (let ((link (expand-file-name name in)))
            (when (and (not (member name names)) (pos-tree--own-link-p link owned))
              (pos-tree--act "unlink" `(path . ,(pos-tree--rel link))))))))))

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

(defun pos-tree--earlier-link-p (dir name)
  "Return non-nil if NAME, in the skills at DIR, is a link an earlier tool made.
That is an untracked symbolic link whose target is a skill's directory
in another repository of the tree."
  (let* ((skills (file-name-as-directory (expand-file-name ".agents/skills" dir)))
         (to (file-symlink-p (expand-file-name name skills)))
         (target (and (stringp to) (expand-file-name to skills))))
    (and target
         (string-prefix-p pos-tree--root target)
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

(defun pos-tree--claude (dir prefix links owned)
  "Plan .claude/skills beneath PREFIX in the repository at DIR for LINKS.
PREFIX is a directory of the repository, relative and ending in a
slash, or empty for the repository itself.  LINKS and OWNED are as
`pos-tree--link-in' takes them.  Absent, .claude/skills is to be a
link to ../.agents/skills when there are skills there.  An ordinary
directory has the tool's links made in it too, and its note.  A file
or a symbolic link, there or at .claude, is left."
  (let* ((skills-path (concat prefix ".claude/skills"))
         (agents-path (concat prefix ".agents/skills"))
         (in (expand-file-name skills-path dir))
         (stray (expand-file-name (concat agents-path "/" pos-tree--note) dir))
         (ordinary (and (not (pos-tree--closed dir skills-path))
                        (file-directory-p in))))
    (cond
     ((pos-tree--closed dir skills-path))
     (ordinary
      (pos-tree--link-in dir skills-path links owned)
      (let ((path (concat skills-path "/" pos-tree--note))
            (note (expand-file-name pos-tree--note in)))
        (cond
         ((not links))
         ((pos-tree--tracked-p dir path) (pos-tree--taken dir path))
         (t (pos-tree--exclude dir path)
            (unless (and (file-regular-p note)
                         (equal (pos-tree--note-text in)
                                (with-temp-buffer (insert-file-contents note)
                                                  (buffer-string))))
              (pos-tree--act "note" `(path . ,(pos-tree--rel note))))))))
     ((or links (pos-tree--agents-entries dir agents-path))
      (pos-tree--exclude dir skills-path)
      (pos-tree--act "link" `(path . ,(pos-tree--rel in))
                     '(target . "../.agents/skills"))))
    ;; The note went with the directory's entries when they were moved.
    (when (and (not ordinary) (file-regular-p stray)
               (not (pos-tree--tracked-p
                     dir (concat agents-path "/" pos-tree--note))))
      (pos-tree--act "unlink" `(path . ,(pos-tree--rel stray))))))

(defun pos-tree--methodology-links (base config)
  "Return what the methodologies CONFIG declares at BASE supply.
A cons (SKILLS . COMMANDS), each an alist (NAME . DIRECTORY) as
`pos-tree--link-in' takes, by methodology and then by name.  A
methodology not there is left for its clone; one with a skill not named
after it is found (methodology-refused) and supplies nothing."
  (let (skills commands)
    (pcase-dolist (`(,name . ,path) (pos-tree-methodologies config))
      (let* ((dir (expand-file-name path base))
             (held (and (file-directory-p dir) (pos-tree--held dir "skills")))
             (bad (seq-find (lambda (skill)
                              (not (string-prefix-p (concat name "-") skill)))
                            held)))
        (cond
         ((not (file-directory-p dir)))
         (bad (pos-tree--find "methodology-refused" (pos-tree--rel dir)
                              (format "a skill not named %s-NAME: %s" name bad)))
         (t (dolist (skill held)
              (push (cons skill (expand-file-name "skills" dir)) skills))
            (dolist (command (pos-tree--held dir "bin"))
              (push (cons command (expand-file-name "bin" dir)) commands))))))
    (cons (nreverse skills) (nreverse commands))))

(defun pos-tree--in-directory (dir base config)
  "Plan the links to the methodologies of the project at BASE, in DIR.
BASE is a directory of the repository at DIR, with CONFIG its own."
  (when (stringp (alist-get 'methodologies config))
    (let ((prefix (file-name-as-directory (file-relative-name base dir)))
          (owned (list (expand-file-name (alist-get 'methodologies config) base)))
          (bin (alist-get 'bin config)))
      (pcase-let ((`(,skills . ,commands) (pos-tree--methodology-links base config)))
        (pos-tree--link-in dir (concat prefix ".agents/skills") skills owned)
        (when (stringp bin)
          (pos-tree--link-in dir (concat prefix bin) commands owned))
        (pos-tree--claude dir prefix skills owned)))))

(defun pos-tree--installs (node)
  "Plan what is installed in NODE and in the repositories beneath it.
A repository with a configuration has auto/ in its configuration
directory, replaced from the source when its version is another, and
links to what auto/ holds and to what its methodologies supply.  A
project that is a directory of it has links to its own methodologies.
One with no configuration, a product, has nothing planned in it but
the removal of links an earlier tool made."
  (let* ((dir (pos-tree--node-dir node))
         (config (pos-tree--node-config node))
         (auto (and config (pos-tree--auto dir))))
    (dolist (name (pos-tree--agents-entries dir))
      (when (pos-tree--earlier-link-p dir name)
        (pos-tree--act "unlink"
                       `(path . ,(pos-tree--rel
                                  (expand-file-name (concat ".agents/skills/" name)
                                                    dir))))))
    (when auto
      (let* ((version (and pos-tree--source (pos-tree--version pos-tree--source)))
             (stale (and version (not (equal version (pos-tree--version auto)))))
             (from (if stale pos-tree--source auto))
             (at (alist-get 'methodologies config))
             (owned (if (stringp at) (list auto (expand-file-name at dir)) (list auto)))
             (supplied (pos-tree--methodology-links dir config))
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
          (pos-tree--exclude dir (file-relative-name auto dir)))
        (when stale
          (pos-tree--act "install" `(path . ,(pos-tree--rel auto))
                         `(version . ,version)))
        (pos-tree--link-in dir ".agents/skills" skills owned)
        (when (stringp bin)
          (pos-tree--link-in dir bin commands owned))
        (pos-tree--claude dir "" skills owned)))
    (pcase-dolist (`(,base . ,own) (pos-tree--node-locals node))
      (pos-tree--in-directory dir base own))
    (dolist (child (pos-tree--node-children node))
      (pos-tree--installs (cdr child)))))

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
         (pos-tree--root (file-name-as-directory dir))
         (pos-tree--source (and source (expand-file-name source)))
         (pos-tree--actions nil)
         (pos-tree--findings nil)
         (pos-tree--warnings-found nil))
    (unless (pos-tree--repository-p dir)
      (pos-tree--refuse 'not-a-repository "Not a repository: %s" root))
    (when pos-tree--source (pos-tree--check-source pos-tree--source))
    (condition-case err
        (pos-tree--installs (pos-tree--mounts dir (pos-tree--config dir nil)))
      (pos-tree-refused
       (pos-tree--find "config-refused" "."
                       (format "%s: %s" (nth 1 err) (nth 2 err)))))
    `((pos . 2)
      (actions . ,(vconcat (nreverse pos-tree--actions)))
      (findings . ,(vconcat (nreverse pos-tree--findings)))
      (warnings . ,(vconcat (nreverse pos-tree--warnings-found))))))

;;;; The second step

(defun pos-tree--run (dir &rest args)
  "Run git in DIR with ARGS, never prompting; refuse if git fails."
  (let* ((process-environment (cons "GIT_TERMINAL_PROMPT=0" process-environment))
         (result (apply #'pos-tree--git dir args)))
    (unless (eq (car result) 0)
      (pos-tree--refuse 'failed "git %s: %s" (string-join args " ") (cdr result)))))

(defun pos-tree--do (root action &optional source)
  "Do ACTION, of a plan for the tree at ROOT, a directory name.
SOURCE is the directory an install copies."
  (let-alist action
    (pcase .do
      ("install"
       (let ((auto (expand-file-name .path root)))
         (unless source
           (pos-tree--refuse 'failed "No source to install from: %s" .path))
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
         (condition-case err
             (make-symbolic-link .target link)
           (file-error
            (pos-tree--refuse 'failed "Cannot make a symbolic link at %s: %s"
                              .path (error-message-string err))))))
      ("unlink"
       (let ((link (expand-file-name .path root)))
         (unless (or (file-symlink-p link)
                     (equal (file-name-nondirectory link) pos-tree--note))
           (pos-tree--refuse 'failed "Not a link or a note: %s" .path))
         (delete-file link)))
      (_ (pos-tree--refuse 'failed "Not an action this tool does: %s" .do)))))

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
    (unless (equal (pos-ledger-json fresh) (pos-ledger-json plan))
      (pos-tree--refuse 'stale-plan "The tree no longer gives this plan"))
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

;;;; Command line

(defconst pos-tree-usage
  "Usage: COMMAND ...  (help prints this; Emacs itself takes --help)

  plan ROOT [SOURCE]
      print what needs to be done for the tree at ROOT to be as its
      configurations declare, as JSON; change nothing.  SOURCE is the
      directory of skills and commands to install; with none, nothing
      is installed
  apply ROOT PLAN [SOURCE]
      do what the plan in the file PLAN holds, or - for standard input,
      if the tree at ROOT still gives it with SOURCE; print the plan
      that remains
  archives ROOT
      update only the managed archive rules in Git's info/exclude;
      print the remaining plan; do not clone repositories, install or
      link
  install ROOT SOURCE
      install from SOURCE and bring the links to it up to date; print
      the remaining plan; do not clone repositories or change archive
      rules

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
        ((or `("plan" ,root) `("plan" ,root ,source))
         (pos-tree--print (pos-tree-plan root source)))
        (`("archives" ,root) (pos-tree--print (pos-tree-ignore-archives root)))
        (`("install" ,root ,source)
         (pos-tree--print (pos-tree-install root source)))
        ((or `("apply" ,root ,file) `("apply" ,root ,file ,source))
         (pos-tree--print
          (pos-tree-apply
           root
           (pos-ledger-parse
            (pos-ledger-read (if (equal file "-") "/dev/stdin" file)))
           source)))
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
