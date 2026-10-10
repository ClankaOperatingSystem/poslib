;;; pos-corpus.el --- The Org files of a tree, and whose each is  -*- lexical-binding: t -*-

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

;; A tree of responsibilities holds Org files, and every command that
;; reads them, the sweep, lint, dedupe, refile and the start-up report,
;; reads the same ones: the corpus.  This is the one walk that decides
;; which files those are and which scope each belongs to, from the
;; configurations alone, as doc/pos-directory.txt has it.
;;
;; A scope is the root, a responsibility, a project or a product.  A
;; responsibility is a directory whose configuration says where its
;; projects belong; a project is what lies there, a directory or a
;; single Org file.  A directory's name carries no meaning by itself.
;;
;; Membership of the tree follows declarations: a parent knows its
;; children and a child knows nothing above it.  The walk enters the
;; ordinary directories of a node and each child the node declares
;; under children, whatever the child is.  A declared child with a
;; configuration is a node, read as its configuration says.  One with
;; none is a product: its Org files are read as the product's, with
;; the default exclusions, and nothing in it is written.  A directory
;; with a configuration that no node declares is not in the tree; it
;; is a finding and is not entered, and neither is a repository that
;; no node declares.  A methodology is not entered.  A directory a
;; node's exclusions name is not entered.  A repository of the tree
;; is written only by what its own configuration allows.  A symbolic
;; link is neither read nor written.  A configuration that is refused
;; is a finding, and nothing beneath it is read.
;;
;; The rules are separate from the disk: `pos-corpus--walk' asks a
;; function for what is at each path, so the tests give it a listing
;; and the commands give it `pos-corpus--list', which looks.
;;
;; - `pos-corpus': walk a root once and return its corpus.
;; - `pos-corpus-files', `pos-corpus-owner', `pos-corpus-scopes':
;;   the files, the scope each belongs to, and the scopes.
;; - `pos-corpus-methodologies': the methodologies the projects declare.
;; - `pos-corpus-writable-p': whether a command may write a file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pos-ledger)
(require 'pos-tree)

(cl-defstruct (pos-scope (:constructor pos-scope--make) (:copier nil))
  "A scope of the tree: the root, a responsibility, a project or a product.
KIND is the symbol root, responsibility, project or product, or nil
for a node whose configuration says neither where its projects belong
nor its methodologies, which is yet to be configured.  PATH is relative
to the root, \".\" for the root itself; for a one-file project it is
the file's path without its extension.  DIR is the directory the scope
is, absolute, as a directory name; for a one-file project, the
directory holding the file.  CONFIG is the scope's configuration as
`pos-tree-read-config' gives it, or nil for a project known by where
it lies.  NODE is the nearest scope at or above this one that has a
configuration, or nil for the root."
  kind path dir config node)

(cl-defstruct (pos-corpus (:constructor pos-corpus--make) (:copier nil))
  "What a walk of a tree found.
ROOT is the root, a directory name.  SCOPES are its scopes, the root
first, then in the order found.  ENTRIES are (FILE . SCOPE), each Org
file read with the scope it belongs to, sorted by file.  FINDINGS are
(PATH . DETAIL) for what was found and not read: a configuration that
was refused, and a directory with a configuration that no node
declares.  UNWRITABLE are the paths, relative to the root, of the
repositories and the products within the tree, beneath which a
command may not write."
  root scopes entries findings unwritable)

;;;; The rules

(defconst pos-corpus--org-regexp "\\`[^.#].*\\.org\\'"
  "Matches the name of an Org file that is read.
Not a hidden file, nor a lock file, which begins with .#.")

(defun pos-corpus--org-file-p (name)
  "Return non-nil if NAME names an Org file read into the corpus."
  (string-match-p pos-corpus--org-regexp name))

(defun pos-corpus--kind (config)
  "Return the kind of scope a node with CONFIG is: responsibility, project or nil."
  (pcase (alist-get 'kind config)
    ("responsibility" 'responsibility)
    ("project" 'project)))

(defun pos-corpus--projects-dir (scope)
  "Return the path, relative to the root, of SCOPE's projects directory, or nil.
Nil for a scope whose configuration names none."
  (let ((projects (alist-get 'projects (pos-scope-config scope))))
    (when (stringp projects)
      (if (equal (pos-scope-path scope) ".")
          projects
        (concat (pos-scope-path scope) "/" projects)))))

;;;; The walk

(defun pos-corpus--methodology-paths (scope)
  "Return the paths of the methodologies SCOPE's node declares.
Each is relative to the root.  The node is the nearest scope at or
above SCOPE with a configuration."
  (when-let* ((node (if (pos-scope-config scope) scope (pos-scope-node scope)))
              (config (pos-scope-config node)))
    (mapcar (lambda (methodology)
              (if (equal (pos-scope-path node) ".")
                  (cdr methodology)
                (concat (pos-scope-path node) "/" (cdr methodology))))
            (pos-tree-methodologies config))))

(defun pos-corpus--walk (root lister)
  "Walk the tree at ROOT and return its corpus, asking LISTER what is where.
LISTER is called with a path relative to ROOT, \"\" for ROOT, and
returns a plist: :kind, one of dir, file, link or nil for nothing;
for a directory, :repository, non-nil for a repository's root,
:config, its configuration file's text, nil for none or the symbol
two-configurations, and :names, the names in it, sorted."
  (let ((files nil) (findings nil) (scopes nil) (unwritable nil))
    (cl-labels
        ((read-config (path text)
           ;; A refused configuration is a finding; the walk returns nil
           ;; and the caller does not enter the directory.
           (condition-case err
               (if (eq text 'two-configurations)
                   (pos-tree--refuse 'two-configurations "The node has two configurations")
                 (pos-tree-read-config text))
             (pos-tree-refused
              (push (cons (if (equal path "") "." path)
                          (format "config-refused: %s: %s" (nth 1 err) (nth 2 err)))
                    findings)
              nil)))
         (nearest-node (scope)
           (if (pos-scope-config scope) scope (pos-scope-node scope)))
         (make-scope (kind path config node)
           (let ((scope (pos-scope--make
                         :kind kind :path path
                         :dir (file-name-as-directory
                               (if (equal path ".") root (expand-file-name path root)))
                         :config config :node node)))
             (push scope scopes)
             scope))
         (declared-by (config path)
           ;; The paths, relative to ROOT, of the children CONFIG, the
           ;; configuration of the node at PATH, declares.
           (mapcar (lambda (child)
                     (let ((at (alist-get 'path child)))
                       (if (member path '("" ".")) at (concat path "/" at))))
                   (append (alist-get 'children config) nil)))
         (descend (dir scope exclusions exclusions-node projects-of declared)
           ;; DIR is relative to ROOT, "" for it.  SCOPE owns what lies
           ;; here.  EXCLUSIONS are those in force, declared by the node
           ;; at EXCLUSIONS-NODE, a prefix with its slash or "".
           ;; PROJECTS-OF is the scope whose projects directory DIR is,
           ;; or nil.  DECLARED are the paths of the children the node
           ;; in force declares; within a product nothing is declared.
           (dolist (name (plist-get (funcall lister dir) :names))
             (let* ((path (if (equal dir "") name (concat dir "/" name)))
                    (info (funcall lister path)))
               (pcase (plist-get info :kind)
                 ('file
                  (when (pos-corpus--org-file-p name)
                    (push (cons (expand-file-name path root)
                                (if projects-of
                                    (make-scope 'project (file-name-sans-extension path)
                                                nil (nearest-node projects-of))
                                  scope))
                          files)))
                 ('dir
                  (let ((text (plist-get info :config)))
                    (cond
                     ((pos-tree-unwalked-p (string-remove-prefix exclusions-node path)
                                           exclusions))
                     ;; A methodology's files are not the project's canon.
                     ((member path (pos-corpus--methodology-paths scope))
                      (when (plist-get info :repository) (push path unwritable)))
                     ;; A product says nothing of what is in it: a
                     ;; repository within it is not entered, and any
                     ;; other directory is the product's.
                     ((eq 'product (pos-scope-kind scope))
                      (unless (plist-get info :repository)
                        (descend path scope exclusions exclusions-node nil nil)))
                     ;; A declared child with a configuration is a node.
                     ((and text (member path declared))
                      (when (plist-get info :repository) (push path unwritable))
                      (when-let* ((config (read-config path text)))
                        (let* ((node (make-scope (pos-corpus--kind config) path config
                                                 (nearest-node scope)))
                               (own (alist-get 'exclude config))
                               (declares (not (eq own :null))))
                          (descend path node
                                   (if declares (pos-tree-exclusions config) exclusions)
                                   (if declares (concat path "/") exclusions-node)
                                   nil (declared-by config path)))))
                     ;; One with none is a product, read under the
                     ;; default exclusions and never written.
                     ((member path declared)
                      (push path unwritable)
                      (descend path (make-scope 'product path nil (nearest-node scope))
                               (pos-tree-exclusions nil) (concat path "/") nil nil))
                     ;; A configuration that no node declares is not in
                     ;; the tree.
                     (text
                      (when (plist-get info :repository) (push path unwritable))
                      (push (cons path "undeclared: a configuration no node declares")
                            findings))
                     ;; Nor is a repository that no node declares.
                     ((plist-get info :repository)
                      (push path unwritable))
                     (projects-of
                      (descend path (make-scope 'project path nil (nearest-node projects-of))
                               exclusions exclusions-node nil declared))
                     (t
                      (descend path scope exclusions exclusions-node
                               (and (equal path (pos-corpus--projects-dir scope))
                                    scope)
                               declared))))))))))
      (let* ((info (funcall lister ""))
             (text (plist-get info :config))
             (config (and text (read-config "" text)))
             (root-scope (make-scope 'root "." config nil)))
        (when (or config (not text))
          (descend "" root-scope (pos-tree-exclusions config) "" nil
                   (declared-by config "")))
        (pos-corpus--make
         :root (file-name-as-directory root)
         :scopes (nreverse scopes)
         :entries (sort files (lambda (a b) (string< (car a) (car b))))
         :findings (nreverse findings)
         :unwritable (nreverse unwritable))))))

(defun pos-corpus--list (root)
  "Return a lister over the disk for the tree at ROOT.
As `pos-corpus--walk' asks: for a path, what is there."
  (lambda (path)
    (let* ((full (if (equal path "") root (expand-file-name path root)))
           (attributes (file-attributes full)))
      (cond
       ((null attributes) nil)
       ((stringp (file-attribute-type attributes)) (list :kind 'link))
       ((eq t (file-attribute-type attributes))
        (list :kind 'dir
              :repository (file-exists-p (expand-file-name ".git" full))
              ;; Two configurations have no text to read: the walk is
              ;; told, and refuses the node.
              :config (condition-case nil
                          (when-let* ((file (pos-tree-config-file full)))
                            (with-temp-buffer
                              (insert-file-contents (expand-file-name file full))
                              (buffer-string)))
                        (pos-tree-refused 'two-configurations))
              :names (sort (directory-files full nil directory-files-no-dot-files-regexp)
                           #'string<)))
       (t (list :kind 'file))))))

;;;; What a corpus answers

(defun pos-corpus (root)
  "Walk the tree at ROOT once and return its corpus.
ROOT is a directory.  The walk reads each configuration beneath it,
enters what they allow and lists every Org file, with its scope."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (pos-corpus--walk root (pos-corpus--list root))))

(defun pos-corpus-files (corpus)
  "Return the Org files of CORPUS, absolute, sorted."
  (mapcar #'car (pos-corpus-entries corpus)))

(defun pos-corpus-owner (corpus file)
  "Return the scope of CORPUS that FILE belongs to, or nil if it is not read."
  (cdr (assoc (expand-file-name file) (pos-corpus-entries corpus))))

(defun pos-corpus-scopes-of-kind (corpus kind)
  "Return the scopes of CORPUS of KIND.
KIND is root, responsibility, project or product."
  (seq-filter (lambda (scope) (eq (pos-scope-kind scope) kind))
              (pos-corpus-scopes corpus)))

(defun pos-corpus-methodologies (corpus)
  "Return the methodologies CORPUS's projects declare, each (SCOPE NAME . DIR).
DIR is the methodology's directory, absolute, as a directory name,
whether or not it is there."
  (mapcan (lambda (scope)
            (when-let* ((config (pos-scope-config scope)))
              (mapcar (lambda (methodology)
                        (cons scope
                              (cons (car methodology)
                                    (file-name-as-directory
                                     (expand-file-name (cdr methodology)
                                                       (pos-scope-dir scope))))))
                      (pos-tree-methodologies config))))
          (pos-corpus-scopes corpus)))

(defun pos-corpus-writable-p (corpus file)
  "Return non-nil if a command may write FILE, a file of CORPUS.
A file of the root's repository may be written.  One within another
repository of the tree is read, and left to that repository's own
configuration to govern; a symbolic link is never reached."
  (let ((relative (file-relative-name (expand-file-name file) (pos-corpus-root corpus))))
    (and (pos-corpus-owner corpus file)
         (not (seq-some (lambda (repository)
                          (string-prefix-p (concat repository "/") relative))
                        (pos-corpus-unwritable corpus))))))

;; `pos-corpus-writable-p' asks of a file the corpus reads; a command
;; that writes beside a scope's files, as a seal does, asks of the scope.
(defun pos-corpus-scope-writable-p (corpus scope)
  "Return non-nil if a command may write within SCOPE, a scope of CORPUS.
A scope at or beneath another repository of the tree is that
repository's to write."
  (let ((path (pos-scope-path scope)))
    (not (seq-some (lambda (repository)
                     (or (equal repository path)
                         (string-prefix-p (concat repository "/") path)))
                   (pos-corpus-unwritable corpus)))))

(provide 'pos-corpus)
;;; pos-corpus.el ends here
