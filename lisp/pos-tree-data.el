;;; pos-tree-data.el --- The tree of responsibilities as data -*- lexical-binding: t; -*-

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

;; The tree under a root as one document of data, for a viewer of the
;; tree: its scopes, each with its place in the tree, its main file,
;; its configuration and what is installed at it; its repositories;
;; where each Org ID is; and the start-up views, pos-startup.el.  The
;; tree is walked once, pos-corpus.el, for all of it.  It reads the
;; files and uses no network.
;;
;; A scope's main file is a one-file project's own file, else the
;; project.org, else the index.org, in the scope's directory, where
;; the walk reads one as the scope's.  Its title is the main file's
;; #+TITLE, and its id the ID before the file's first heading.
;;
;; - `pos-tree-data': the document, for `json-serialize'.
;; - `pos-tree-data-batch': shell entry; --view NAME, repeated, picks
;;   views; prints the document as JSON.
;; - `pos-tree-data-version': the version of the document's form.
;; - `pos-tree-data-views': the views the document gives when none is
;;   named.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-corpus)
(require 'pos-lock)
(require 'pos-node)
(require 'pos-startup)

(defconst pos-tree-data-version 1
  "The version of the document's form.
It changes when the meaning of a field does.")

(defconst pos-tree-data-views pos-startup-views
  "The views the document gives when none is named: every one.")

;;;; Paths

(defvar pos-tree-data--root nil
  "The root whose document is being made.")

(defun pos-tree-data--relative (root file)
  "Return FILE relative to ROOT, \".\" for ROOT itself."
  (directory-file-name (file-relative-name file root)))

(defun pos-tree-data--within-p (path at)
  "Return non-nil if PATH, relative to the root, lies beneath AT.
AT is a scope's path; every path but the root's lies beneath \".\"."
  (and (not (equal path at))
       (or (equal at ".") (string-prefix-p (concat at "/") path))))

(defun pos-tree-data--enclosing (path scopes)
  "Return the scope of SCOPES nearest above PATH, or nil.
That is the one whose path is the longest that PATH lies beneath."
  (let (best)
    (dolist (scope scopes)
      (let ((at (pos-scope-path scope)))
        (when (and (pos-tree-data--within-p path at)
                   (or (null best)
                       (equal (pos-scope-path best) ".")
                       (> (length at) (length (pos-scope-path best)))))
          (setq best scope))))
    best))

(defun pos-tree-data--repository (root dir)
  "Return the directory of the repository holding DIR, within ROOT.
The nearest directory at or above DIR that holds .git, else ROOT."
  (let ((at (file-name-as-directory dir)))
    (while (and (not (equal at root))
                (string-prefix-p root at)
                (not (file-exists-p (expand-file-name ".git" at))))
      (setq at (file-name-directory (directory-file-name at))))
    (if (string-prefix-p root at) at root)))

;;;; A scope's files

(defun pos-tree-data--main-file (root scope files)
  "Return SCOPE's main file under ROOT, absolute, or nil.
FILES are the scope's Org files, absolute.  A one-file project's own
file, else project.org, else index.org, in the scope's directory."
  (seq-find (lambda (file) (member file files))
            (list (expand-file-name (concat (pos-scope-path scope) ".org") root)
                  (expand-file-name "project.org" (pos-scope-dir scope))
                  (expand-file-name "index.org" (pos-scope-dir scope)))))

(defun pos-tree-data--file-facts (file)
  "Return (TITLE ID STATUS OUTCOME) of FILE, each a string or :null.
TITLE is its #+TITLE; the others are properties before its first
heading."
  (with-current-buffer (pos-visit file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t)
           (property (lambda (name)
                       (or (and (not (org-at-heading-p))
                                (org-entry-get nil name))
                           :null))))
       (list (save-excursion
               (if (re-search-forward "^#\\+TITLE:[ \t]*\\(.*?\\)[ \t]*$" nil t)
                   (match-string-no-properties 1)
                 :null))
             (funcall property "ID")
             (funcall property "STATUS")
             (funcall property "OUTCOME"))))))

(defun pos-tree-data--ids (root corpus)
  "Return where each Org ID in CORPUS's files is, for `json-serialize'.
An alist of each ID, as a symbol, with its file relative to ROOT, the
line of its heading, 1 for an ID before the first heading, and its
scope.  An ID written twice is given where it is first found."
  (let ((seen (make-hash-table :test #'equal))
        ids)
    (pcase-dolist (`(,file . ,scope) (pos-corpus-entries corpus))
      (with-current-buffer (pos-visit file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((case-fold-search t))
           (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$" nil t)
             (let ((id (match-string-no-properties 1)))
               (unless (gethash id seen)
                 (puthash id t seen)
                 (push `(,(intern id)
                         (file . ,(pos-tree-data--relative root file))
                         (line . ,(save-excursion
                                    (if (org-before-first-heading-p)
                                        1
                                      (org-back-to-heading t)
                                      (line-number-at-pos))))
                         (scope . ,(pos-scope-path scope)))
                       ids))))))))
    (sort ids (lambda (a b) (string< (car a) (car b))))))

;;;; Configuration and what is installed

(defun pos-tree-data--image (scope parents repository)
  "Return (IMAGE . FROM), the image SCOPE's commands run in, or nil.
FROM is the path of the scope whose configuration names IMAGE: SCOPE
or the nearest above it in REPOSITORY, a directory, that names one.
PARENTS maps each scope's path to the scope directly above it."
  (let ((at scope) found)
    (while (and at (not found))
      (let ((image (alist-get 'image (pos-scope-config at))))
        (if (stringp image)
            (setq found (cons image (pos-scope-path at)))
          (let ((above (gethash (pos-scope-path at) parents)))
            (setq at (and above
                          (equal repository (pos-tree-data--repository-of above))
                          above))))))
    found))

(defun pos-tree-data--repository-of (scope)
  "Return the directory of the repository holding SCOPE."
  (pos-tree-data--repository pos-tree-data--root (pos-scope-dir scope)))

(defun pos-tree-data--image-source (dir)
  "Return the image's skills at DIR as a source, or nil.
They are what auto/ holds at a repository's root, as `pos-lock-image'
gives them."
  (when-let* ((auto (pos-lock-auto dir)))
    (and (file-directory-p auto) (pos-lock-image auto dir))))

(defun pos-tree-data--first-line (file)
  "Return the first line of FILE, or :null if there is no such file."
  (if (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-substring-no-properties
                      (point-min) (line-end-position))))
    :null))

(defun pos-tree-data--installed (dir)
  "Return the contents of auto/ at the repository DIR, or :null.
An alist of version and commit, as the source's files name them."
  (let ((auto (pos-lock-auto dir)))
    (if (and auto (file-directory-p auto))
        `((version . ,(pos-tree-data--first-line (expand-file-name "version" auto)))
          (commit . ,(pos-tree-data--first-line (expand-file-name "commit" auto))))
      :null)))

(defun pos-tree-data--declared (scopes dir)
  "Return the children entry of SCOPES that declares DIR, or nil.
DIR is a directory name."
  (seq-some (lambda (scope)
              (seq-find (lambda (child)
                          (let ((path (alist-get 'path child)))
                            (and (stringp path)
                                 (equal dir (file-name-as-directory
                                             (expand-file-name path (pos-scope-dir scope)))))))
                        (alist-get 'children (pos-scope-config scope))))
            (seq-filter #'pos-scope-config scopes)))

;;;; The document

(defun pos-tree-data--findings (root scope corpus-findings)
  "Return SCOPE's findings, for `json-serialize'.
CORPUS-FINDINGS are (PATH . DETAIL), what the walk did not read that
lies most nearly beneath SCOPE; each is not-read.  A scope with a
configuration adds how its skills-lock.json differs from it, as
`pos-lock-differences' finds, with the file's path relative to ROOT."
  (append
   (mapcar (lambda (finding)
             `((finding . "not-read")
               (path . ,(format "%s" (car finding)))
               (skill . :null)
               (detail . ,(format "%s" (cdr finding)))))
           corpus-findings)
   (when-let* ((config (pos-scope-config scope)))
     (let ((dir (pos-scope-dir scope)))
       (mapcar (lambda (difference)
                 (let-alist difference
                   `((finding . ,.finding)
                     (path . ,(pos-tree-data--relative
                               root (expand-file-name pos-lock-file dir)))
                     (skill . ,(or .skill :null))
                     (detail . ,(or .detail :null)))))
               (pos-lock-differences dir config (pos-tree-data--image-source dir)))))))

(defun pos-tree-data--scope (root scope context)
  "Return SCOPE under ROOT as data, for `json-serialize'.
CONTEXT is a plist of :corpus, :parents, :reviews and :findings."
  (let* ((corpus (plist-get context :corpus))
         (scopes (pos-corpus-scopes corpus))
         (path (pos-scope-path scope))
         (files (mapcar #'car (seq-filter (lambda (entry) (eq (cdr entry) scope))
                                          (pos-corpus-entries corpus))))
         (main (pos-tree-data--main-file root scope files))
         (facts (if main (pos-tree-data--file-facts main) '(:null :null :null :null)))
         (parent (gethash path (plist-get context :parents)))
         (repository (pos-tree-data--repository-of scope))
         (config (pos-scope-config scope))
         (image (pos-tree-data--image scope (plist-get context :parents) repository))
         (project (eq (pos-scope-kind scope) 'project)))
    `((path . ,path)
      (kind . ,(symbol-name (pos-scope-kind scope)))
      (title . ,(nth 0 facts))
      (file . ,(if main (pos-tree-data--relative root main) :null))
      (id . ,(nth 1 facts))
      (parent . ,(if parent (pos-scope-path parent) :null))
      (children . ,(vconcat
                    (mapcar #'pos-scope-path
                            (seq-filter (lambda (other)
                                          (eq scope (gethash (pos-scope-path other)
                                                             (plist-get context :parents))))
                                        scopes))))
      (repository . ,(pos-tree-data--relative root repository))
      (files . ,(vconcat (mapcar (lambda (file) (pos-tree-data--relative root file))
                                 files)))
      (status . ,(if project (nth 2 facts) :null))
      (outcome . ,(if project (nth 3 facts) :null))
      (review . ,(or (cdr (assoc path (plist-get context :reviews))) :null))
      (configuration
       . ,(if config
              `((file . ,(pos-tree-data--relative
                          root (expand-file-name (pos-node-config-file (pos-scope-dir scope))
                                                 (pos-scope-dir scope))))
                (content . ,config))
            :null))
      (image . ,(if image (car image) :null))
      (image_from . ,(if image (cdr image) :null))
      (installed_skills
       . ,(if config
              (vconcat (pos-lock-installed (pos-scope-dir scope) config
                                           (pos-tree-data--image-source
                                            (pos-scope-dir scope))))
            :null))
      (findings . ,(vconcat (pos-tree-data--findings
                             root scope
                             (alist-get scope (plist-get context :findings))))))))

(defun pos-tree-data--repositories (root scopes)
  "Return the repositories of SCOPES under ROOT as data, the root's first.
Each is an alist of path; remote and branch, from the children entry
that declares it, or :null; and installed, what its auto/ holds."
  (let (dirs)
    (dolist (scope scopes)
      (cl-pushnew (pos-tree-data--repository-of scope) dirs :test #'equal))
    (mapcar (lambda (dir)
              (let* ((child (pos-tree-data--declared scopes dir))
                     (remote (alist-get 'remote child))
                     (branch (alist-get 'branch child)))
                `((path . ,(pos-tree-data--relative root dir))
                  (remote . ,(if (stringp remote) remote :null))
                  (branch . ,(cond ((stringp branch) branch)
                                   ((stringp remote) "master")
                                   (t :null)))
                  (installed . ,(pos-tree-data--installed dir)))))
            (nreverse dirs))))

(defun pos-tree-data (root &optional views)
  "Return the tree under ROOT as data, for `json-serialize'.
VIEWS are the start-up views to give, by name; every one of
`pos-tree-data-views' when nil.  An alist:

  version       `pos-tree-data-version'
  made          when the document was made, an ISO 8601 time
  files_read    the count of Org files read
  not_read      what the walk did not read, as `pos-startup-data' has it
  scopes        each scope, the root first: path, kind, title, file, id,
                parent, children, repository, files, status, outcome,
                review, configuration, image, image_from,
                installed_skills and findings
  repositories  each repository, the root's first: path, remote,
                branch and installed
  ids           each Org ID with its file, line and scope
  views         each view's items, as `pos-startup-data' has them

What a scope or a repository lacks is :null."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (pos-tree-data--root root)
         (corpus (pos-corpus root))
         (scopes (pos-corpus-scopes corpus))
         (pos-visit-corpus corpus)
         (vc-handled-backends nil)
         (org-element-cache-persistent nil)
         (startup (pos-startup-data root (or views pos-tree-data-views) corpus))
         (parents (make-hash-table :test #'equal))
         findings)
    (dolist (scope scopes)
      (puthash (pos-scope-path scope)
               (pos-tree-data--enclosing (pos-scope-path scope) scopes)
               parents))
    (dolist (finding (pos-corpus-findings corpus))
      (let ((scope (or (pos-tree-data--enclosing (format "%s" (car finding)) scopes)
                       (car scopes))))
        (setf (alist-get scope findings) (append (alist-get scope findings)
                                                 (list finding)))))
    (let ((context (list :corpus corpus :parents parents :findings findings
                         :reviews (pos-startup-review-dates root corpus))))
      `((version . ,pos-tree-data-version)
        (made . ,(format-time-string "%FT%T%:z"))
        (files_read . ,(alist-get 'files_read startup))
        (not_read . ,(alist-get 'not_read startup))
        (scopes . ,(vconcat (mapcar (lambda (scope)
                                      (pos-tree-data--scope root scope context))
                                    scopes)))
        (repositories . ,(vconcat (pos-tree-data--repositories root scopes)))
        ;; An empty table, for no ID, is an empty object.
        (ids . ,(or (pos-tree-data--ids root corpus) (make-hash-table)))
        (views . ,(alist-get 'views startup))))))

(defun pos-tree-data-batch ()
  "Print the tree data of `pos-directory' as JSON and a newline.
Each --view NAME in `command-line-args-left' names a view to give in
place of every view.  Exit 2 on any other argument."
  (let (views)
    (while command-line-args-left
      (let ((argument (pop command-line-args-left)))
        (if (and (equal argument "--view") command-line-args-left
                 (member (car command-line-args-left) pos-startup-views))
            (push (pop command-line-args-left) views)
          (message "Usage: [--view %s] ..." (string-join pos-startup-views "|"))
          (kill-emacs 2))))
    ;; `json-serialize' gives UTF-8 bytes, which `princ' would print in
    ;; Emacs's internal form, each byte above 127 as two.
    (princ (concat (decode-coding-string
                    (json-serialize (pos-tree-data pos-directory (nreverse views)))
                    'utf-8)
                   "\n"))))

(provide 'pos-tree-data)
;;; pos-tree-data.el ends here
