;;; pos-tree-view.el --- The tree of responsibilities in buffers -*- lexical-binding: t; -*-

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

;; The tree under a root shown in two buffers, for a person at the
;; keyboard, from the same data a web viewer of the tree reads,
;; pos-tree-data.el.
;;
;; *pos-tree* is the tree of responsibilities, each beneath the one
;; that holds it, then the start-up views and the repositories.
;; *pos-tree-page* is one scope's page: the scopes above it; its
;; status, outcome and next review; the responsibilities, projects and
;; products beneath it; its items in each start-up view; its
;; configuration, installed skills and findings; its Org files; and its
;; repository.  Or it is the list of repositories.
;;
;; Each scope, item, file, view and repository shown leads somewhere:
;; a scope to its page, an item to its heading, an Org file to the
;; file, a view to `pos-views-show' and a repository to the list of
;; repositories.  An Org file's own links are Org's to follow.
;;
;; The tree is read once, and both buffers show what was read.
;;
;; In either buffer:
;;
;;   RET   follow what is at point
;;   ^     show the page of the scope above this page's
;;   g     read the tree again
;;   n, p  move to the next and the previous thing that leads somewhere
;;   q     bury the buffer
;;
;; - `pos-tree-view': show the tree of responsibilities.
;; - `pos-tree-view-scope': show a scope's page.
;; - `pos-tree-view-repositories': show the list of repositories.
;; - `pos-tree-view-follow': follow what is at point.
;; - `pos-tree-view-up': show the page of the scope above.
;; - `pos-tree-view-next', `pos-tree-view-previous': move between the
;;   things that lead somewhere.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-tree-data)
(require 'pos-views)

(defface pos-tree-view-heading '((t :inherit bold))
  "Face of a heading in the tree's buffers."
  :group 'pos)

(defface pos-tree-view-quiet '((t :inherit shadow))
  "Face of what is said of a thing, beside it, in the tree's buffers."
  :group 'pos)

(defconst pos-tree-view-menu-kinds '("root" "responsibility")
  "The kinds of scope the tree of responsibilities shows.")

(defconst pos-tree-view-kind-order '("root" "responsibility" "project" "product")
  "The order of the kinds of scope, where scopes are listed together.")

(defconst pos-tree-view-view-titles
  '(("handover" . "Latest handovers") ("next" . "Next actions")
    ("waiting" . "Waiting") ("scheduled" . "Scheduled")
    ("deadlines" . "Deadlines") ("reviews" . "Reviews due")
    ("reviews-to-schedule" . "Reviews to schedule")
    ("stuck" . "Stuck projects") ("no-outcome" . "Projects with no outcome")
    ("projects" . "Active projects") ("intray" . "Intray")
    ("someday" . "Someday") ("finished" . "Finished this week")
    ("all" . "All open items"))
  "Each start-up view's heading, in the order the buffers list them.")

(defvar pos-tree-view--documents nil
  "The tree data read, as (ROOT . DOCUMENT), ROOT a directory name.")

(defvar-local pos-tree-view--root nil
  "The root whose tree this buffer shows.")

(defvar-local pos-tree-view--page nil
  "What this buffer shows: tree, repositories, or (scope . PATH).")

;;;; The data

(defun pos-tree-view--value (value)
  "Return VALUE, or nil for :null."
  (unless (eq value :null) value))

(defun pos-tree-view--list (value)
  "Return VALUE, a vector, a list or :null, as a list."
  (append (pos-tree-view--value value) nil))

(defun pos-tree-view--get (object key)
  "Return KEY of the alist OBJECT, nil for :null."
  (pos-tree-view--value (alist-get key object)))

(defun pos-tree-view--document (root &optional again)
  "Return the tree data of ROOT, read now if AGAIN or never read."
  (or (and (not again) (cdr (assoc root pos-tree-view--documents)))
      (let ((document (progn (message "Reading the tree under %s..." root)
                             (save-current-buffer (pos-tree-data root)))))
        (message "Reading the tree under %s...done" root)
        (setf (alist-get root pos-tree-view--documents nil nil #'equal) document)
        document)))

(defun pos-tree-view--scopes (document)
  "Return DOCUMENT's scopes, as a list."
  (pos-tree-view--list (alist-get 'scopes document)))

(defun pos-tree-view--scope (document path)
  "Return the scope of DOCUMENT at PATH, or nil."
  (seq-find (lambda (scope) (equal (alist-get 'path scope) path))
            (pos-tree-view--scopes document)))

(defun pos-tree-view--name (scope)
  "Return the name of SCOPE: its title, else the last part of its path."
  (or (pos-tree-view--get scope 'title)
      (let ((path (alist-get 'path scope)))
        (if (equal path ".") "Root" (file-name-nondirectory path)))))

(defun pos-tree-view--children (document scope)
  "Return the scopes of DOCUMENT directly beneath SCOPE, by kind, then name."
  (let ((paths (pos-tree-view--list (alist-get 'children scope))))
    (sort (seq-filter (lambda (child) (member (alist-get 'path child) paths))
                      (pos-tree-view--scopes document))
          (lambda (a b)
            (let ((ka (seq-position pos-tree-view-kind-order (alist-get 'kind a)))
                  (kb (seq-position pos-tree-view-kind-order (alist-get 'kind b))))
              (if (equal ka kb)
                  (string< (downcase (pos-tree-view--name a))
                           (downcase (pos-tree-view--name b)))
                (< (or ka 9) (or kb 9))))))))

(defun pos-tree-view--ancestors (document scope)
  "Return the scopes of DOCUMENT above SCOPE, the root first."
  (let (chain (parent (pos-tree-view--get scope 'parent)))
    (while-let ((above (and parent (pos-tree-view--scope document parent))))
      (push above chain)
      (setq parent (pos-tree-view--get above 'parent)))
    chain))

(defun pos-tree-view--views (document)
  "Return the names of DOCUMENT's views, in the buffers' order."
  (let ((names (mapcar (lambda (view) (symbol-name (car view)))
                       (alist-get 'views document))))
    (append (seq-filter (lambda (name) (member name names))
                        (mapcar #'car pos-tree-view-view-titles))
            (seq-remove (lambda (name) (assoc name pos-tree-view-view-titles))
                        names))))

;;;; Inserting

(defun pos-tree-view--insert-target (text target &optional face)
  "Insert TEXT, which leads to TARGET when followed, in FACE.
TARGET is (scope . PATH), (file . PATH), (item FILE . LINE),
\(view . NAME) or (repositories)."
  (insert (propertize text 'pos-tree-view-target target
                      'face (or face 'link)
                      'mouse-face 'highlight
                      'help-echo "RET: follow")))

(defun pos-tree-view--insert-heading (text)
  "Insert TEXT as a heading, after a blank line unless at the start."
  (unless (bobp) (insert "\n"))
  (insert (propertize text 'face 'pos-tree-view-heading) "\n"))

(defun pos-tree-view--insert-quiet (text)
  "Insert TEXT, what is said of a thing, beside it."
  (insert (propertize text 'face 'pos-tree-view-quiet)))

(defun pos-tree-view--insert-facts (pairs)
  "Insert each of PAIRS, (LABEL . TEXT), whose TEXT is not nil, one a line.
TEXT is a string, or a function that inserts it."
  (let* ((shown (seq-filter #'cdr pairs))
         (width (apply #'max 0 (mapcar (lambda (pair) (length (car pair))) shown))))
    (dolist (pair shown)
      (insert "  " (string-pad (car pair) (+ 2 width)))
      (if (functionp (cdr pair)) (funcall (cdr pair)) (insert (cdr pair)))
      (insert "\n"))))

(defun pos-tree-view--insert-scope-line (scope)
  "Insert a line that leads to SCOPE's page, with its kind beside it."
  (insert "  ")
  (pos-tree-view--insert-target (pos-tree-view--name scope)
                                (cons 'scope (alist-get 'path scope)))
  (pos-tree-view--insert-quiet (concat "  " (alist-get 'kind scope)))
  (insert "\n"))

(defun pos-tree-view--insert-project (project)
  "Insert a line that leads to PROJECT's page, with its status, review and outcome."
  (insert "  ")
  (pos-tree-view--insert-target (pos-tree-view--name project)
                                (cons 'scope (alist-get 'path project)))
  (pos-tree-view--insert-quiet
   (concat (string-join (delq nil (list "" (pos-tree-view--get project 'status)
                                        (pos-tree-view--get project 'review)))
                        "  ")
           (if-let* ((outcome (pos-tree-view--get project 'outcome)))
               (concat "  " outcome)
             "")))
  (insert "\n"))

(defun pos-tree-view--insert-item (document item)
  "Insert a line of ITEM, as a start-up view gives it, that leads to it.
An item of a heading leads to its heading; a scope's row, named as
DOCUMENT names the scope, to the scope."
  (insert "  ")
  (if-let* ((file (pos-tree-view--get item 'file)))
      (let ((dates (delq nil (list (when-let* ((d (pos-tree-view--get item 'scheduled)))
                                     (concat "scheduled " d))
                                   (when-let* ((d (pos-tree-view--get item 'deadline)))
                                     (concat "due " d))
                                   (when-let* ((d (pos-tree-view--get item 'closed)))
                                     (concat "closed " d))))))
        (when-let* ((state (pos-tree-view--get item 'state)))
          (insert (propertize state 'face 'org-todo) " "))
        (pos-tree-view--insert-target (or (pos-tree-view--get item 'title) file)
                                      (cons 'item (cons file (alist-get 'line item))))
        (when dates (pos-tree-view--insert-quiet (concat "  " (string-join dates ", ")))))
    (let* ((path (alist-get 'scope item))
           (scope (pos-tree-view--scope document path)))
      (pos-tree-view--insert-target (if scope (pos-tree-view--name scope) path)
                                    (cons 'scope path))
      (pos-tree-view--insert-quiet
       (string-join (delq nil (list "" (pos-tree-view--get item 'status)
                                    (pos-tree-view--get item 'review)
                                    (pos-tree-view--get item 'next)
                                    (pos-tree-view--get item 'outcome)))
                    "  "))))
  (insert "\n"))

(defun pos-tree-view--insert-configuration (document scope)
  "Insert SCOPE's configuration, image, installed skills and findings.
DOCUMENT is the tree data."
  (let* ((config (pos-tree-view--get scope 'configuration))
         (content (and config (pos-tree-view--get config 'content)))
         (image (pos-tree-view--get scope 'image))
         (from (pos-tree-view--get scope 'image_from))
         (findings (pos-tree-view--list (alist-get 'findings scope))))
    (when (or config image findings)
      (pos-tree-view--insert-heading "Configuration")
      (pos-tree-view--insert-facts
       (list (cons "File" (and config (pos-tree-view--get config 'file)))
             (cons "Image" (and image
                                (lambda ()
                                  (insert image)
                                  (unless (equal from (alist-get 'path scope))
                                    (insert " from ")
                                    (pos-tree-view--insert-target
                                     (pos-tree-view--name
                                      (or (pos-tree-view--scope document from)
                                          `((path . ,from))))
                                     (cons 'scope from))))))
             (cons "Projects" (and content (pos-tree-view--get content 'projects))))))
    (when config
      (let ((sources (pos-tree-view--get content 'methodologies)))
        (cond ((stringp sources)
               (insert "  Sources of skills beneath " sources "\n"))
              ((setq sources (pos-tree-view--list sources))
               (insert "  Sources of skills\n")
               (dolist (source sources)
                 (insert "    " (or (pos-tree-view--get source 'name) "") "  "
                         (or (pos-tree-view--get source 'source) ""))
                 (pos-tree-view--insert-quiet
                  (concat "  " (or (pos-tree-view--get source 'ref) "")
                          "  " (or (pos-tree-view--get source 'kind) "")
                          "  " (if-let* ((skills (pos-tree-view--list
                                                  (alist-get 'skills source))))
                                   (string-join skills ", ")
                                 "all")))
                 (insert "\n")))))
      (let ((skills (pos-tree-view--list (alist-get 'installed_skills scope))))
        (insert "  Installed skills  " (if skills (string-join skills ", ") "none") "\n")))
    (when findings
      (insert "  Findings\n")
      (dolist (finding findings)
        (insert "    " (propertize (or (pos-tree-view--get finding 'finding) "")
                                   'face 'warning)
                "  " (or (pos-tree-view--get finding 'path) ""))
        (pos-tree-view--insert-quiet
         (string-join (delq nil (list "" (pos-tree-view--get finding 'skill)
                                      (pos-tree-view--get finding 'detail)))
                      "  "))
        (insert "\n")))))

;;;; The buffers

(defun pos-tree-view--insert-tree (document)
  "Insert the tree of responsibilities of DOCUMENT, the views and repositories."
  (insert (propertize "Tree of responsibilities" 'face 'pos-tree-view-heading) "  ")
  (pos-tree-view--insert-quiet (abbreviate-file-name pos-tree-view--root))
  (insert "\n")
  (when-let* ((made (pos-tree-view--get document 'made)))
    (pos-tree-view--insert-quiet (concat "Read at " made "\n")))
  (insert "\n")
  (cl-labels ((branch (scope depth)
                (insert (make-string (* 2 depth) ?\s))
                (pos-tree-view--insert-target (pos-tree-view--name scope)
                                              (cons 'scope (alist-get 'path scope)))
                (insert "\n")
                (dolist (child (pos-tree-view--children document scope))
                  (when (member (alist-get 'kind child) pos-tree-view-menu-kinds)
                    (branch child (1+ depth))))))
    (when-let* ((root (pos-tree-view--scope document ".")))
      (branch root 0)))
  (pos-tree-view--insert-heading "Views")
  (dolist (name (pos-tree-view--views document))
    (insert "  ")
    (pos-tree-view--insert-target
     (or (cdr (assoc name pos-tree-view-view-titles)) name) (cons 'view name))
    (insert "\n"))
  (pos-tree-view--insert-heading "Configuration")
  (insert "  ")
  (pos-tree-view--insert-target "Repositories" '(repositories))
  (insert "\n"))

(defun pos-tree-view--insert-scope-page (document path)
  "Insert the page of DOCUMENT's scope at PATH."
  (let ((scope (or (pos-tree-view--scope document path)
                   (user-error "The tree has no scope at %s" path))))
    (let ((ancestors (pos-tree-view--ancestors document scope)))
      (dolist (above ancestors)
        (pos-tree-view--insert-target (pos-tree-view--name above)
                                      (cons 'scope (alist-get 'path above)))
        (insert " / "))
      (when ancestors (insert "\n\n")))
    (insert (propertize (pos-tree-view--name scope) 'face 'pos-tree-view-heading))
    (pos-tree-view--insert-quiet (concat "  " (alist-get 'kind scope) "  " path))
    (insert "\n\n")
    (pos-tree-view--insert-facts
     (list (cons "Status" (pos-tree-view--get scope 'status))
           (cons "Outcome" (pos-tree-view--get scope 'outcome))
           (cons "Next review" (pos-tree-view--get scope 'review))
           (cons "Main file"
                 (when-let* ((file (pos-tree-view--get scope 'file)))
                   (lambda () (pos-tree-view--insert-target file (cons 'file file)))))
           (cons "ID" (pos-tree-view--get scope 'id))))
    (let ((children (pos-tree-view--children document scope)))
      (pcase-dolist (`(,heading . ,kinds)
                     '(("Responsibilities" "root" "responsibility")
                       ("Projects" "project") ("Products" "product")))
        (when-let* ((shown (seq-filter (lambda (child)
                                         (member (alist-get 'kind child) kinds))
                                       children)))
          (pos-tree-view--insert-heading heading)
          (dolist (child shown)
            (if (equal heading "Projects")
                (pos-tree-view--insert-project child)
              (pos-tree-view--insert-scope-line child))))))
    (dolist (name (pos-tree-view--views document))
      (unless (member name '("all" "projects"))
        (when-let* ((items (seq-filter
                            (lambda (item) (equal (alist-get 'scope item) path))
                            (pos-tree-view--list
                             (alist-get (intern name) (alist-get 'views document))))))
          (pos-tree-view--insert-heading
           (or (cdr (assoc name pos-tree-view-view-titles)) name))
          (dolist (item items) (pos-tree-view--insert-item document item)))))
    (pos-tree-view--insert-configuration document scope)
    (when-let* ((files (pos-tree-view--list (alist-get 'files scope))))
      (pos-tree-view--insert-heading "Org files")
      (dolist (file files)
        (insert "  ")
        (pos-tree-view--insert-target file (cons 'file file))
        (insert "\n")))
    (when-let* ((path (pos-tree-view--get scope 'repository))
                (repository (seq-find (lambda (r) (equal (alist-get 'path r) path))
                                      (pos-tree-view--list
                                       (alist-get 'repositories document)))))
      (pos-tree-view--insert-heading "Repository")
      (pos-tree-view--insert-facts
       (list (cons "Path" (lambda ()
                            (pos-tree-view--insert-target path '(repositories))))
             (cons "Remote" (pos-tree-view--get repository 'remote))
             (cons "Branch" (pos-tree-view--get repository 'branch)))))))

(defun pos-tree-view--insert-repositories (document)
  "Insert the list of DOCUMENT's repositories."
  (insert (propertize "Repositories" 'face 'pos-tree-view-heading) "\n\n")
  (dolist (repository (pos-tree-view--list (alist-get 'repositories document)))
    (let ((path (alist-get 'path repository))
          (installed (pos-tree-view--get repository 'installed)))
      (insert "  ")
      (pos-tree-view--insert-target path (cons 'scope path))
      (pos-tree-view--insert-quiet
       (string-join
        (delq nil (list ""
                        (pos-tree-view--get repository 'remote)
                        (pos-tree-view--get repository 'branch)
                        (when-let* ((version (and installed
                                                  (pos-tree-view--get installed 'version))))
                          (concat "ClankOS " version))))
        "  "))
      (insert "\n"))))

(defvar-keymap pos-tree-view-mode-map
  :doc "Keymap for `pos-tree-view-mode'."
  :parent special-mode-map
  "RET" #'pos-tree-view-follow
  "^" #'pos-tree-view-up
  "n" #'pos-tree-view-next
  "p" #'pos-tree-view-previous)

(define-derived-mode pos-tree-view-mode special-mode "POS-Tree"
  "Major mode for the buffers of a tree of responsibilities."
  (setq-local revert-buffer-function #'pos-tree-view--revert)
  (setq truncate-lines t))

(defun pos-tree-view--render ()
  "Fill the current buffer with its page, from the tree data read."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (document (pos-tree-view--document pos-tree-view--root)))
    (erase-buffer)
    (pcase pos-tree-view--page
      ('tree (pos-tree-view--insert-tree document))
      ('repositories (pos-tree-view--insert-repositories document))
      (`(scope . ,path) (pos-tree-view--insert-scope-page document path)))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun pos-tree-view--revert (&rest _)
  "Read the tree again, and show it in each of the tree's buffers.
The `revert-buffer-function' of the mode."
  (pos-tree-view--document pos-tree-view--root t)
  (let ((root pos-tree-view--root))
    (dolist (name '("*pos-tree*" "*pos-tree-page*"))
      (when-let* ((buffer (get-buffer name)))
        (with-current-buffer buffer
          (when (equal pos-tree-view--root root)
            (pos-tree-view--render)))))))

(defun pos-tree-view--show (name root page)
  "Show PAGE of ROOT in the buffer NAME; return the buffer.
PAGE is a value of `pos-tree-view--page'."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pos-tree-view-mode) (pos-tree-view-mode))
      (setq pos-tree-view--root root
            pos-tree-view--page page
            default-directory root)
      (let ((inhibit-read-only t)) (erase-buffer))
      (pos-tree-view--render)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun pos-tree-view--root-of (root)
  "Return ROOT, else this buffer's root, else `pos-directory', as a directory name."
  (file-name-as-directory
   (expand-file-name (or root pos-tree-view--root pos-directory))))

;;;###autoload
(defun pos-tree-view (&optional root)
  "Show the tree of responsibilities under ROOT.
ROOT defaults to the root of the tree's buffers, else `pos-directory'."
  (interactive)
  (pos-tree-view--show "*pos-tree*" (pos-tree-view--root-of root) 'tree))

(defun pos-tree-view-scope (path &optional root)
  "Show the page of the scope at PATH, relative to ROOT; \".\" is the root.
ROOT defaults to the root of the tree's buffers, else `pos-directory'."
  (interactive
   (let ((root (pos-tree-view--root-of nil)))
     (list (completing-read "Scope: "
                            (mapcar (lambda (scope) (alist-get 'path scope))
                                    (pos-tree-view--scopes
                                     (pos-tree-view--document root)))
                            nil t)
           root)))
  (pos-tree-view--show "*pos-tree-page*" (pos-tree-view--root-of root)
                       (cons 'scope path)))

(defun pos-tree-view-repositories (&optional root)
  "Show the repositories of the tree under ROOT.
ROOT defaults to the root of the tree's buffers, else `pos-directory'."
  (interactive)
  (pos-tree-view--show "*pos-tree-page*" (pos-tree-view--root-of root)
                       'repositories))

(defun pos-tree-view-follow ()
  "Follow what is at point: show a scope's page, visit a file or an item.
A view is shown by `pos-views-show'."
  (interactive)
  (let ((target (or (get-text-property (point) 'pos-tree-view-target)
                    (user-error "Nothing here leads anywhere")))
        (root pos-tree-view--root))
    (pcase target
      (`(scope . ,path) (pos-tree-view-scope path root))
      (`(file . ,file) (find-file-other-window (expand-file-name file root)))
      (`(item ,file . ,line)
       (find-file-other-window (expand-file-name file root))
       (widen)
       (goto-char (point-min))
       (forward-line (1- line))
       (when (derived-mode-p 'org-mode) (org-fold-show-context)))
      (`(view . ,name) (pos-views-show name root))
      ('(repositories) (pos-tree-view-repositories root)))))

(defun pos-tree-view-up ()
  "Show the page of the scope above this page's; from the tree, nothing."
  (interactive)
  (pcase pos-tree-view--page
    (`(scope . ,path)
     (let ((parent (pos-tree-view--get
                    (pos-tree-view--scope (pos-tree-view--document pos-tree-view--root)
                                          path)
                    'parent)))
       (if parent
           (pos-tree-view-scope parent pos-tree-view--root)
         (pos-tree-view pos-tree-view--root))))
    (_ (pos-tree-view pos-tree-view--root))))

(defun pos-tree-view-next ()
  "Move to the next thing that leads somewhere."
  (interactive)
  (let ((at (point)))
    (when (get-text-property at 'pos-tree-view-target)
      (setq at (next-single-property-change at 'pos-tree-view-target)))
    (when (and at (not (get-text-property at 'pos-tree-view-target)))
      (setq at (next-single-property-change at 'pos-tree-view-target)))
    (when at (goto-char at))))

(defun pos-tree-view-previous ()
  "Move to the previous thing that leads somewhere."
  (interactive)
  (let ((at (point)))
    ;; From the start of the thing at point, each step back is to the
    ;; start of the stretch before, which leads somewhere or does not.
    (when (and (> at (point-min))
               (get-text-property at 'pos-tree-view-target)
               (eq (get-text-property (1- at) 'pos-tree-view-target)
                   (get-text-property at 'pos-tree-view-target)))
      (setq at (previous-single-property-change at 'pos-tree-view-target nil (point-min))))
    (while (and at (> at (point-min))
                (setq at (previous-single-property-change at 'pos-tree-view-target nil (point-min)))
                (not (get-text-property at 'pos-tree-view-target))))
    (when (and at (get-text-property at 'pos-tree-view-target))
      (goto-char at))))

(provide 'pos-tree-view)
;;; pos-tree-view.el ends here
