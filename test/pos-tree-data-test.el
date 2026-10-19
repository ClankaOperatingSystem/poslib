;;; pos-tree-data-test.el --- Tests for pos-tree-data -*- lexical-binding: t; -*-

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

;; The tree as data: each scope's place, main file and configuration,
;; the repositories, the IDs and the views, from one tree of cases.

;;; Code:

(require 'ert)
(require 'pos-tree-data)
(require 'pos-test-support)

(defun pos-tree-data-test-day (days)
  "Return the date DAYS from today, as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (time-add nil (days-to-time days))))

(defmacro pos-tree-data-test-with-tree (&rest body)
  "Evaluate BODY with `root' holding the tree of cases.
The root is a repository with an image and a source of skills it has
no skills-lock.json for.  It has a one-file project, alpha, under
review; a responsibility, home, with a project of its own, roof; a
product, widget, on a branch of its own; a child repository with a
configuration, kit; and a configuration no node declares, stray."
  (declare (indent 0))
  `(pos-test-with-files root
       `((".git/HEAD" . ,pos-test-git-head)
         (".pos/config.yaml"
          . ,(pos-test-lines
              "pos: 2"
              "image: example.org/image:1"
              "projects: projects/"
              "methodologies:"
              "  - source: https://example.org/team/gnome.git"
              "    ref: v1"
              "children:"
              "  - path: home"
              "  - path: widget"
              "    remote: https://example.org/widget.git"
              "    branch: main"
              "  - path: kit"
              "    remote: git@example.org:kit.git"))
         ("index.org"
          . ,(pos-test-lines
              ":PROPERTIES:" ":ID:       root-id" ":END:" "#+TITLE: Root" ""
              "* Tasks" "** NEXT Water the plants" ":PROPERTIES:"
              ":ID:       plants-id" ":END:"))
         ("projects/alpha.org"
          . ,(pos-test-lines
              ":PROPERTIES:" ":ID:       alpha-id" ":STATUS:   WIP"
              ":OUTCOME:  The roof is fixed." ":END:" "#+TITLE: Alpha" ""
              "* TODO Review Alpha :review:"
              (format "SCHEDULED: <%s>" (pos-tree-data-test-day 2))))
         ("home/.clanka/config.yml" . ,pos-test-child-config)
         ("home/index.org" . "#+TITLE: Home\n* TODO Tidy the hall\n")
         ("home/projects/roof/project.org" . "#+TITLE: Roof\n* NEXT Call the roofer\n")
         ("widget/.git/HEAD" . ,pos-test-git-head)
         ("widget/notes.org" . "* TODO Oil the widget\n")
         ("kit/.git/HEAD" . ,pos-test-git-head)
         ("kit/.pos/config.yaml" . "pos: 2\n")
         ("kit/index.org" . "#+TITLE: Kit\n")
         ("stray/.pos/config.yaml" . "pos: 2\n")
         ("stray/notes.org" . "* NEXT Not read\n"))
     ,@body))

(defun pos-tree-data-test-scope (data path)
  "Return the scope of DATA whose path is PATH."
  (seq-find (lambda (scope) (equal path (alist-get 'path scope)))
            (alist-get 'scopes data)))

(defun pos-tree-data-test-repository (data path)
  "Return the repository of DATA whose path is PATH."
  (seq-find (lambda (repository) (equal path (alist-get 'path repository)))
            (alist-get 'repositories data)))

;;;; The tree

(ert-deftest pos-tree-data/each-scope-has-its-kind-parent-and-children ()
  "The scopes are the root's first, each with the one above and those beneath.
A responsibility's project lies beneath it, and a configuration no
node declares is no scope."
  (pos-tree-data-test-with-tree
    (let ((data (pos-tree-data root '("next"))))
      (should (equal "." (alist-get 'path (aref (alist-get 'scopes data) 0))))
      (should (equal '(("." "root" :null)
                       ("home" "responsibility" ".")
                       ("home/projects/roof" "project" "home")
                       ("kit" "responsibility" ".")
                       ("projects/alpha" "project" ".")
                       ("widget" "product" "."))
                     (sort (mapcar (lambda (scope)
                                     (let-alist scope (list .path .kind .parent)))
                                   (alist-get 'scopes data))
                           (lambda (a b) (string< (car a) (car b))))))
      (should (equal '("home" "kit" "projects/alpha" "widget")
                     (sort (append (alist-get 'children
                                              (pos-tree-data-test-scope data "."))
                                   nil)
                           #'string<)))
      (should (equal ["home/projects/roof"]
                     (alist-get 'children (pos-tree-data-test-scope data "home"))))
      (should (equal [] (alist-get 'children (pos-tree-data-test-scope data "widget")))))))

(ert-deftest pos-tree-data/a-scope-has-its-main-file-title-and-id ()
  "The main file is a one-file project's own, project.org, or index.org.
The title is its #+TITLE and the id the ID before its first heading.
A product with neither file has none."
  (pos-tree-data-test-with-tree
    (let ((data (pos-tree-data root '("next"))))
      (pcase-dolist (`(,path ,file ,title ,id)
                     '(("." "index.org" "Root" "root-id")
                       ("projects/alpha" "projects/alpha.org" "Alpha" "alpha-id")
                       ("home" "home/index.org" "Home" :null)
                       ("home/projects/roof" "home/projects/roof/project.org" "Roof" :null)
                       ("widget" :null :null :null)))
        (ert-info (path)
          (let-alist (pos-tree-data-test-scope data path)
            (should (equal (list file title id) (list .file .title .id))))))
      (should (equal ["widget/notes.org"]
                     (alist-get 'files (pos-tree-data-test-scope data "widget")))))))

(ert-deftest pos-tree-data/a-project-has-its-status-outcome-and-review ()
  "A project's STATUS and OUTCOME come from its file, its review from its review.
A responsibility has no status or outcome."
  (pos-tree-data-test-with-tree
    (let ((data (pos-tree-data root '("next"))))
      (let-alist (pos-tree-data-test-scope data "projects/alpha")
        (should (equal "WIP" .status))
        (should (equal "The roof is fixed." .outcome))
        (should (equal (pos-tree-data-test-day 2) .review)))
      (let-alist (pos-tree-data-test-scope data "home")
        (should (equal '(:null :null :null) (list .status .outcome .review)))))))

;;;; Configuration and repositories

(ert-deftest pos-tree-data/a-scope-has-its-configuration-and-image ()
  "A node's configuration is given as read, with its file.
The image passes down within a repository and not into a mounted one."
  (pos-tree-data-test-with-tree
    (let ((data (pos-tree-data root '("next"))))
      (let-alist (pos-tree-data-test-scope data ".")
        (should (equal ".pos/config.yaml" .configuration.file))
        (should (equal "projects" (alist-get 'projects .configuration.content)))
        (should (equal '("example.org/image:1" . ".") (cons .image .image_from)))
        (should (equal "." .repository)))
      (let-alist (pos-tree-data-test-scope data "home")
        (should (equal "home/.clanka/config.yml" .configuration.file))
        (should (equal '("example.org/image:1" . ".") (cons .image .image_from))))
      (let-alist (pos-tree-data-test-scope data "kit")
        (should (equal "kit" .repository))
        (should (equal '(:null . :null) (cons .image .image_from))))
      (let-alist (pos-tree-data-test-scope data "widget")
        (should (equal '(:null :null) (list .configuration .installed_skills)))))))

(ert-deftest pos-tree-data/a-repository-has-the-remote-and-branch-declared ()
  "Each repository is given once, the root's first.
A declared remote with no branch rests on master."
  (pos-tree-data-test-with-tree
    (let ((data (pos-tree-data root '("next"))))
      (should (equal "." (alist-get 'path (aref (alist-get 'repositories data) 0))))
      (should (equal '("." "kit" "widget")
                     (sort (mapcar (lambda (repository) (alist-get 'path repository))
                                   (alist-get 'repositories data))
                           #'string<)))
      (pcase-dolist (`(,path ,remote ,branch)
                     '(("." :null :null)
                       ("widget" "https://example.org/widget.git" "main")
                       ("kit" "git@example.org:kit.git" "master")))
        (ert-info (path)
          (let-alist (pos-tree-data-test-repository data path)
            (should (equal (list remote branch :null)
                           (list .remote .branch .installed)))))))))

(ert-deftest pos-tree-data/a-scope-has-its-findings ()
  "What the walk did not read is the finding of the scope it lies in.
A declared source with no skills-lock.json differs from the lock."
  (pos-tree-data-test-with-tree
    (let* ((data (pos-tree-data root '("next")))
           (findings (append (alist-get 'findings (pos-tree-data-test-scope data "."))
                             nil)))
      (should (equal "stray" (alist-get 'path (seq-find
                                               (lambda (finding)
                                                 (equal "not-read"
                                                        (alist-get 'finding finding)))
                                               findings))))
      (should (equal "skills-lock.json"
                     (alist-get 'path (seq-find
                                       (lambda (finding)
                                         (equal "lock-differs"
                                                (alist-get 'finding finding)))
                                       findings))))
      (should (equal [] (alist-get 'findings (pos-tree-data-test-scope data "home")))))))

;;;; IDs and views

(ert-deftest pos-tree-data/each-id-has-its-file-line-and-scope ()
  "An ID before the first heading is at line 1; another, at its heading's."
  (pos-tree-data-test-with-tree
    (let ((ids (alist-get 'ids (pos-tree-data root '("next")))))
      (should (equal '((file . "index.org") (line . 1) (scope . "."))
                     (alist-get 'root-id ids)))
      (should (equal '((file . "index.org") (line . 7) (scope . "."))
                     (alist-get 'plants-id ids)))
      (should (equal "projects/alpha" (alist-get 'scope (alist-get 'alpha-id ids)))))))

(ert-deftest pos-tree-data/the-views-are-those-named-or-every-one ()
  "The views are those named; with none named, every view."
  (pos-tree-data-test-with-tree
    (should (equal '(next stuck)
                   (mapcar #'car (alist-get 'views (pos-tree-data root '("next" "stuck"))))))
    (should (equal (mapcar #'intern pos-startup-views)
                   (mapcar #'car (alist-get 'views (pos-tree-data root)))))))

;;;; The command

(ert-deftest pos-tree-data/the-command-prints-the-data-as-json ()
  "The output is one line of JSON: the document of the views named."
  (pos-tree-data-test-with-tree
    (let* ((pos-directory root)
           (command-line-args-left (list "--view" "next"))
           (output (with-output-to-string (pos-tree-data-batch)))
           (json (json-parse-string output)))
      (should (string-suffix-p "}\n" output))
      (should (equal 1 (cl-count ?\n output)))
      (should (equal pos-tree-data-version (gethash "version" json)))
      (should (equal '("next") (hash-table-keys (gethash "views" json))))
      (should (equal 6 (length (gethash "scopes" json))))
      (should-not command-line-args-left))))

(provide 'pos-tree-data-test)
;;; pos-tree-data-test.el ends here
