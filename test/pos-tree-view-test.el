;;; pos-tree-view-test.el --- Tests for pos-tree-view -*- lexical-binding: t; -*-

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

;; The tree in buffers: what the tree and a scope's page show, where
;; each thing shown leads, and that the tree is read once.

;;; Code:

(require 'ert)
(require 'pos-tree-view)
(require 'pos-test-support)

(defmacro pos-tree-view-test-with-tree (&rest body)
  "Evaluate BODY with `root' holding a tree, and no buffer of it left after.
The root has an image and a source of skills, a one-file project,
alpha, a responsibility, home, with a project of its own, roof, and a
product, widget, in a repository of its own."
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
              "    remote: https://example.org/widget.git"))
         ("index.org" . ,(pos-test-lines "#+TITLE: Root" "" "* Tasks"
                                         "** NEXT Water the plants"))
         ("projects/alpha.org"
          . ,(pos-test-lines ":PROPERTIES:" ":STATUS:   WIP"
                             ":OUTCOME:  The outline is agreed." ":END:"
                             "#+TITLE: Alpha" "* NEXT Draft the outline"))
         ("home/.clanka/config.yml" . ,pos-test-child-config)
         ("home/index.org" . "#+TITLE: Home\n")
         ("home/projects/roof/project.org"
          . "#+TITLE: Roof\n* NEXT Call the roofer\n")
         ("widget/.git/HEAD" . ,pos-test-git-head)
         ("widget/notes.org" . "* TODO Oil the widget\n"))
     (let ((pos-tree-view--documents nil)
           (before (buffer-list)))
       (unwind-protect
           (save-window-excursion ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before) (kill-buffer buffer)))))))

(defun pos-tree-view-test-target (text)
  "Return where TEXT, first found in the current buffer, leads."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (get-text-property (match-beginning 0) 'pos-tree-view-target)))

(defun pos-tree-view-test-line (text)
  "Return the first line of the current buffer with TEXT in it, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward text nil t)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun pos-tree-view-test-follow (text)
  "Follow TEXT, first found in the current buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0))
  (pos-tree-view-follow))

(ert-deftest pos-tree-view/the-tree-is-the-responsibilities-the-views-and-repositories ()
  "The tree shows each responsibility beneath its parent, and no project or product."
  (pos-tree-view-test-with-tree
    (with-current-buffer (pos-tree-view root)
      (should (equal "Root" (pos-tree-view-test-line "Root")))
      (should (equal "  Home" (pos-tree-view-test-line "Home")))
      (should (equal '(scope . "home") (pos-tree-view-test-target "Home")))
      (should-not (pos-tree-view-test-line "Roof"))
      (should-not (pos-tree-view-test-line "Alpha"))
      (should-not (pos-tree-view-test-line "widget"))
      (should (equal '(view . "next") (pos-tree-view-test-target "Next actions")))
      (should (equal '(repositories) (pos-tree-view-test-target "Repositories"))))))

(ert-deftest pos-tree-view/a-scope-page-shows-what-lies-beneath-and-its-items ()
  "The root's page lists its responsibilities, projects, products and items."
  (pos-tree-view-test-with-tree
    (with-current-buffer (pos-tree-view-scope "." root)
      (should (equal "*pos-tree-page*" (buffer-name)))
      (should (equal '(scope . "home") (pos-tree-view-test-target "Home")))
      (should (string-match-p "Alpha  WIP  The outline is agreed\\."
                              (pos-tree-view-test-line "Alpha")))
      (should (equal '(scope . "projects/alpha") (pos-tree-view-test-target "Alpha")))
      (should (equal '(scope . "widget") (pos-tree-view-test-target "widget")))
      (should (equal "  NEXT Water the plants" (pos-tree-view-test-line "Water the plants")))
      (should (equal '(item "index.org" . 4) (pos-tree-view-test-target "Water the plants")))
      (should (string-match-p "Main file  index\\.org" (pos-tree-view-test-line "Main file")))
      (should (equal '(file . "index.org") (pos-tree-view-test-target "index.org"))))))

(ert-deftest pos-tree-view/a-scope-page-shows-its-configuration-and-repository ()
  "The root's page has its configuration, image, sources, files and repository."
  (pos-tree-view-test-with-tree
    (with-current-buffer (pos-tree-view-scope "." root)
      (should (string-match-p "File +\\.pos/config\\.yaml" (pos-tree-view-test-line ".pos/config.yaml")))
      (should (string-match-p "Image +example\\.org/image:1$" (pos-tree-view-test-line "example.org/image:1")))
      (should (string-match-p "https://example\\.org/team/gnome\\.git  v1"
                              (pos-tree-view-test-line "gnome.git")))
      (should (pos-tree-view-test-line "Installed skills"))
      (should (string-match-p "Path +\\.$" (pos-tree-view-test-line "Path")))
      (save-excursion
        (goto-char (point-min))
        (search-forward "Path")
        (end-of-line)
        (should (equal '(repositories)
                       (get-text-property (1- (point)) 'pos-tree-view-target)))))))

(ert-deftest pos-tree-view/a-project-page-leads-up-the-tree ()
  "A project's page names the scopes above it, and ^ shows the one above."
  (pos-tree-view-test-with-tree
    (with-current-buffer (pos-tree-view-scope "home/projects/roof" root)
      (should (equal "Root / Home / " (pos-tree-view-test-line "Root /")))
      (should (equal '(scope . ".") (pos-tree-view-test-target "Root")))
      (should (string-match-p "Image +example\\.org/image:1 from Root"
                              (pos-tree-view-test-line "example.org/image:1")))
      (pos-tree-view-up)
      (should (equal '(scope . "home") pos-tree-view--page))
      (pos-tree-view-up)
      (should (equal '(scope . ".") pos-tree-view--page))
      (pos-tree-view-up)
      (should (equal "*pos-tree*" (buffer-name (current-buffer)))))))

(ert-deftest pos-tree-view/following-a-scope-a-file-and-an-item ()
  "A scope leads to its page, a file to the file, an item to its heading."
  (pos-tree-view-test-with-tree
    (with-current-buffer (pos-tree-view root)
      (pos-tree-view-test-follow "Home"))
    (with-current-buffer "*pos-tree-page*"
      (should (equal '(scope . "home") pos-tree-view--page))
      (pos-tree-view-scope "." root)
      (pos-tree-view-test-follow "Water the plants")
      (should (equal (expand-file-name "index.org" root) buffer-file-name))
      (should (looking-at-p "\\*\\* NEXT Water the plants")))
    (with-current-buffer "*pos-tree-page*"
      (pos-tree-view-test-follow "index.org")
      (should (equal (expand-file-name "index.org" root) buffer-file-name)))))

(ert-deftest pos-tree-view/the-repositories ()
  "The list of repositories gives each with its remote, and leads to its scope."
  (pos-tree-view-test-with-tree
    (with-current-buffer (pos-tree-view-repositories root)
      (should (string-match-p "widget  https://example\\.org/widget\\.git  master"
                              (pos-tree-view-test-line "widget")))
      (should (equal '(scope . "widget") (pos-tree-view-test-target "widget"))))))

(ert-deftest pos-tree-view/next-and-previous-move-between-what-leads-somewhere ()
  "The keys n and p stop at each thing that leads somewhere, in order."
  (pos-tree-view-test-with-tree
    (with-current-buffer (pos-tree-view-scope "home/projects/roof" root)
      (goto-char (point-min))
      (should (equal '(scope . ".") (get-text-property (point) 'pos-tree-view-target)))
      (pos-tree-view-next)
      (should (equal '(scope . "home") (get-text-property (point) 'pos-tree-view-target)))
      (pos-tree-view-next)
      (should (equal '(file . "home/projects/roof/project.org")
                     (get-text-property (point) 'pos-tree-view-target)))
      (pos-tree-view-previous)
      (should (equal '(scope . "home") (get-text-property (point) 'pos-tree-view-target)))
      (pos-tree-view-previous)
      (should (equal '(scope . ".") (get-text-property (point) 'pos-tree-view-target)))
      (pos-tree-view-previous)
      (should (= (point) (point-min))))))

(ert-deftest pos-tree-view/the-tree-is-read-once-and-again-on-g ()
  "Both buffers show what was read once; g reads the tree again."
  (pos-tree-view-test-with-tree
    (let ((reads 0))
      (cl-letf* ((read (symbol-function 'pos-tree-data))
                 ((symbol-function 'pos-tree-data)
                  (lambda (&rest args) (setq reads (1+ reads)) (apply read args))))
        (pos-tree-view root)
        (pos-tree-view-scope "home" root)
        (pos-tree-view-repositories root)
        (should (= 1 reads))
        (with-temp-file (expand-file-name "home/index.org" root)
          (insert "#+TITLE: Hearth\n"))
        (with-current-buffer "*pos-tree*"
          (should (pos-tree-view-test-line "Home"))
          (revert-buffer)
          (should (= 2 reads))
          (should (pos-tree-view-test-line "Hearth")))))))

(provide 'pos-tree-view-test)
;;; pos-tree-view-test.el ends here
