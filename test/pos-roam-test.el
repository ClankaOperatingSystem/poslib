;;; pos-roam-test.el --- Tests for pos-roam.el  -*- lexical-binding: t -*-

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

;; Run: make test.
;;
;; Each test is named for the rule it pins.  The index of a tree: what
;; it covers, what it records, and whose database it is.  The index of
;; each temporary repository goes to the cache test/pos-test-support.el
;; names, never to a database of the user's.

;;; Code:

(require 'ert)
(require 'pos-roam)
(require 'pos-test-support)

(ert-deftest pos-roam/the-index-covers-the-corpus-and-no-more ()
  "The files indexed are the corpus: what the configuration allows.
Archives, attics, and hidden and underscore directories are left out
by the default exclusions, a product repository by having no
configuration, and a lock file by its name."
  (pos-test-with-files root '(("a.org" . "") ("sub/b.org" . "")
                              ("archives/x.org" . "") ("sub/attic/y.org" . "")
                              ("_work/z.org" . "") (".hidden/h.org" . "")
                              ("sub/_deep/archives/w.org" . "")
                              ("vendor/lib/.git/HEAD" . "ref: refs/heads/master\n")
                              ("vendor/lib/README.org" . "")
                              (".#a.org" . ""))
    (should (equal '("a.org" "sub/b.org")
                   (mapcar (lambda (file) (file-relative-name file root))
                           (pos-roam-files root))))))

(ert-deftest pos-roam/an-editor-set-up-on-a-tree-shares-the-commands-index ()
  "An editor pointed at a tree indexes its corpus in the commands' database.
The database is the one the commands keep for the tree, in the cache,
and archives and underscore directories are left out of it."
  (pos-test-with-files root '(("a.org" . ":PROPERTIES:\n:ID: file-a\n:END:\n")
                              ("archives/x.org" . ":PROPERTIES:\n:ID: file-x\n:END:\n")
                              ("_work/z.org" . ":PROPERTIES:\n:ID: file-z\n:END:\n"))
    (let ((org-roam-directory nil)
          (org-roam-db-location nil)
          (pos-roam--set-up nil)
          (expected (let ((org-roam-directory nil))
                      (pos-roam-db-location
                       (file-name-as-directory (file-truename root))))))
      (unwind-protect
          (progn
            (pos-roam-setup root)
            (should (equal expected org-roam-db-location))
            (should (string-prefix-p (file-name-as-directory pos-roam-cache-directory)
                                     org-roam-db-location))
            (org-roam-db-sync)
            (should (equal '("a.org")
                           (mapcar (lambda (row)
                                     (file-relative-name (car row) (file-truename root)))
                                   (org-roam-db-query [:select file :from files]))))
            (pos-roam-with-index root
              (should (equal expected org-roam-db-location))))
        (advice-remove 'org-roam-list-files #'pos-roam--list-files)))))

(ert-deftest pos-roam/the-referrers-are-the-id-links-the-index-records ()
  "A link is recorded where a node holds it.
In a file with an ID, or under a heading with one.  A link in a file
with neither is not."
  (pos-test-with-files root '(("a.org" . "* Target\n:PROPERTIES:\n:ID: target\n:END:\n")
                              ("b.org" . ":PROPERTIES:\n:ID: file-b\n:END:\nSee [[id:target][it]].\n")
                              ("c.org" . "* Note\n:PROPERTIES:\n:ID: note-c\n:END:\n[[id:target]]\n")
                              ("d.org" . "Unindexed: [[id:target]]\n")
                              ("archives/e.org" . ":PROPERTIES:\n:ID: file-e\n:END:\n[[id:target]]\n"))
    (pos-roam-with-index root
      (should (equal '(("b.org" . 36) ("c.org" . 39))
                     (mapcar (lambda (referrer)
                               (cons (file-relative-name (car referrer) (file-truename root))
                                     (cdr referrer)))
                             (pos-roam-referrers "target")))))))

(ert-deftest pos-roam/the-index-follows-what-changed-on-disk ()
  "The index is brought up to date each time it is used."
  (pos-test-with-files root '(("a.org" . "* Target\n:PROPERTIES:\n:ID: target\n:END:\n")
                              ("b.org" . ":PROPERTIES:\n:ID: file-b\n:END:\n"))
    (pos-roam-with-index root
      (should-not (pos-roam-referrers "target")))
    (with-temp-file (expand-file-name "b.org" root)
      (insert ":PROPERTIES:\n:ID: file-b\n:END:\n[[id:target]]\n"))
    (pos-roam-with-index root
      (should (equal 1 (length (pos-roam-referrers "target")))))))

(ert-deftest pos-roam/a-link-is-rewritten-there-and-no-other ()
  "The link at the recorded place is pointed elsewhere.
One that has moved is left, and reported; a dry run reports without
writing."
  (pos-test-with-files root '(("b.org" . "Lead [[id:old][x]] and [[id:old]].\n"))
    (let ((file (expand-file-name "b.org" root)))
      (should (pos-roam-rewrite-link file 6 "old" "new" t))
      (with-current-buffer (find-file-noselect file)
        (should-not (buffer-modified-p)))
      (should (pos-roam-rewrite-link file 6 "old" "new"))
      (should-not (pos-roam-rewrite-link file 7 "old" "new"))
      (with-current-buffer (find-file-noselect file) (save-buffer))
      (should (equal "Lead [[id:new][x]] and [[id:old]].\n"
                     (pos-test-file-string file))))))

(ert-deftest pos-roam/the-database-is-the-users-only-around-the-root ()
  "A root within `org-roam-directory' is served by that index.
Any other root has one of its own, named by its path, in the cache."
  (pos-test-with-files root '(("a.org" . ""))
    (let ((org-roam-directory (file-name-directory (directory-file-name root)))
          (org-roam-db-location "/nowhere/theirs.db"))
      (should (equal "/nowhere/theirs.db" (pos-roam-db-location root))))
    (let ((org-roam-directory (expand-file-name "elsewhere" root))
          (org-roam-db-location "/nowhere/theirs.db"))
      (make-directory org-roam-directory)
      (should (equal (expand-file-name (concat (sha1 (file-truename root)) ".db")
                                       pos-roam-cache-directory)
                     (pos-roam-db-location root))))))

(ert-deftest pos-roam/indexing-writes-no-database-of-the-users-elsewhere ()
  "Indexing a tree outside `org-roam-directory' leaves its database alone.
Whatever org-roam's variables are rebound to meanwhile."
  (pos-test-with-files root '(("a.org" . "* Target\n:PROPERTIES:\n:ID: target\n:END:\n"))
    (let* ((theirs (expand-file-name "theirs.db" root))
           (org-roam-directory (expand-file-name "elsewhere" root))
           (org-roam-db-location theirs))
      (make-directory org-roam-directory)
      (pos-roam-with-index root
        (should (equal 1 (caar (org-roam-db-query [:select (funcall count *) :from nodes])))))
      (should-not (file-exists-p theirs))
      (should (file-exists-p (expand-file-name (concat (sha1 (file-truename root)) ".db")
                                               pos-roam-cache-directory))))))

(provide 'pos-roam-test)
;;; pos-roam-test.el ends here
