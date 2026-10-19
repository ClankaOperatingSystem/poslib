;;; pos-org-html-test.el --- Tests for pos-org-html -*- lexical-binding: t; -*-

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

;; Org files as HTML for a viewer: where each kind of link leads, the
;; anchors of headings, the titles, and what is not done.

;;; Code:

(require 'ert)
(require 'pos-org-html)
(require 'pos-test-support)

(defvar pos-org-html-test-ran nil
  "Set by a source block, if one is ever evaluated.")

(defmacro pos-org-html-test-with-tree (&rest body)
  "Evaluate BODY with `root' holding a tree of linked Org files.
index.org links to each kind of target; home/index.org has headings
with an ID, with a CUSTOM_ID, with neither, and two with one title."
  (declare (indent 0))
  `(pos-test-with-files root
       `(("index.org"
          . ,(pos-test-lines
              "#+TITLE: Root & branch"
              "* Tasks"
              "- [[file:home/index.org::*The hall][the hall]]"
              "- [[file:home/index.org::#porch][the porch]]"
              "- [[file:home/index.org]]"
              "- [[id:roof-id][the roof]]"
              "- [[*Tasks][these tasks]]"
              "- [[https://example.org/a?b=1&c=2][a site]]"
              "- [[ipfs://bafyexample/notes.org][a sealed note]]"
              "- [[elisp:(setq pos-org-html-test-ran t)][a command]]"
              "- [[file:notes.txt][some notes]]"
              "- [[file:../outside.org][outside]]"
              "#+begin_src emacs-lisp :exports results"
              "(setq pos-org-html-test-ran t)"
              "#+end_src"))
         ("home/index.org"
          . ,(pos-test-lines
              "* The hall"
              "* The roof"
              ":PROPERTIES:"
              ":ID:       roof-id"
              ":END:"
              "* The porch"
              ":PROPERTIES:"
              ":CUSTOM_ID: porch"
              ":END:"
              "* Notes"
              "* Notes"))
         ("notes.txt" . "Some notes.\n"))
     ,@body))

(defun pos-org-html-test-html (root file)
  "Return the HTML pos-org-html gives FILE under ROOT."
  (alist-get 'html (aref (pos-org-html root (list file)) 0)))

(ert-deftest pos-org-html-file-links-lead-to-pages-and-headings ()
  "A link to an Org file under the root leads to its page, at the heading named."
  (pos-org-html-test-with-tree
    (let ((html (pos-org-html-test-html root "index.org")))
      (should (string-match-p "<a href=\"/file/home/index.org#the-hall\">the hall</a>" html))
      (should (string-match-p "<a href=\"/file/home/index.org#porch\">the porch</a>" html))
      (should (string-match-p "<a href=\"/file/home/index.org\">home/index.org</a>" html)))))

(ert-deftest pos-org-html-other-links ()
  "An ID, a heading here, a web link and other links each lead where they should."
  (pos-org-html-test-with-tree
    (let ((html (pos-org-html-test-html root "index.org")))
      (should (string-match-p "<a href=\"/id/roof-id\">the roof</a>" html))
      (should (string-match-p "<a href=\"#tasks\">these tasks</a>" html))
      (should (string-match-p "<a href=\"https://example.org/a\\?b=1&amp;c=2\">a site</a>" html))
      (should (string-match-p "<a href=\"ipfs://bafyexample/notes.org\">a sealed note</a>" html))
      (should (string-match-p "<li>a command</li>" html))
      (should (string-match-p "<code>some notes</code>" html))
      (should (string-match-p "<code>outside</code>" html)))))

(ert-deftest pos-org-html-the-addresses-can-be-changed ()
  "The formats of a page's address and an ID's address are the caller's."
  (pos-org-html-test-with-tree
    (let* ((pos-org-html-file-url "/garden/%s.html")
           (pos-org-html-id-url "/garden/ids/%s")
           (html (pos-org-html-test-html root "index.org")))
      (should (string-match-p "href=\"/garden/home/index.org.html#porch\"" html))
      (should (string-match-p "href=\"/garden/ids/roof-id\"" html)))))

(ert-deftest pos-org-html-headings-have-anchors ()
  "A heading's anchor is its CUSTOM_ID or its slug, each once, and its ID."
  (pos-org-html-test-with-tree
    (let ((html (pos-org-html-test-html root "home/index.org")))
      (dolist (anchor '("the-hall" "the-roof" "porch" "notes" "notes-2"))
        (should (string-match-p (format "<h[0-9] id=\"%s\"" anchor) html)))
      (should (string-match-p "<h[0-9] id=\"the-roof\"><a id=\"roof-id\"></a>" html)))))

(ert-deftest pos-org-html-slug ()
  "A slug is the title in lower case, with hyphens between its words."
  (should (equal "the-hall" (pos-org-html-slug "The hall")))
  (should (equal "what-s-next" (pos-org-html-slug "  What's next?  ")))
  (should (equal "café-2026" (pos-org-html-slug "Café 2026"))))

(ert-deftest pos-org-html-titles-errors-and-order ()
  "Each file has its title or :null; a missing file has an error; the order is kept."
  (pos-org-html-test-with-tree
    (let ((files (pos-org-html root '("home/index.org" "missing.org" "index.org"))))
      (should (equal '("home/index.org" "missing.org" "index.org")
                     (mapcar (lambda (file) (alist-get 'file file)) files)))
      (should (eq :null (alist-get 'title (aref files 0))))
      (should (alist-get 'error (aref files 1)))
      (should-not (alist-get 'html (aref files 1)))
      (should (equal "Root & branch" (alist-get 'title (aref files 2)))))))

(ert-deftest pos-org-html-evaluates-and-writes-nothing ()
  "No source block is evaluated, and the files are left as they were."
  (pos-org-html-test-with-tree
    (let ((before (with-temp-buffer
                    (insert-file-contents (expand-file-name "home/index.org" root))
                    (buffer-string))))
      (setq pos-org-html-test-ran nil)
      (pos-org-html root '("index.org" "home/index.org"))
      (should-not pos-org-html-test-ran)
      (should (equal before (with-temp-buffer
                              (insert-file-contents (expand-file-name "home/index.org" root))
                              (buffer-string)))))))

(ert-deftest pos-org-html-json ()
  "The result serializes as JSON."
  (pos-org-html-test-with-tree
    (let ((json (json-parse-string
                 (decode-coding-string
                  (json-serialize (pos-org-html root '("index.org" "missing.org")))
                  'utf-8)
                 :object-type 'alist)))
      (should (equal "Root & branch" (alist-get 'title (aref json 0)))))))

(provide 'pos-org-html-test)
;;; pos-org-html-test.el ends here
