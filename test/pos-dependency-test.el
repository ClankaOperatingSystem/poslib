;;; pos-dependency-test.el --- Tests for the dependency rule -*- lexical-binding: t; -*-

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
;; Each test is named for the rule it pins, and its tree is in the
;; test.  The root declares area, a responsibility, and kit, a product;
;; its projects lie in projects/.

;;; Code:

(require 'ert)
(require 'pos-dependency)
(require 'pos-lint)
(require 'pos-test-support)

(defconst pos-dependency-test-tree
  `((".pos/config.yaml" . ,(pos-test-declaring "area" "kit"))
    ("area/.pos/config.yaml" . ,pos-test-child-config)
    ("area/plan.org" . ":PROPERTIES:\n:ID: plan\n:END:\n#+TITLE: Plan\n")
    ("kit/README.md" . "# Kit\n"))
  "The files of a root, a responsibility and a product, every test's.")

(defun pos-dependency-test-findings (root)
  "Return the findings of the dependency rule in ROOT, as lines."
  (pos-lint-format (pos-dependency-findings (pos-corpus root)) root))

(ert-deftest pos-dependency/canon-links-to-its-own-and-its-childrens-canon ()
  "A link out of a scope, into an attic or into a child's archive is found.
A link to the scope's own canon or archive, or to a child's canon, is
not.  A project of one file links to nothing but itself.  A link that
leads nowhere is found, and one to a file the scope's archive now
holds names it."
  (pos-test-with-files root
      (append pos-dependency-test-tree
              '(("notes.org"
                 . "[[file:area/plan.org]] [[file:archives/old.org]]
[[file:area/archives/report.org]]
[[file:attic/draft.org]]
[[file:gone.org]]
[[file:moved.org]]
")
                ("archives/old.org" . "")
                ("archives/moved.org" . "")
                ("area/archives/report.org" . "")
                ("attic/draft.org" . "")
                ("area/notes.org" . "[[file:plan.org]] [[file:../notes.org]]\n")
                ("projects/one.org" . "[[file:one.org]] [[file:../notes.org]]\n")))
    (should (equal (pos-test-lines
                    "area/notes.org:1: D-canon-links: forbidden: outside the scope: file:../notes.org"
                    "notes.org:2: D-child-archive-links: poor form: into a child's archive: file:area/archives/report.org"
                    "notes.org:3: D-attic-links: forbidden: into an attic: file:attic/draft.org"
                    "notes.org:4: D-links-resolve: does not resolve: file:gone.org"
                    "notes.org:5: D-links-resolve: moved to archives/moved.org: file:moved.org"
                    "projects/one.org:1: D-canon-links: forbidden: outside a single-file scope: file:../notes.org")
                   (pos-dependency-test-findings root)))))

(ert-deftest pos-dependency/nothing-beside-the-root-or-elsewhere-on-disk ()
  "A path out of the root is found, in Org, in Markdown and in code.
A relative path that climbs out of the root and a path beside the root
are forbidden; a path into a home directory is inadvisable.  In code a
climbing path and Path.parents[N] are paths too; in Org and Markdown
only a home directory's is."
  (pos-test-with-temp-dir root
    (let ((beside (expand-file-name "../peer.org" root)))
      (pos-test-write-files
       root
       (append pos-dependency-test-tree
               `(("notes.org" . ,(format "[[file:../outside.org]]\n[[file:%s]]\nSee ~/.tool/rc.\n"
                                         beside))
                 ("guide.md" . "Read ../peer.md and [it](../peer.md).\n")
                 ("tool/run.py" . ,(concat "BASE = Path(__file__).parents[1]\n"
                                           "PEER = Path(__file__).parents[2]\n"
                                           "DATA = '../../data'\n"
                                           "CONF = '~/.tool/rc'\n")))))
      (should (equal (pos-test-lines
                      "guide.md:1: D-sibling-links: forbidden: climbs out of the root: ../peer.md"
                      "notes.org:1: D-sibling-links: forbidden: climbs out of the root: file:../outside.org"
                      (format "notes.org:2: D-sibling-links: forbidden: a sibling on disk: file:%s" beside)
                      "notes.org:3: D-outside-links: inadvisable: outside the root on disk: ~/.tool/rc"
                      "tool/run.py:2: D-sibling-links: forbidden: climbs out of the root: .parents[2]"
                      "tool/run.py:3: D-sibling-links: forbidden: climbs out of the root: ../../data"
                      "tool/run.py:4: D-outside-links: inadvisable: outside the root on disk: ~/.tool/rc")
                     (pos-dependency-test-findings root))))))

(ert-deftest pos-dependency/an-id-link-resolves-to-one-file-within-the-scope ()
  "An ID link is followed to the file that holds the ID.
One no file holds, one two places hold, and one that leads out of the
scope are found.  A link in a property's value is followed too, and a
person's identity, in a configuration directory, holds its ID."
  (pos-test-with-files root
      (append pos-dependency-test-tree
              '(("notes.org" . ":PROPERTIES:\n:ID: notes\n:END:
[[id:plan]] [[id:nowhere]]
[[id:twice]]
* WAITING Word from Ann
:PROPERTIES:
:WAITING_ON: [[id:ann-here][Ann@here]] [[id:ann-gone][Ann@gone]]
:END:
")
                ("a.org" . ":PROPERTIES:\n:ID: twice\n:END:\n")
                ("b.org" . "* Heading\n:PROPERTIES:\n:ID: twice\n:END:\n")
                (".pos/person-identities/ann.org" . ":PROPERTIES:\n:ID: ann-here\n:END:\n")
                ("area/top.org" . "[[id:plan]] [[id:notes]]\n")))
    (should (equal (pos-test-lines
                    "area/top.org:1: D-canon-links: forbidden: outside the scope: id:notes (notes.org)"
                    "notes.org:4: D-links-resolve: no file holds the ID: id:nowhere"
                    "notes.org:5: D-links-resolve: the ID is in a.org, b.org::*Heading: id:twice"
                    "notes.org:8: D-links-resolve: no file holds the ID: id:ann-gone")
                   (pos-dependency-test-findings root)))))

(ert-deftest pos-dependency/a-pragma-allows-a-line-a-block-or-a-file ()
  "A pragma on the line, on the line above, or above the block exempts it.
One in the first five lines exempts the file.  It names the rule it
allows, and allows no other."
  (pos-test-with-files root
      (append pos-dependency-test-tree
              '(("notes.org" . "[[file:gone.org]] doctrine: allow D-links-resolve
# doctrine: allow D-links-resolve (the next line)
[[file:gone.org]]
# doctrine: allow D-outside-links (another rule)
[[file:gone.org]]
")
                ("guide.md" . "# Guide

<!-- doctrine: allow D-outside-links (an example) -->
```sh
run ~/.tool/x
run ~/.tool/y
```

```sh
run ~/.tool/z
```
")
                ("tool/run.py" . "# doctrine: allow-file D-sibling-links (fixtures)\nA = '../../a'\n")))
    (should (equal (pos-test-lines
                    "guide.md:10: D-outside-links: inadvisable: outside the root on disk: ~/.tool/z"
                    "notes.org:5: D-links-resolve: does not resolve: file:gone.org")
                   (pos-dependency-test-findings root)))))

(ert-deftest pos-dependency/a-products-files-are-not-checked ()
  "A file the root may not write is not found for what it refers to.
The root's link into the product is a link to a child's canon."
  (pos-test-with-files root
      (append pos-dependency-test-tree
              '(("notes.org" . "[[file:kit/README.md]]\n")
                ("kit/notes.org" . "[[file:../notes.org]] [[file:gone.org]]\n")
                ("kit/run.py" . "A = '../../a'\n")))
    (should (equal "" (pos-dependency-test-findings root)))))

(ert-deftest pos-dependency/the-lint-reports-the-rule ()
  "The lint reports the dependency rule's findings among its own."
  (pos-test-with-files root
      (append pos-dependency-test-tree
              '(("notes.org" . ":PROPERTIES:\n:ID: notes\n:END:\n[[file:gone.org]]\n")))
    (let ((pos-directory root))
      (should (equal (pos-test-lines
                      "notes.org:4: D-links-resolve: does not resolve: file:gone.org")
                     (pos-lint-format
                      (seq-filter (lambda (finding)
                                    (string-prefix-p "D-" (pos-finding-message finding)))
                                  (pos-lint))
                      root))))))

(provide 'pos-dependency-test)
;;; pos-dependency-test.el ends here
