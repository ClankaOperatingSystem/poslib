;;; pos-links-test.el --- Tests for pos-links.el  -*- lexical-binding: t -*-

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

;; What these tests show, after the Links section of doc/formats.org:
;; links are found in Org and Markdown files as Org and markdown-mode
;; find them, at byte offsets a rewrite can trust; a rewrite changes
;; only the links it was given and refuses one that has moved; an
;; annotation keeps the link's text; a link resolves to the CID of a
;; sealed item in the scope's archive, to a rumour of anything else that
;; exists, a container's archive included, or to nothing; and a garden
;; is the repository, beyond which nothing is read.
;;
;; The archives are real: pos-seal seals each in a temporary directory,
;; so `pos-links-link' is checked against what a seal actually wrote.

;;; Code:

(require 'ert)
(require 'pos-links)
(require 'pos-seal)
(require 'pos-test-support)

;;;; Fixtures

(defconst pos-links-test-org
  (concat "#+TITLE: A\n\n"
          "See [[file:notes/b.org::*Heading][the notes]], [[file:c.md]], [[./d.txt][d]],\n"
          "[[https://example.org][web]] and [[id:123][id]].\n")
  "An Org file with three file links, a web link and an id link.")

(defconst pos-links-test-markdown
  (concat "# A\n\n"
          "See [the notes](notes/b.md#frag), ![a picture](pic.png), [web](https://example.org)\n"
          "and `[code](x.md)` and [a ref][r].\n\n"
          "```\n[fenced](y.md)\n```\n\n"
          "[r]: refs/z.md \"Z\"\n[s]: #anchor\n[t]: mailto:x@example.org\n")
  "A Markdown file with a link, an image and a reference definition.")

(defconst pos-links-test-garden
  '(("garden/.git/HEAD" . "ref: refs/heads/master\n"))
  "The files that make garden/ a repository, and so a garden.")

(defun pos-links-test-seal (scope item)
  "Seal SCOPE's ITEM, a directory under it, into SCOPE's archive.
Return the archive's true name."
  (make-directory (expand-file-name "archives" scope) t)
  (let ((plan (pos-seal-plan (expand-file-name item scope)
                             (expand-file-name (concat "archives/" item) scope))))
    (pos-seal-apply plan (secure-hash 'sha256 (pos-ledger-json plan)))
    (file-truename (expand-file-name "archives" scope))))

;;;; Finding

(ert-deftest pos-links/org-file-links-are-found-with-their-search-options ()
  "Org file links are found in any form, their search options kept.
The suffix keeps its ::, and the raw link is the whole an annotation
replaces.  A link of another type, web or id, is not a path link, and a
file that is neither Org nor Markdown has no links."
  (pos-test-with-files dir `(("a.org" . ,pos-links-test-org)
                             ("a.txt" . "[[file:x.org]]\n"))
    (should (equal '((18 "file:notes/b.org::*Heading" "notes/b.org" "::*Heading"
                         18 "file:notes/b.org::*Heading")
                     (61 "file:c.md" "c.md" "" 61 "file:c.md")
                     (76 "./d.txt" "./d.txt" "" 76 "./d.txt"))
                   (mapcar (lambda (link) (seq-take link 6))
                           (pos-links-in-file (expand-file-name "a.org" dir)))))
    (should-not (pos-links-in-file (expand-file-name "a.txt" dir)))))

(ert-deftest pos-links/markdown-links-are-found-with-their-fragments ()
  "Markdown inline links, images and reference definitions are found.
Fragments are kept and paths URL-decoded; an inline link's whole is all
of it with its text, a definition's is its URL.  A URL with a scheme, a
bare fragment, and links in inline or fenced code are not path links."
  (pos-test-with-files dir `(("a.md" . ,pos-links-test-markdown)
                             ("b.md" . "[n](notes/b%20c.org#frag)\n"))
    (should (equal '((21 "notes/b.md#frag" "notes/b.md" "#frag"
                         9 "[the notes](notes/b.md#frag)" "the notes")
                     (52 "pic.png" "pic.png" "" 39 "![a picture](pic.png)" "a picture")
                     (154 "refs/z.md" "refs/z.md" "" 154 "refs/z.md" nil))
                   (pos-links-in-file (expand-file-name "a.md" dir))))
    ;; The decoded path is the file on disk; the text is what a rewrite
    ;; replaces; the whole and description are read after decoding, which
    ;; searches strings of its own.
    (should (equal '((4 "notes/b%20c.org#frag" "notes/b c.org" "#frag"
                        0 "[n](notes/b%20c.org#frag)" "n"))
                   (pos-links-in-file (expand-file-name "b.md" dir))))))

(ert-deftest pos-links/a-markdown-url-in-angle-brackets-is-a-link ()
  "A URL written <with spaces> is found whole, with its description."
  (pos-test-with-files dir '(("a.md" . "![img](<pic one.png>)\n"))
    (should (equal '((7 "<pic one.png>" "pic one.png" "" 0 "![img](<pic one.png>)" "img"))
                   (pos-links-in-file (expand-file-name "a.md" dir))))))

(ert-deftest pos-links/offsets-count-bytes-not-characters ()
  "A link after a two-byte character is at its byte offset.
That is one more than its character position, in Org and Markdown alike."
  (pos-test-with-files dir '(("a.md" . "Café [tail](t.md)\n")
                             ("a.org" . "Café [[file:t.org]]\n"))
    (should (equal '((13 "t.md" "t.md" "" 6 "[tail](t.md)" "tail"))
                   (pos-links-in-file (expand-file-name "a.md" dir))))
    (should (equal '((8 "file:t.org" "t.org" "" 8 "file:t.org"))
                   (mapcar (lambda (link) (seq-take link 6))
                           (pos-links-in-file (expand-file-name "a.org" dir)))))))

;;;; Rewriting and annotating

(ert-deftest pos-links/a-rewrite-changes-only-the-links-it-is-given ()
  "A rewrite replaces each link at its offset and nothing else.
The rewrites may come in any order, the text around them is untouched,
and no rewrites leave the bytes as they were."
  (let ((bytes (encode-coding-string "Café [a](x.md) and [b](y.md)\n" 'utf-8))
        (rewrites '((10 "x.md" "ipfs://X") (24 "y.md" "ipfs://Y"))))
    (should (equal (encode-coding-string "Café [a](ipfs://X) and [b](ipfs://Y)\n" 'utf-8)
                   (pos-links-rewrite bytes rewrites)))
    (should (equal (encode-coding-string "Café [a](ipfs://X) and [b](ipfs://Y)\n" 'utf-8)
                   (pos-links-rewrite bytes (reverse rewrites))))
    (should (equal '((10 "x.md" "ipfs://X") (24 "y.md" "ipfs://Y")) rewrites))
    (should (equal bytes (pos-links-rewrite bytes nil)))))

(ert-deftest pos-links/a-rewrite-refuses-a-link-that-is-not-where-it-was-planned ()
  "A rewrite whose text is not at its offset is refused as a plan failure.
Nothing is returned for the other links."
  (let ((bytes (encode-coding-string "See [a](x.md).\n" 'utf-8)))
    (should (equal 'plan
                   (cadr (should-error (pos-links-rewrite bytes '((7 "x.md" "y.md")))
                                       :type 'pos-ledger-refused))))
    (should (equal 'plan
                   (cadr (should-error (pos-links-rewrite bytes '((8 "x.txt" "y.md")))
                                       :type 'pos-ledger-refused))))))

(ert-deftest pos-links/an-org-annotation-makes-the-label-the-link-s-type ()
  "In Org the annotation is the label as the link's type.
It replaces file: or no type, keeping the path and its search option;
the description outside the raw link is not the annotation's to touch."
  (should (equal "broken:notes/b.org::*Heading"
                 (pos-links-annotate "a.org"
                                     '(18 "file:notes/b.org::*Heading" "notes/b.org"
                                          "::*Heading" 18 "file:notes/b.org::*Heading")
                                     "broken")))
  (should (equal "later:./d.txt"
                 (pos-links-annotate "a.org" '(76 "./d.txt" "./d.txt" "" 76 "./d.txt")
                                     "later"))))

(ert-deftest pos-links/a-markdown-annotation-keeps-the-text-and-notes-the-link ()
  "In Markdown a link becomes its text and a bracketed note of the link.
The note gives the link as written, fragment and all; an image's text is
kept the same way; a reference definition, which has no text, takes the
label as a prefix."
  (let ((link '(21 "notes/b.md#frag" "notes/b.md" "#frag"
                   9 "[the notes](notes/b.md#frag)" "the notes")))
    (should (equal "the notes [broken link: notes/b.md#frag]"
                   (pos-links-annotate "a.md" link "broken")))
    (should (equal "the notes [later record: notes/b.md#frag]"
                   (pos-links-annotate "a.md" link "later"))))
  (should (equal "a picture [broken link: pic.png]"
                 (pos-links-annotate "a.md" '(52 "pic.png" "pic.png" ""
                                                 39 "![a picture](pic.png)" "a picture")
                                     "broken")))
  (should (equal "broken:refs/z.md"
                 (pos-links-annotate "a.md" '(154 "refs/z.md" "refs/z.md" ""
                                                  154 "refs/z.md" nil)
                                     "broken"))))

;;;; The garden

(ert-deftest pos-links/a-garden-is-the-repository-else-the-outermost-scope-with-an-archive ()
  "A garden is the repository, else the outermost scope with an archive.
Where there is no repository, it is the outermost directory above with
an archives/, and failing that the scope itself.  The name is the true
one."
  (pos-test-with-files dir pos-links-test-garden
    (make-directory (expand-file-name "garden/scope/archives" dir) t)
    (should (equal (file-truename (expand-file-name "garden/" dir))
                   (pos-links-garden (expand-file-name "garden/scope" dir)))))
  (pos-test-with-temp-dir dir
    (make-directory (expand-file-name "garden/archives" dir) t)
    (make-directory (expand-file-name "garden/scope/archives" dir) t)
    (make-directory (expand-file-name "garden/scope/child" dir) t)
    (should (equal (file-truename (expand-file-name "garden/" dir))
                   (pos-links-garden (expand-file-name "garden/scope/child" dir))))
    (should (equal (file-truename (expand-file-name "garden/" dir))
                   (pos-links-garden (expand-file-name "garden/scope" dir))))
    (should (equal (file-truename (expand-file-name "alone/" dir))
                   (pos-links-garden (expand-file-name "alone/" dir))))))

(ert-deftest pos-links/a-target-outside-the-garden-is-not-read ()
  "A path beyond the repository lies outside the garden and is not read.
Its description says only that: its title and size are never read."
  (pos-test-with-files dir `(,@pos-links-test-garden
                             ("garden/scope/notes.org" . "#+TITLE: Notes\n")
                             ("elsewhere/secret.org" . "#+TITLE: Secret\n"))
    (let ((scope (expand-file-name "garden/scope/" dir)))
      (should (pos-links-outside-garden-p (expand-file-name "elsewhere/secret.org" dir)
                                          scope))
      (should-not (pos-links-outside-garden-p (expand-file-name "garden/scope/notes.org" dir)
                                              scope))
      (should-not (pos-links-outside-garden-p (expand-file-name "garden/missing.org" dir)
                                              scope))
      (should (equal "outside the garden, and was not read"
                     (pos-links-description (expand-file-name "elsewhere/secret.org" dir)
                                            scope))))))

(ert-deftest pos-links/a-description-gives-size-hash-and-title ()
  "A file in the garden is described by its size, its SHA-256 and title.
The title is an Org #+TITLE or a Markdown # heading, trimmed, when it
has one; a directory is a directory."
  (let ((org "#+TITLE:  Notes \n\nText.\n")
        (md "# Read me\n\nText.\n")
        (txt "plain\n"))
    (pos-test-with-files dir `(,@pos-links-test-garden
                               ("garden/scope/notes.org" . ,org)
                               ("garden/scope/readme.md" . ,md)
                               ("garden/scope/plain.txt" . ,txt)
                               ("garden/scope/sub/x.txt" . "x\n"))
      (let ((scope (expand-file-name "garden/scope/" dir)))
        (should (equal (format "a file of %d bytes, SHA-256 =%s=, titled \"Notes\""
                               (length org) (secure-hash 'sha256 org))
                       (pos-links-description (expand-file-name "notes.org" scope) scope)))
        (should (equal (format "a file of %d bytes, SHA-256 =%s=, titled \"Read me\""
                               (length md) (secure-hash 'sha256 md))
                       (pos-links-description (expand-file-name "readme.md" scope) scope)))
        (should (equal (format "a file of %d bytes, SHA-256 =%s="
                               (length txt) (secure-hash 'sha256 txt))
                       (pos-links-description (expand-file-name "plain.txt" scope) scope)))
        (should (equal "a directory"
                       (pos-links-description (expand-file-name "sub" scope) scope)))))))

;;;; Resolving

(ert-deftest pos-links/a-link-into-an-archive-is-within-its-scope ()
  "An archive is within a scope when it is the scope's own or beneath it.
A scope's beneath it counts; a container's archive does not, nor does a
sibling's whose name merely begins the same way."
  (pos-test-with-temp-dir dir
    (dolist (archive '("garden/archives" "garden/scope/archives"
                       "garden/scope/child/archives" "garden/scope2/archives"))
      (make-directory (expand-file-name archive dir) t))
    (let ((scope (expand-file-name "garden/scope" dir)))
      (should (pos-links-within-scope-p (expand-file-name "garden/scope/archives" dir) scope))
      (should (pos-links-within-scope-p (expand-file-name "garden/scope/child/archives" dir)
                                        scope))
      (should-not (pos-links-within-scope-p (expand-file-name "garden/archives" dir) scope))
      (should-not (pos-links-within-scope-p (expand-file-name "garden/scope2/archives" dir)
                                            scope)))))

(ert-deftest pos-links/a-link-to-nothing-is-broken ()
  "A target that does not exist resolves as broken.
That holds whether it would lie in canon or in an archive on disk that
never had it."
  (pos-test-with-files dir pos-links-test-garden
    (make-directory (expand-file-name "garden/scope/archives" dir) t)
    (let ((scope (expand-file-name "garden/scope/" dir))
          (canon (expand-file-name "garden/scope/missing.org" dir))
          (archived (expand-file-name "garden/scope/archives/missing/x.txt" dir)))
      (should (equal (cons 'broken canon) (pos-links-resolve canon scope)))
      (should (equal (cons 'broken archived) (pos-links-resolve archived scope)))
      (should (equal (cons 'broken canon) (pos-links-resolve canon))))))

(ert-deftest pos-links/a-link-into-the-scope-s-archive-cites-the-sealed-item ()
  "A path in the scope's own archive resolves to the sealed item's CID.
The link is ipfs://, the CID of the sealed item holding it, and the path
within the item unless it is the item; a path in the archive that was
never sealed is refused."
  (pos-test-with-files dir `(,@pos-links-test-garden
                             ("garden/scope/trial/result.md" . "result\n")
                             ("garden/scope/trial/notes/deep.txt" . "deep\n"))
    (let* ((scope (expand-file-name "garden/scope/" dir))
           (archive (pos-links-test-seal scope "trial"))
           (cid (pos-cid-directory (expand-file-name "trial" archive))))
      (should (equal (cons 'cid (concat "ipfs://" cid))
                     (pos-links-resolve (expand-file-name "trial" archive) scope)))
      (should (equal (cons 'cid (concat "ipfs://" cid "/result.md"))
                     (pos-links-resolve (expand-file-name "trial/result.md" archive) scope)))
      (should (equal (cons 'cid (concat "ipfs://" cid "/notes/deep.txt"))
                     (pos-links-resolve (expand-file-name "trial/notes/deep.txt" archive)
                                        scope)))
      (pos-test-write-files archive '(("stray.txt" . "stray\n")))
      (should (equal 'unsealed
                     (cadr (should-error
                            (pos-links-resolve (expand-file-name "stray.txt" archive) scope)
                            :type 'pos-ledger-refused)))))))

(ert-deftest pos-links/a-link-into-a-container-s-archive-is-a-rumour ()
  "From a scope, a sealed path in its container's archive is a rumour.
So is canon and anything else that exists; the same path is cited by
CID when no scope is given, since any archive is cited then."
  (pos-test-with-files dir `(,@pos-links-test-garden
                             ("garden/trial/x.txt" . "x\n")
                             ("garden/scope/notes.org" . "#+TITLE: Notes\n")
                             ("elsewhere/secret.org" . "#+TITLE: Secret\n"))
    (let* ((garden (expand-file-name "garden/" dir))
           (scope (expand-file-name "garden/scope/" dir))
           (archive (pos-links-test-seal garden "trial"))
           (sealed (expand-file-name "trial/x.txt" archive))
           (canon (expand-file-name "notes.org" scope))
           (outside (expand-file-name "elsewhere/secret.org" dir)))
      (should (equal (cons 'rumour sealed) (pos-links-resolve sealed scope)))
      (should (equal (cons 'rumour canon) (pos-links-resolve canon scope)))
      (should (equal (cons 'rumour outside) (pos-links-resolve outside scope)))
      (should (equal (cons 'cid (concat "ipfs://"
                                        (pos-cid-directory (expand-file-name "trial" archive))
                                        "/x.txt"))
                     (pos-links-resolve sealed)))
      (should (equal (cons 'cid (cdr (pos-links-resolve sealed)))
                     (pos-links-resolve sealed garden))))))

(ert-deftest pos-links/a-sealed-path-s-link-is-its-item-s-cid-and-the-path-within ()
  "A sealed path's link is ipfs://, the item's CID and the path within.
It is what a sealed item's link to the path is rewritten to, a
directory's path within included; a trailing slash or a relative path
names the same.  A path never sealed, and a path in no archive, are
refused."
  (pos-test-with-files dir `(,@pos-links-test-garden
                             ("garden/scope/trial/result.md" . "result\n")
                             ("garden/scope/trial/notes/deep.txt" . "deep\n"))
    (let* ((scope (expand-file-name "garden/scope/" dir))
           (archive (pos-links-test-seal scope "trial"))
           (cid (pos-cid-directory (expand-file-name "trial" archive))))
      (should (equal (concat "ipfs://" cid "/notes/deep.txt")
                     (pos-links-link (expand-file-name "trial/notes/deep.txt" archive))))
      (should (equal (concat "ipfs://" cid "/notes")
                     (pos-links-link (expand-file-name "trial/notes" archive))))
      (should (equal (concat "ipfs://" cid)
                     (pos-links-link (expand-file-name "trial/" archive))))
      (should (equal (concat "ipfs://" cid "/result.md")
                     (let ((default-directory archive))
                       (pos-links-link "trial/result.md"))))
      (should (equal (cdr (pos-links-resolve (expand-file-name "trial/result.md" archive)
                                             scope))
                     (pos-links-link (expand-file-name "trial/result.md" archive))))
      (should (equal 'unsealed
                     (cadr (should-error (pos-links-link (expand-file-name "other" archive))
                                         :type 'pos-ledger-refused))))
      (should (equal 'unsealed
                     (cadr (should-error (pos-links-link (expand-file-name "notes.org" scope))
                                         :type 'pos-ledger-refused)))))))

(provide 'pos-links-test)
;;; pos-links-test.el ends here
