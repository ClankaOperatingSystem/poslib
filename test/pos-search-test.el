;;; pos-search-test.el --- Tests for pos-search -*- lexical-binding: t; -*-

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

;; Search over an archive on disk: grep's behaviour behind the port, the
;; same answer pyposlib's adapter gives over the same items, and the
;; command at a shell.  The items and queries are those of pyposlib's
;; test_search.py, so the two libraries are held to one answer.

;;; Code:

(require 'ert)
(require 'pos-test-support)
(require 'pos-fixtures)
(require 'pos-search)
(require 'pos-index)

(defconst pos-search-test-items
  '(("first" ("a.txt" . "alpha\nbeta alpha\n") ("sub/b.txt" . "gamma") ("raw.bin" . "\377\376alpha"))
    ("second" ("c.txt" . "alpha"))
    ("third" ("x.txt" . "alpha") ("y.txt" . "alpha")))
  "The items pyposlib's search tests seal, each a name and its files.")

(defun pos-search-test-sealed (scope)
  "Seal `pos-search-test-items' into SCOPE's archive; return the archive."
  (dolist (item pos-search-test-items)
    (dolist (file (cdr item))
      (pos-test-write-bytes (expand-file-name (concat (car item) "/" (car file)) scope)
                            (cdr file)))
    (pos-test-approve (pos-seal-plan (expand-file-name (car item) scope)
                                     (expand-file-name (concat "archives/" (car item)) scope))))
  (file-truename (expand-file-name "archives" scope)))

(defun pos-search-test-shape (hits)
  "Return HITS as (REF FIRST LAST PASSAGE) each, for comparing."
  (mapcar (lambda (hit)
            (let ((lines (alist-get 'lines (alist-get 'range hit))))
              (list (alist-get 'ref hit) (aref lines 0) (aref lines 1) (alist-get 'passage hit))))
          hits))

(defmacro pos-search-test-with-archive (&rest body)
  "Evaluate BODY with `scope', `archive' and `cids' bound.
The scope holds the sealed items, and the CIDs are its fold."
  (declare (indent 0))
  `(pos-test-with-temp-dir dir
     (let* ((scope (expand-file-name "scope" dir))
            (_ (make-directory (expand-file-name "archives" scope) t))
            (archive (pos-search-test-sealed scope))
            (cids (pos-ledger-fold-cids archive)))
       (ignore scope archive cids)
       ,@body)))

(ert-deftest pos-search/hits-are-in-the-archive-s-order-named-by-their-items ()
  "A literal search finds each line, by path then line, each by its item's link.
A file that is not UTF-8 gives nothing."
  (pos-search-test-with-archive
    (let ((first (cdr (assoc "first" cids))) (second (cdr (assoc "second" cids)))
          (third (cdr (assoc "third" cids))))
      (should (equal `((,(concat "ipfs://" first "/a.txt") 1 1 "alpha")
                       (,(concat "ipfs://" first "/a.txt") 2 2 "beta alpha")
                       (,(concat "ipfs://" second "/c.txt") 1 1 "alpha")
                       (,(concat "ipfs://" third "/x.txt") 1 1 "alpha")
                       (,(concat "ipfs://" third "/y.txt") 1 1 "alpha"))
                     (pos-search-test-shape (pos-search-disk archive "alpha"))))
      (should (equal `((,(concat "ipfs://" first "/sub/b.txt") 1 1 "gamma"))
                     (pos-search-test-shape (pos-search-disk archive "gam"))))
      (should-not (pos-search-disk archive "zzz"))
      (should (equal 2 (length (pos-search-disk archive "alpha" nil 2)))))))

(ert-deftest pos-search/a-hit-is-what-fetch-gives-at-its-range ()
  "Each reference resolves through fetch, from a directory beneath the scope.
The passage is the lines of the range."
  (pos-search-test-with-archive
    (let ((canon (expand-file-name "canon" scope)))
      (make-directory canon)
      (let ((hits (pos-search-disk archive "alpha")))
        (should (equal 5 (length hits)))
        (dolist (hit hits)
          (let ((lines (pos-search-lines (pos-index-bytes (alist-get 'ref hit) canon)))
                (range (alist-get 'lines (alist-get 'range hit))))
            (should (equal (alist-get 'passage hit)
                           (string-join (seq-subseq lines (1- (aref range 0)) (aref range 1))
                                        "\n")))))
        (should-not (string-search "raw.bin" (mapconcat (lambda (h) (alist-get 'ref h)) hits)))))))

(ert-deftest pos-search/regex-is-posix-extended-as-grep-takes-it ()
  "A regex is matched within a line, with groups and alternatives written bare.
One that does not parse is request; another mode is mode."
  (pos-search-test-with-archive
    (should (equal '("beta alpha" "gamma")
                   (mapcar (lambda (h) (alist-get 'passage h))
                           (pos-search-disk archive "^[bg]" "regex"))))
    (should (equal 4 (length (pos-search-disk archive "^alpha$" "regex"))))
    (should (equal 6 (length (pos-search-disk archive "(al|ga)" "regex"))))
    (should (equal 1 (length (pos-search-disk archive "a{2,}|gam+a" "regex"))))
    (should (equal 2 (length (pos-search-disk archive "[[:space:]]alpha|gam[[:alpha:]]" "regex"))))
    (pos-test-refused pos-ledger-refused "request" (pos-search-disk archive "(" "regex"))
    (pos-test-refused pos-ledger-refused "mode" (pos-search-disk archive "alpha" "words"))
    (pos-test-refused pos-ledger-refused "request" (pos-search-disk archive ""))
    (pos-test-refused pos-ledger-refused "request" (pos-search-disk archive "alpha" nil 0))))

(ert-deftest pos-search/within-searches-beneath-one-cid ()
  "Within an item, a file or the root, only what is beneath is searched.
A CID the ledger does not enrol is absent."
  (pos-search-test-with-archive
    (should (equal 2 (length (pos-search-disk archive "alpha" nil nil (cdr (assoc "third" cids))))))
    (should (equal 2 (length (pos-search-disk archive "alpha" nil nil
                                              (cdr (assoc "first/a.txt" cids))))))
    (should (equal 4 (length (pos-search-disk archive "alpha" nil 4 (cdr (assoc "." cids))))))
    (pos-test-refused pos-ledger-refused "absent"
      (pos-search-disk archive "alpha" nil nil (pos-cid-bytes "never")))))

(ert-deftest pos-search/a-ledger-without-cids-is-hashed-from-disk ()
  "A schema 1 ledger enrols no CID: files are hashed as link hashes them.
Each hit names its file by its own CID, there being no item."
  (pos-fixture-with (pos-fixture "ledger" "seal-legacy-archive") dir
    (let ((archive (expand-file-name "scope/archives" dir)))
      (should (equal `((,(concat "ipfs://" (pos-cid-bytes "alpha")) 1 1 "alpha"))
                     (pos-search-test-shape (pos-search-disk archive "alpha")))))))

(cl-defstruct pos-search-test-keeper
  "A keeper that is not HTTP: the modes it declares and the hits it answers."
  modes hits asked)

(cl-defmethod pos-remote-describe ((keeper pos-search-test-keeper))
  "Return what KEEPER describes: search, where it declares modes."
  (if (pos-search-test-keeper-modes keeper)
      `((events . 1) (search . ((modes . ,(vconcat (pos-search-test-keeper-modes keeper))))))
    '((events . 1) (search . :null))))

(cl-defmethod pos-remote-search ((keeper pos-search-test-keeper) query &optional mode limit within)
  "Record what KEEPER was asked, QUERY MODE LIMIT and WITHIN; answer its hits."
  (push (list query mode limit within) (pos-search-test-keeper-asked keeper))
  (if (and within (not (equal within "bafkept")))
      (pos-ledger-refuse 'absent "The ledger enrols nothing under %s" within)
    `((hits . ,(vconcat (pos-search-test-keeper-hits keeper))))))

(defun pos-search-test-kept (dir)
  "Make DIR a scope with a kept archive; return the archive path.
Its ledger lies beside where the archive would be, and nothing is on disk."
  (make-directory (expand-file-name "archive-integrity/ledger" dir) t)
  (make-directory (expand-file-name ".pos" dir) t)
  (pos-test-write-bytes
   (expand-file-name ".pos/config.yaml" dir)
   (concat "pos: 2\narchives:\n  - scope: \".\"\n    kept: remote\n"
           "    ledger: 0f1e2d3c-4b5a-4968-8778-a6b5c4d3e2f1\n"
           "    url: \"https://keeper.example/ledgers/0f1e2d3c\"\n"))
  (expand-file-name "archives" dir))

(ert-deftest pos-search/a-scope-s-archives-are-searched-each-by-what-reaches-it ()
  "Under one root, each archive is searched by what reaches it.
The limit caps them together; a within one archive enrols searches
that one, and one none enrols is absent."
  (pos-test-with-temp-dir dir
    (let* ((kept (file-truename (pos-search-test-kept dir)))
           (other (expand-file-name "projects/b" dir))
           (hit '((ref . "ipfs://bafkept/result.txt") (range . ((lines . [1 1])))
                  (passage . "result")))
           (keeper (make-pos-search-test-keeper :modes '("literal") :hits (list hit)))
           (pos-remote-keeper-function (lambda (_url) keeper)))
      (make-directory (expand-file-name "archives" other) t)
      (pos-test-write-bytes (expand-file-name "note/n.txt" other) "a result\nno\nanother result\n")
      (pos-test-approve (pos-seal-plan (expand-file-name "note" other)
                                       (expand-file-name "archives/note" other)))
      (let* ((note (cdr (assoc "note" (pos-ledger-fold-cids (expand-file-name "archives" other)))))
             (found (pos-search-scope dir "result")))
        (should (equal `((,kept ("ipfs://bafkept/result.txt" 1 1 "result"))
                         (,(file-truename (expand-file-name "archives" other))
                          (,(concat "ipfs://" note "/n.txt") 1 1 "a result")
                          (,(concat "ipfs://" note "/n.txt") 3 3 "another result")))
                       (mapcar (lambda (a) (cons (car a) (pos-search-test-shape (cdr a)))) found)))
        (should (equal '(1 1) (mapcar (lambda (a) (length (cdr a)))
                                      (pos-search-scope dir "result" nil 2))))
        (should (equal (list (file-truename (expand-file-name "archives" other)))
                       (mapcar #'car (pos-search-scope dir "result" nil nil note))))
        (should (equal (list kept) (mapcar #'car (pos-search-scope dir "result" nil nil "bafkept"))))
        (pos-test-refused pos-ledger-refused "absent"
          (pos-search-scope dir "result" nil nil (pos-cid-bytes "x")))
        (should (equal '("result" nil nil nil) (car (last (pos-search-test-keeper-asked keeper)))))))))

(ert-deftest pos-search/a-keeper-that-does-not-search-is-named ()
  "A kept archive whose keeper declares no search is absent, naming both.
Before anything is searched, and a within does not hide it."
  (pos-test-with-temp-dir dir
    (let* ((kept (file-truename (pos-search-test-kept dir)))
           (keeper (make-pos-search-test-keeper :modes nil))
           (pos-remote-keeper-function (lambda (_url) keeper)))
      (dolist (within '(nil "bafkept"))
        (let ((message (cadr (cdr (should-error (pos-search-scope dir "result" nil nil within)
                                                :type 'pos-ledger-refused)))))
          (should (string-search kept message))
          (should (string-search "keeper.example" message))))
      (should-not (pos-search-test-keeper-asked keeper)))))

(ert-deftest pos-search/the-command-prints-grep-s-shape-with-a-citation ()
  "Each hit a line as LINK:LINE:TEXT; 0 with hits, 1 with none, nil for usage."
  (pos-search-test-with-archive
    (let ((first (cdr (assoc "first" cids))))
      (should (equal (concat "ipfs://" first "/a.txt:1:alpha\n"
                             "ipfs://" first "/a.txt:2:beta alpha\n")
                     (with-output-to-string
                       (should (equal 0 (pos-search-command
                                         (list scope "alpha" "--limit" "2")))))))
      (should (equal 1 (pos-search-command (list scope "zzz"))))
      (should (equal 1 (pos-search-command
                        (list scope "zeta" "--mode" "regex" "--within" first "--limit" "1"))))
      (should-not (pos-search-command (list scope)))
      (should-not (pos-search-command (list scope "alpha" "--limit")))
      (should-not (pos-search-command (list scope "alpha" "--other" "x")))
      (should-not (pos-search-command (list scope "alpha" "--mode" "regex" "--mode" "literal")))
      (pos-test-refused pos-ledger-refused "request"
        (pos-search-command (list scope "alpha" "--limit" "0"))))))

(provide 'pos-search-test)
;;; pos-search-test.el ends here
