;;; pos-wrap-up-test.el --- Tests for pos-wrap-up.el  -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'pos-wrap-up)
(require 'pos-test-support)

(defmacro pos-wrap-up-test-with-node (root &rest body)
  "Evaluate BODY with ROOT a node with an empty archives/ and no journal."
  (declare (indent 1))
  `(pos-test-with-temp-dir dir
     (let ((,root (file-name-as-directory (file-truename dir))))
       (make-directory (expand-file-name "archives" ,root))
       ,@body)))

(defun pos-wrap-up-test-journal (root)
  "Return the text of ROOT's journal.org."
  (pos-test-file-string (expand-file-name "journal.org" root)))

(ert-deftest pos-wrap-up/a-title-gives-a-file-name ()
  "A slug is the title in lower case, other characters made one hyphen each run.
None is left at either end, and a title with no letter or digit gives
handover."
  (should (equal "the-roof-mended" (pos-wrap-up-slug "The roof mended")))
  (should (equal "pos-capture-exits-1" (pos-wrap-up-slug " =pos-capture= exits 1! ")))
  (should (equal "handover" (pos-wrap-up-slug "..."))))

(ert-deftest pos-wrap-up/seals-the-handover-and-indexes-it-as-the-latest ()
  "The handover is sealed as journal/DATE-SLUG.org in the node's archive.
Its heading leads the node's journal.org, made for it with a file ID,
tagged handover and latest, with an ID, its date as an inactive
timestamp and the record's citation, which is what is returned."
  (pos-wrap-up-test-with-node root
    (let ((citation (pos-wrap-up root "The roof mended" "What happened.\n" "2026-10-14")))
      (should (string-match-p "\\`ipfs://bafk[a-z2-7]+\\'" citation))
      (should (equal "What happened.\n"
                     (pos-test-text root "archives/journal/2026-10-14-the-roof-mended.org")))
      (should (string-match-p
               (concat "\\`:PROPERTIES:\n:ID: +[0-9a-f-]\\{36\\}\n:END:\n"
                       "#\\+TITLE: Journal\n\n"
                       "\\* The roof mended +:handover:latest:\n"
                       ":PROPERTIES:\n"
                       ":ID: +[0-9a-f-]\\{36\\}\n"
                       ":DATE: +\\[2026-10-14 Wed\\]\n"
                       ":RECORD: +" (regexp-quote citation) "\n"
                       ":END:\n\\'")
               (pos-wrap-up-test-journal root))))))

(ert-deftest pos-wrap-up/the-latest-tag-moves-to-the-new-handover ()
  "A second handover leads the journal and takes the latest tag.
The first keeps its handover tag and its other properties."
  (pos-wrap-up-test-with-node root
    (pos-wrap-up root "The roof mended" "First.\n" "2026-10-14")
    (pos-wrap-up root "The gutter cleared" "Second.\n" "2026-10-15")
    (let ((journal (pos-wrap-up-test-journal root)))
      (should (string-match-p "\\* The gutter cleared +:handover:latest:\n" journal))
      (should (string-match-p "\\* The roof mended +:handover:\n" journal))
      (should (< (string-match "The gutter cleared" journal)
                 (string-match "The roof mended" journal)))
      (should (string-match-p ":DATE: +\\[2026-10-14 Wed\\]" journal)))
    (should (equal "Second.\n"
                   (pos-test-text root "archives/journal/2026-10-15-the-gutter-cleared.org")))))

(ert-deftest pos-wrap-up/a-journal-that-is-there-keeps-what-it-has ()
  "The heading goes before the journal's first heading; its preamble stays."
  (pos-wrap-up-test-with-node root
    (pos-test-write-files root '(("journal.org"
                                  . "#+TITLE: Our journal\nRead the latest.\n* Older :handover:latest:\n")))
    (pos-wrap-up root "Newer" "Body.\n" "2026-10-14")
    (should (string-match-p
             (concat "\\`#\\+TITLE: Our journal\nRead the latest.\n"
                     "\\* Newer +:handover:latest:\n:PROPERTIES:\n\\(?:.*\n\\)+?:END:\n"
                     "\\* Older +:handover:\n\\'")
             (pos-wrap-up-test-journal root)))))

(ert-deftest pos-wrap-up/refuses-and-writes-nothing ()
  "A blank title, an empty body or a date not of the form is refused.
Nothing is sealed and no journal is made."
  (pos-wrap-up-test-with-node root
    (should-error (pos-wrap-up root "  " "Body.\n" "2026-10-14") :type 'user-error)
    (should-error (pos-wrap-up root "Title" "  \n" "2026-10-14") :type 'user-error)
    (should-error (pos-wrap-up root "Title" "Body.\n" "14/10/2026") :type 'user-error)
    (should-error (pos-wrap-up root "Title" "Body.\n" "2026-13-40") :type 'user-error)
    (should-not (file-exists-p (expand-file-name "journal.org" root)))
    (should-not (file-exists-p (expand-file-name "archives/journal" root)))))

(ert-deftest pos-wrap-up/the-command-prints-the-citation-the-title-and-the-date ()
  "The command keeps the handover read from its input and prints one line.
A refusal exits 1 and prints nothing on its output."
  (pos-wrap-up-test-with-node root
    (let ((input (expand-file-name "handover.org" root))
          (process-environment
           (append (list (concat "POS_WRAP_UP_ROOT=" root)
                         "POS_WRAP_UP_TITLE=The roof mended"
                         "POS_WRAP_UP_DATE=2026-10-14")
                   process-environment))
          (status nil))
      (pos-test-write-files root '(("handover.org" . "What happened.\n")))
      (let ((pos-wrap-up-input input))
        (should (string-match-p
                 "\\`ipfs://bafk[a-z2-7]+: The roof mended, 2026-10-14\n\\'"
                 (with-output-to-string (pos-wrap-up-batch)))))
      (setenv "POS_WRAP_UP_TITLE" "")
      (cl-letf (((symbol-function 'kill-emacs)
                 (lambda (&optional code) (throw 'pos-wrap-up-test-exit code))))
        (let ((pos-wrap-up-input input))
          (should (equal "" (with-output-to-string
                              (setq status (catch 'pos-wrap-up-test-exit
                                             (pos-wrap-up-batch)
                                             :returned)))))))
      (should (eql 1 status)))))

(provide 'pos-wrap-up-test)
;;; pos-wrap-up-test.el ends here
