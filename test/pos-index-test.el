;;; pos-index-test.el --- Tests for pos-index.el  -*- lexical-binding: t -*-

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

;; The CID index of a scope: built from sealed archives, rebuilt when
;; it is gone, and the ipfs: links it resolves, on disk or at a keeper.

;;; Code:

(require 'ert)
(require 'org)
(require 'pos-index)
(require 'pos-fixtures)

(defmacro pos-index-test-with-sealed (&rest body)
  "Evaluate BODY with `scope' holding two sealed items.
A sealed trial/ and a sealed note.md, with `trial-cid' and `note-cid'
their CIDs."
  (declare (indent 0))
  `(pos-test-with-scope
     (pos-test-write-bytes (expand-file-name "note.md" scope) "note")
     (dolist (item '("trial" "note.md"))
       (let ((plan (pos-seal-plan (expand-file-name item scope)
                                  (expand-file-name (concat "archives/" item) scope))))
         (pos-test-approve plan)))
     (let ((trial-cid (pos-cid-directory (expand-file-name "archives/trial" scope)))
           (note-cid (pos-cid-file (expand-file-name "archives/note.md" scope))))
       (ignore trial-cid note-cid)
       ,@body)))

(ert-deftest pos-index/a-cid-and-a-path-name-a-file-in-an-item ()
  "A link names a file by its item's CID and path, or by its own CID.
That is ipfs://ITEM-CID/PATH for a file within a sealed directory, and
ipfs://ITS-CID for a file sealed alone."
  (pos-index-test-with-sealed
    (should (equal (file-truename (expand-file-name "archives/trial/result.md" scope))
                   (pos-index-resolve scope (concat "ipfs://" trial-cid "/result.md"))))
    (should (equal (file-truename (expand-file-name "archives/note.md" scope))
                   (pos-index-resolve scope (concat "ipfs://" note-cid))))
    (should-error (pos-index-resolve scope "ipfs://bafkreiaaaa"))))

(ert-deftest pos-index/the-index-is-ephemera ()
  "A deleted index is rebuilt from the sealed archives when needed.
It comes back byte for byte as it was."
  (pos-index-test-with-sealed
    (pos-index-build scope)
    (let* ((file (expand-file-name pos-index-file scope))
           (saved (pos-ledger-read file)))
      (delete-directory (file-name-directory file) t)
      (should (pos-index-resolve scope (concat "ipfs://" note-cid)))
      (should (equal saved (pos-ledger-read file))))))

(ert-deftest pos-index/a-kept-archive-is-indexed-from-its-ledger ()
  "The index of a kept archive holds the CIDs its ledger folds to.
What a keeper keeps is not on disk to hash, so the ledger's CIDs stand
at the paths the files would have."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let* ((scope (expand-file-name "projects/a" (file-truename dir)))
           (cids (pos-ledger-fold-cids (expand-file-name "archives" scope)))
           (index (pos-index-build scope)))
      (should (equal ["archives/first/a.txt"]
                     (cdr (assoc (cdr (assoc "first/a.txt" cids)) index))))
      (should (equal ["archives"] (cdr (assoc (cdr (assoc "." cids)) index)))))))

(defmacro pos-index-test-with-kept (&rest body)
  "Evaluate BODY in a tree whose scope projects/a is with a keeper.
`scope' is the scope, `cids' what its ledger folds to, `url' its
keeper's URL, and `asked' collects the requests made, each answered
with the bytes \"result\"."
  (declare (indent 0))
  `(let ((fixture (pos-fixture "ledger" "report-kept-asked")))
     (pos-fixture-with fixture dir
       (let* ((scope (expand-file-name "projects/a" (file-truename dir)))
              (cids (pos-ledger-fold-cids (expand-file-name "archives" scope)))
              (url (alist-get 'url (alist-get 'keeper fixture)))
              (asked nil)
              (pos-remote-keeper-function
               (lambda (base) (pos-remote-http-create :url base :token "a-token")))
              (pos-remote-send-function
               (lambda (method url headers _body)
                 (push (list method url (cdr (assoc "Authorization" headers))) asked)
                 (cons 200 "result"))))
         (ignore cids url asked)
         ,@body))))

(ert-deftest pos-index/a-file-a-keeper-keeps-resolves-to-where-the-keeper-has-it ()
  "A file sealed to a keeper resolves to the keeper's URL for its CID.
By its item's CID and its path, or by its own CID.  A directory
resolves to nothing: the protocol reads files."
  (pos-index-test-with-kept
    (let ((there (concat url "/ipfs/" (cdr (assoc "trial/result.txt" cids)))))
      (should (equal there (pos-index-resolve
                            scope (concat "ipfs://" (cdr (assoc "trial" cids))
                                          "/result.txt"))))
      (should (equal there (pos-index-resolve
                            scope (concat "ipfs://" (cdr (assoc "trial/result.txt" cids))))))
      (should-error (pos-index-resolve scope (concat "ipfs://" (cdr (assoc "trial" cids)))))
      (should-not asked))))

(ert-deftest pos-index/org-follows-an-ipfs-link-to-its-keeper ()
  "A kept file's ipfs: link is fetched from its keeper and shown.
The request carries the caller's token, and the file is shown read-only
in the mode its name gives."
  (pos-index-test-with-kept
    (let ((canon (expand-file-name "notes.org" scope))
          (file-cid (cdr (assoc "trial/result.txt" cids))))
      (pos-test-write-bytes canon (concat "[[ipfs://" (cdr (assoc "trial" cids))
                                          "/result.txt][the result]]\n"))
      (with-current-buffer (find-file-noselect canon)
        (unwind-protect
            (progn
              (goto-char (point-min))
              (search-forward "ipfs:")
              (save-window-excursion
                (org-open-at-point)
                (should (equal "result" (buffer-string)))
                (should buffer-read-only)
                (should-not buffer-file-name)
                (should (derived-mode-p 'text-mode))
                (kill-buffer))
              (should (equal `(("GET" ,(concat url "/ipfs/" file-cid) "Bearer a-token"))
                             asked)))
          (kill-buffer))))))

(ert-deftest pos-index/a-link-s-bytes-are-read-from-disk ()
  "A link's bytes are read from the archive beneath the scope.
By its item's CID and its path or by its own CID, an Org search after
:: left aside.  A directory, a CID no archive has and text that is no
link are refused as absent."
  (pos-index-test-with-sealed
    (let ((default-directory (file-name-as-directory (expand-file-name "trial" scope))))
      (make-directory default-directory t)
      (should (equal "result" (pos-index-bytes (concat "ipfs://" trial-cid "/result.md"))))
      (should (equal "result" (pos-index-bytes
                               (concat "ipfs://" trial-cid "/result.md::*A heading"))))
      (should (equal "note" (pos-index-bytes (concat "ipfs://" note-cid))))
      (should (equal "note" (pos-index-bytes (concat "ipfs://" note-cid "::a search"))))
      (should (equal "note" (pos-index-bytes (concat "ipfs://" note-cid) scope)))
      (dolist (uri (list (concat "ipfs://" trial-cid) "ipfs://bafkreiaaaa"
                         (concat "ipfs://" trial-cid "/other.md") "result.md"))
        (should (equal 'absent
                       (cadr (should-error (pos-index-bytes uri)
                                           :type 'pos-ledger-refused))))))))

(ert-deftest pos-index/a-link-s-bytes-are-read-from-its-keeper ()
  "A kept file's bytes are read from the keeper by its ledger CID.
The request carries the caller's token, and asks for the CID the ledger
enrols the file under.  Bytes that are not that CID's are refused as
entry."
  (pos-index-test-with-kept
    (let ((file-cid (cdr (assoc "trial/result.txt" cids))))
      (should (equal "result" (pos-index-bytes
                               (concat "ipfs://" (cdr (assoc "trial" cids)) "/result.txt")
                               scope)))
      (should (equal `(("GET" ,(concat url "/ipfs/" file-cid) "Bearer a-token")) asked))
      (let ((pos-remote-send-function (lambda (&rest _) (cons 200 "another"))))
        (should (equal 'entry
                       (cadr (should-error (pos-index-bytes (concat "ipfs://" file-cid) scope)
                                           :type 'pos-ledger-refused))))))))

(ert-deftest pos-index/a-program-prints-a-link-s-bytes ()
  "The fetch command prints a link's bytes as they are.
Whatever they are; a link to no archived file exits 2 and prints
nothing."
  (pos-test-with-scope
    (let ((bytes (concat (apply #'unibyte-string (number-sequence 0 255)) "\r\n\303\251")))
      (let ((coding-system-for-write 'binary))
        (write-region bytes nil (expand-file-name "trial/bytes.bin" scope) nil 'silent))
      (let* ((archive (pos-test-scope-sealed scope))
             (cid (cdr (assoc "trial" (pos-ledger-fold-cids archive))))
             (default-directory (file-name-as-directory scope))
             (run (lambda (link)
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (let ((coding-system-for-read 'binary))
                        (list (call-process
                               (expand-file-name invocation-name invocation-directory)
                               nil '(t nil) nil "-Q" "--batch"
                               "-L" (file-name-directory
                                     (expand-file-name (locate-library "markdown-mode")))
                               "-L" (file-name-directory
                                     (expand-file-name (locate-library "yaml")))
                               "-L" (file-name-directory
                                     (expand-file-name (locate-library "pos-seal")))
                               "-l" "pos-seal" "-f" "pos-seal-batch" "fetch" link)
                              (buffer-string)))))))
        (should (equal (list 0 bytes) (funcall run (concat "ipfs://" cid "/bytes.bin"))))
        (should (equal '(2 "") (funcall run (concat "ipfs://" cid "/other.bin"))))))))

(ert-deftest pos-index/a-fetched-file-is-decoded-as-its-bytes-say ()
  "A kept file of bytes that are not text is shown undecoded.
As visiting it would show it: one character a byte, though some of its
bytes would read as UTF-8.  UTF-8 text is still text."
  (pos-index-test-with-kept
    (let ((uri (concat "ipfs://" (cdr (assoc "trial/result.txt" cids))))
          (bytes (concat (apply #'unibyte-string (number-sequence 0 255)) "\303\251\377")))
      (let ((pos-remote-send-function (lambda (&rest _) (cons 200 bytes))))
        (with-current-buffer (pos-index-fetch scope uri)
          (unwind-protect
              (progn
                (should (eq 'no-conversion buffer-file-coding-system))
                (should (= (length bytes) (buffer-size)))
                (should (equal bytes (encode-coding-string (buffer-string)
                                                           'no-conversion))))
            (kill-buffer))))
      (let ((pos-remote-send-function
             (lambda (&rest _) (cons 200 (encode-coding-string "café" 'utf-8)))))
        (with-current-buffer (pos-index-fetch scope uri)
          (unwind-protect
              (should (equal "café" (buffer-string)))
            (kill-buffer)))))))

(ert-deftest pos-index/org-follows-ipfs-links ()
  "An ipfs: link in canon opens the archived file it names."
  (pos-index-test-with-sealed
    (let ((canon (expand-file-name "notes.org" scope)))
      (pos-test-write-bytes canon (concat "[[ipfs://" trial-cid "/result.md][the result]]\n"))
      (with-current-buffer (find-file-noselect canon)
        (unwind-protect
            (progn
              (goto-char (point-min))
              (org-open-at-point)
              (should (equal (file-truename (expand-file-name "archives/trial/result.md" scope))
                             (file-truename buffer-file-name)))
              (kill-buffer))
          (kill-buffer (find-buffer-visiting canon)))))))

(ert-deftest pos-index/org-follows-an-ipfs-link-that-carries-a-search ()
  "An ipfs: link written with an Org search after :: opens the file."
  (pos-index-test-with-sealed
    (let ((canon (expand-file-name "notes.org" scope)))
      (pos-test-write-bytes canon (concat "[[ipfs://" trial-cid "/result.md::result][the result]]\n"))
      (with-current-buffer (find-file-noselect canon)
        (unwind-protect
            (progn
              (goto-char (point-min))
              (org-open-at-point)
              (should (equal (file-truename (expand-file-name "archives/trial/result.md" scope))
                             (file-truename buffer-file-name)))
              (kill-buffer))
          (kill-buffer (find-buffer-visiting canon)))))))

(provide 'pos-index-test)
;;; pos-index-test.el ends here
