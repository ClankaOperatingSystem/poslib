;;; pos-seal-test.el --- Tests for pos-seal.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The seal fixtures in fixtures/ledger/ are shared
;; with pyposlib, which must seal the same bytes.

;;; Code:

(require 'ert)
(require 'pos-seal)
(require 'pos-fixtures)

(defun pos-seal-test-relative-plan (plan dir)
  "Return PLAN with its absolute paths made relative to DIR."
  (let ((root (file-name-as-directory (file-truename dir))))
    (mapcar (lambda (pair)
              (if (memq (car pair) '(source destination archive ledger))
                  (cons (car pair) (file-relative-name (cdr pair) root))
                pair))
            plan)))

(defun pos-seal-test-run (fixture dir)
  "Seal FIXTURE, built in DIR; return what happened, relative to DIR.
An alist of plan, event and report, or of error.  A fixture with a
keeper is sealed to its recording, with the claims it gives; the
recording must be played out, and the item gone from where it lay."
  (let-alist fixture
    (let ((pos-seal-claims-function
           (if .keeper (lambda (&rest _) .claims) pos-seal-claims-function)))
      (pos-test-with-keeper .keeper nil
        (let ((got (let ((pos-ledger-offline t)) (pos-seal-test-run-1 fixture dir))))
          (when (and .keeper (not (alist-get 'error got))
                     (file-exists-p (expand-file-name .source dir)))
            (error "The item was left where it lay"))
          got)))))

(defun pos-seal-test-run-1 (fixture dir)
  "Seal FIXTURE, built in DIR, as `pos-seal-test-run' with its keeper bound."
  (let-alist fixture
    (condition-case err
        (let* ((plan (pos-seal-plan (expand-file-name .source dir)
                                    (expand-file-name .destination dir) .ledger_id
                                    (or .date "2026-09-28")))
               (result (pos-test-approve plan))
               (scope (file-name-directory (alist-get 'archive plan))))
          `((plan . ,(pos-seal-test-relative-plan plan dir))
            (event . ((name . ,(file-name-nondirectory (car result)))
                      (encoded . ,(decode-coding-string (pos-ledger-read (car result))
                                                        'utf-8))))
            ;; The recording is of the seal: the report after it asks no keeper.
            (report . ,(let ((pos-ledger-offline t))
                         (pos-test-report-relative (pos-ledger-check scope) dir)))))
      (pos-ledger-refused `((error . ,(symbol-name (cadr err))))))))

(ert-deftest pos-seal/every-shared-fixture-seals-the-same-bytes ()
  "Plan, event and the report after it, or the refusal, as fixtures/ledger/."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "seal")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let ((got (pos-seal-test-run (cdr named) dir)))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (pos-test-same-json .plan (alist-get 'plan got))
                (pos-test-same-json .event (alist-get 'event got))
                (pos-test-same-json .report (alist-get 'report got))))))))))

(ert-deftest pos-seal/every-shared-fixture-converts-the-same-bytes ()
  "Each convert fixture in fixtures/ledger/ converts as recorded.
The conversion events, what was skipped and the report after, or the
refusal."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "convert")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let* ((root (expand-file-name .root dir))
                   (base (file-name-as-directory (file-truename dir)))
                   (got (condition-case err
                            (pos-seal-convert root)
                          (pos-ledger-refused
                           `((error . ,(symbol-name (cadr err))))))))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (should-not (alist-get 'error got))
                (pos-test-same-json
                 .converted
                 (vconcat
                  (mapcar (lambda (c)
                            (let ((file (alist-get 'event c)))
                              `((name . ,(file-name-nondirectory file))
                                (encoded . ,(decode-coding-string
                                             (pos-ledger-read file) 'utf-8)))))
                          (alist-get 'converted got))))
                (pos-test-same-json
                 .skipped
                 (vconcat
                  (mapcar (lambda (s)
                            `((archive . ,(file-relative-name (alist-get 'archive s) base))
                              (reason . ,(alist-get 'reason s))))
                          (alist-get 'skipped got))))
                (pos-test-same-json
                 .report (pos-test-report-relative (pos-ledger-check root) dir))))))))))

(ert-deftest pos-seal/every-shared-fixture-keeps-the-same-way ()
  "Each keep fixture in fixtures/ledger/ keeps as recorded.
What was moved to a keeper, what was left with its reason and the
report after, or the refusal: each request as its keeper recorded it,
and a kept archive gone from disk."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "keep")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let* ((root (expand-file-name .root dir))
                   (base (file-name-as-directory (file-truename dir)))
                   (relative (lambda (items)
                               (vconcat
                                (mapcar (lambda (item)
                                          (cons (cons 'archive
                                                      (file-relative-name
                                                       (alist-get 'archive item) base))
                                                (assq-delete-all 'archive
                                                                 (copy-alist item))))
                                        items))))
                   (pos-seal-claims-function (lambda (&rest _) .claims))
                   (got (pos-test-with-keeper .keeper nil
                          (condition-case err
                              (pos-seal-keep root)
                            (pos-ledger-refused
                             `((error . ,(symbol-name (cadr err)))))))))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (should-not (alist-get 'error got))
                (pos-test-same-json .kept (funcall relative (alist-get 'kept got)))
                (pos-test-same-json .skipped (funcall relative (alist-get 'skipped got)))
                (seq-doseq (item (alist-get 'kept got))
                  (should-not (file-exists-p (alist-get 'archive item))))
                (pos-test-same-json
                 .report (pos-test-report-relative (pos-ledger-check root) dir))))))))))

(ert-deftest pos-seal/every-shared-fixture-recalls-the-same-way ()
  "Each recall fixture in fixtures/ledger/ recalls as recorded.
What was brought back from a keeper, what was left with its reason and
the archive on disk after, or the refusal: each request as its keeper
recorded it."
  (dolist (named (pos-fixtures "ledger"))
    (let-alist (cdr named)
      (when (equal .kind "recall")
        (ert-info ((car named))
          (pos-fixture-with (cdr named) dir
            (let* ((root (expand-file-name .root dir))
                   (base (file-name-as-directory (file-truename dir)))
                   (relative (lambda (items)
                               (vconcat
                                (mapcar (lambda (item)
                                          (cons (cons 'archive
                                                      (file-relative-name
                                                       (alist-get 'archive item) base))
                                                (assq-delete-all 'archive
                                                                 (copy-alist item))))
                                        items))))
                   (got (pos-test-with-keeper .keeper nil
                          (condition-case err
                              (pos-seal-recall root)
                            (pos-ledger-refused
                             `((error . ,(symbol-name (cadr err)))))))))
              (if .error
                  (should (equal .error (alist-get 'error got)))
                (should-not (alist-get 'error got))
                (pos-test-same-json .recalled (funcall relative (alist-get 'recalled got)))
                (pos-test-same-json .skipped (funcall relative (alist-get 'skipped got)))
                (let (after)
                  (named-let walk ((here (expand-file-name "archives" (file-truename root))))
                    (let ((names (directory-files here nil directory-files-no-dot-files-regexp)))
                      (unless names
                        (push (list (file-relative-name here base) 'directory) after))
                      (dolist (name names)
                        (let ((path (expand-file-name name here)))
                          (if (file-directory-p path)
                              (walk path)
                            (push (list (file-relative-name path base)
                                        (decode-coding-string (pos-ledger-read path) 'utf-8)
                                        (logand (file-modes path) #o777))
                                  after))))))
                  (should (equal (mapcar (lambda (entry)
                                           (let-alist entry
                                             (if .directory
                                                 (list .path 'directory)
                                               (list .path .text .mode))))
                                         .after)
                                 (sort after (lambda (a b) (string< (car a) (car b)))))))))))))))

(ert-deftest pos-seal/sealing-removes-write-bits ()
  "The sealed files and the ledger event are read-only; CIDs do not change."
  (pos-test-with-scope
    (let* ((plan (pos-test-scope-plan scope))
           (event (car (pos-test-approve plan)))
           (file (expand-file-name "archives/trial/result.md" scope)))
      (should (zerop (logand (file-modes file) #o222)))
      (should (equal #o444 (logand (file-modes event) #o777)))
      (should (equal (pos-cid-file file)
                     (alist-get 'cid (cdr (assoc "trial/result.md" (alist-get 'add plan)))))))))

(ert-deftest pos-seal/the-ledger-alone-gives-the-archive-s-cids ()
  "After a seal, the ledger's entries give every CID the archive has.
Root included, without reading the archive."
  (pos-test-with-scope
    (let* ((plan (pos-test-scope-plan scope))
           (root (cdr (pos-test-approve plan)))
           (archive (expand-file-name "archives" scope))
           (by-path (lambda (cids)
                      (sort (copy-sequence cids)
                            (lambda (a b) (string< (car a) (car b))))))
           (folded (pos-ledger-fold-cids archive)))
      (should (equal (funcall by-path (pos-cid-tree archive))
                     (funcall by-path folded)))
      (should (equal root (cdr (assoc "." folded)))))))

(ert-deftest pos-seal/a-plan-is-applied-only-as-reviewed ()
  "A plan that is not the one reviewed is refused, and nothing moves.
One whose hash differs from the one reviewed, or whose item changed
after review."
  (pos-test-with-scope
    (let ((plan (pos-test-scope-plan scope)))
      (should-error (pos-seal-apply plan (make-string 64 ?0)) :type 'pos-ledger-refused)
      (pos-test-write-bytes (expand-file-name "trial/result.md" scope) "changed")
      (should-error (pos-test-approve plan)
                    :type 'pos-ledger-refused)
      (should (file-exists-p (expand-file-name "trial/result.md" scope)))
      (should-not (file-exists-p (expand-file-name "archives/trial" scope))))))

(ert-deftest pos-seal/an-interrupted-seal-resumes ()
  "After the move but before the event, the same plan finishes the work;
applied twice, it adds nothing."
  (pos-test-with-scope
    (let* ((plan (pos-test-scope-plan scope))
           (hash (pos-ledger-sha (pos-ledger-json plan))))
      (make-directory (expand-file-name "archives" scope) t)
      (rename-file (expand-file-name "trial" scope) (expand-file-name "archives/trial" scope))
      (let ((first (pos-seal-apply plan hash)))
        (should (equal first (pos-seal-apply plan hash))))
      (should (equal 1 (nth 2 (pos-ledger-history (expand-file-name "archives" scope))))))))

(ert-deftest pos-seal/a-new-record-is-staged-then-sealed ()
  "New bytes are staged beside the archive, then sealed like any item."
  (pos-test-with-scope
    (let* ((plan (pos-seal-stage "handover\n" (expand-file-name "archives/journal/h.md" scope)))
           (staged (alist-get 'source plan)))
      (should (string-prefix-p (file-truename (expand-file-name "_seal/" scope)) staged))
      (pos-test-approve plan)
      (should-not (file-exists-p staged))
      (should (equal "handover\n" (pos-ledger-read (expand-file-name "archives/journal/h.md"
                                                                     scope)))))))

(ert-deftest pos-seal/a-new-record-staged-again-replaces-the-first ()
  "Staging for one destination twice leaves one staged file, the second's.
The first plan is refused, the second seals, and _seal is gone."
  (pos-test-with-scope
    (let* ((target (expand-file-name "archives/journal/h.md" scope))
           (first (pos-seal-stage "draft\n" target))
           (second (pos-seal-stage "handover\n" target)))
      (should (equal (alist-get 'source first) (alist-get 'source second)))
      (should (equal 1 (length (directory-files (expand-file-name "_seal" scope) nil
                                                "\\`new-"))))
      (should (equal 'plan
                     (cadr (should-error
                            (pos-seal-apply first (pos-ledger-sha (pos-ledger-json first)))
                            :type 'pos-ledger-refused))))
      (pos-test-approve second)
      (should (equal "handover\n" (pos-ledger-read target)))
      (should-not (file-exists-p (expand-file-name "_seal" scope))))))

(ert-deftest pos-seal/a-new-record-may-start-an-archive ()
  "A scope with no archives/ yet has an empty one; sealing makes it."
  (pos-test-with-scope
    (delete-directory (expand-file-name "archives" scope))
    (let ((plan (pos-seal-stage "first\n" (expand-file-name "archives/first.txt" scope))))
      (should (equal 1 (alist-get 'number plan)))
      (pos-test-approve plan)
      (should (equal "first\n" (pos-ledger-read (expand-file-name "archives/first.txt"
                                                                  scope))))
      (should (file-directory-p (expand-file-name "archive-integrity/ledger" scope))))))

(ert-deftest pos-seal/a-refused-new-record-leaves-nothing-staged ()
  "When planning fails, the staged bytes and an emptied _seal/ go."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "archives/taken.txt" scope) "taken")
    (should-error (pos-seal-stage "new\n" (expand-file-name "archives/taken.txt" scope))
                  :type 'pos-ledger-refused)
    (should-not (file-exists-p (expand-file-name "_seal" scope)))))

(ert-deftest pos-seal/a-new-record-s-links-are-resolved ()
  "A new record's links are found, and read as written from its destination."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "notes.org" scope) "#+TITLE: Notes\n")
    (let* ((plan (pos-seal-stage "See [notes](../../notes.org).\n"
                                 (expand-file-name "archives/journal/h.md" scope)))
           (links (alist-get 'links plan)))
      (should (string-suffix-p ".md" (alist-get 'source plan)))
      (should (equal 1 (length links)))
      (should (equal "rumour" (alist-get 'kind (aref links 0))))
      (should (string-match-p "Rumour of notes.org"
                              (alist-get 'text (aref (alist-get 'rumours plan) 0)))))))

(ert-deftest pos-seal/a-new-records-links-may-be-read-from-elsewhere ()
  "A new record's links are read from the directory it was composed in.
A program that writes a record in the project's root, linking to
notes.org beside it, seals it with links-from the root; the link is a
rumour of notes.org, as it would be if written from the destination.
A directory that is not there is refused, and nothing is left staged."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "notes.org" scope) "#+TITLE: Notes\n")
    (let* ((plan (pos-seal-stage "See [notes](notes.org).\n"
                                 (expand-file-name "archives/journal/h.md" scope)
                                 nil scope))
           (links (alist-get 'links plan)))
      (should (equal "rumour" (alist-get 'kind (aref links 0))))
      (should (string-match-p "Rumour of notes.org"
                              (alist-get 'text (aref (alist-get 'rumours plan) 0)))))
    (let ((staged (directory-files (expand-file-name "_seal" scope) nil "\\`new-")))
      (should-error (pos-seal-stage "new\n" (expand-file-name "archives/journal/i.md" scope)
                                    nil (expand-file-name "nowhere" scope))
                    :type 'pos-ledger-refused)
      (should (equal staged
                     (directory-files (expand-file-name "_seal" scope) nil "\\`new-"))))))

(ert-deftest pos-seal/two-items-may-rumour-one-target-on-one-day ()
  "Each item's rumour of a target names that item, so is its own record;
a rumour already sealed word for word is cited, not sealed again."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "notes.org" scope) "#+TITLE: Notes\n")
    (let ((seal (lambda (name)
                  (pos-test-write-bytes (expand-file-name (concat name "/r.md") scope)
                                        "See [notes](../notes.org).\n")
                  (let ((plan (pos-seal-plan (expand-file-name name scope)
                                             (expand-file-name (concat "archives/" name) scope)
                                             nil "2026-09-28")))
                    (pos-test-approve plan)
                    (alist-get 'destination (aref (alist-get 'rumours plan) 0))))))
      (should-not (equal (funcall seal "first") (funcall seal "second")))
      (should (equal 4 (nth 2 (pos-ledger-history (expand-file-name "archives" scope))))))))

(ert-deftest pos-seal/a-program-applies-its-own-plan-explicitly ()
  "The write-new command with --apply seals at once; without it, a plan."
  (pos-test-with-scope
    (let* ((target (expand-file-name "archives/journal/h.md" scope))
           (run (lambda (&rest args)
                  (with-temp-buffer
                    (insert "handover\n")
                    (list (apply #'call-process-region (point-min) (point-max)
                                 (expand-file-name invocation-name invocation-directory)
                                 t t nil "-Q" "--batch"
                                 "-L" (file-name-directory
                                       (expand-file-name (locate-library "markdown-mode")))
                                 "-L"
                                 (file-name-directory (expand-file-name (locate-library "pos-seal")))
                                 "-l" "pos-seal" "-f" "pos-seal-batch" "write-new" target args)
                          (buffer-string))))))
      (let ((planned (funcall run)))
        (ert-info ((cadr planned)) (should (equal 0 (car planned)))))
      (should-not (file-exists-p target))
      (let ((applied (funcall run "--apply")))
        (ert-info ((cadr applied)) (should (equal 0 (car applied)))))
      (should (equal "handover\n" (pos-ledger-read target)))
      (should-not (file-exists-p (expand-file-name "_seal" scope))))))

(ert-deftest pos-seal/a-program-seals-an-item-explicitly ()
  "The seal command with --apply moves and seals at once; without it, a plan."
  (pos-test-with-scope
    (let* ((source (expand-file-name "trial" scope))
           (target (expand-file-name "archives/trial" scope))
           (run (lambda (&rest args)
                  (apply #'call-process (expand-file-name invocation-name invocation-directory)
                         nil nil nil "-Q" "--batch"
                         "-L" (file-name-directory
                               (expand-file-name (locate-library "markdown-mode")))
                         "-L" (file-name-directory (expand-file-name (locate-library "pos-seal")))
                         "-l" "pos-seal" "-f" "pos-seal-batch" "seal" source target args))))
      (should (equal 0 (funcall run)))
      (should-not (file-exists-p target))
      (should (equal 0 (funcall run "--apply")))
      (should (file-exists-p (expand-file-name "result.md" target)))
      (should (equal 1 (nth 2 (pos-ledger-history (expand-file-name "archives" scope))))))))

(ert-deftest pos-seal/a-staging-directory-left-empty-is-not-sealed ()
  "An empty _seal in an item is removed and no event records it.
At any depth; one that holds something is sealed as it is, and another
empty directory is recorded.  The check after the seal is clean."
  (pos-test-with-scope
    (dolist (dir '("trial/_seal" "trial/child/_seal" "trial/kept/_seal" "trial/hollow"))
      (make-directory (expand-file-name dir scope) t))
    (pos-test-write-bytes (expand-file-name "trial/kept/_seal/plan.json" scope) "{}")
    (let* ((archive (pos-test-scope-sealed scope))
           (event (pos-ledger-parse
                   (pos-ledger-read (car (last (nth 3 (pos-ledger-history archive))))))))
      (should (equal ["trial/child" "trial/hollow"] (alist-get 'empty event)))
      (should-not (file-exists-p (expand-file-name "trial/_seal" archive)))
      (should-not (file-exists-p (expand-file-name "trial/child/_seal" archive)))
      (should (file-exists-p (expand-file-name "trial/kept/_seal/plan.json" archive)))
      (should-not (pos-seal-findings-p (pos-ledger-check scope))))))

(ert-deftest pos-seal/a-new-record-leaves-no-staging-directory ()
  "A record staged by write-new is sealed and _seal, left empty, is gone;
a _seal that holds something else stays."
  (pos-test-with-scope
    (let ((seal (lambda (name)
                  (let ((plan (pos-seal-stage "record\n" (expand-file-name
                                                          (concat "archives/" name) scope))))
                    (pos-test-approve plan)))))
      (funcall seal "first.txt")
      (should-not (file-exists-p (expand-file-name "_seal" scope)))
      (pos-test-write-bytes (expand-file-name "_seal/other.json" scope) "{}")
      (funcall seal "second.txt")
      (should (equal '("other.json") (pos-ledger--entries (expand-file-name "_seal" scope)))))))

(ert-deftest pos-seal/a-sealed-path-has-a-link ()
  "A sealed item's link is ipfs:// and its CID; a path within adds it.
A collection's member included.  A path never sealed, and a path in no
archive, are refused."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "trial/README.org" scope)
                          "#+TITLE: Trial\n#+COLLECTION: t\n")
    (let* ((archive (pos-test-scope-sealed scope))
           (cid (cdr (assoc "trial" (pos-ledger-fold-cids archive)))))
      (should (equal (concat "ipfs://" cid)
                     (pos-links-link (expand-file-name "trial" archive))))
      (should (equal (concat "ipfs://" cid)
                     (pos-links-link (expand-file-name "trial/" archive))))
      (should (equal (concat "ipfs://" cid "/result.md")
                     (pos-links-link (expand-file-name "trial/result.md" archive))))
      (should (equal 'unsealed
                     (cadr (should-error (pos-links-link (expand-file-name "other" archive))
                                         :type 'pos-ledger-refused))))
      (should (equal 'unsealed
                     (cadr (should-error (pos-links-link (expand-file-name "trial" scope))
                                         :type 'pos-ledger-refused)))))))

(ert-deftest pos-seal/a-program-prints-a-sealed-path-s-link ()
  "The link command prints the link on one line; a path never sealed exits 2."
  (pos-test-with-scope
    (let* ((archive (pos-test-scope-sealed scope))
           (run (lambda (path)
                  (with-temp-buffer
                    (list (call-process (expand-file-name invocation-name invocation-directory)
                                        nil '(t nil) nil "-Q" "--batch"
                                        "-L" (file-name-directory
                                              (expand-file-name (locate-library "markdown-mode")))
                                        "-L" (file-name-directory
                                              (expand-file-name (locate-library "pos-seal")))
                                        "-l" "pos-seal" "-f" "pos-seal-batch" "link" path)
                          (buffer-string))))))
      (should (equal (list 0 (concat (pos-links-link (expand-file-name "trial/result.md" archive))
                                     "\n"))
                     (funcall run (expand-file-name "trial/result.md" archive))))
      (should (equal '(2 "") (funcall run (expand-file-name "other" archive)))))))

(ert-deftest pos-seal/reading-links-reads-nothing-else ()
  "A #+SETUPFILE in an item is not followed while its links are read."
  (pos-test-with-scope
    (let ((outside (expand-file-name "../outside.setup" scope)) read)
      (pos-test-write-bytes outside "#+TITLE: never read\n")
      (pos-test-write-bytes (expand-file-name "trial/r.org" scope)
                            (concat "#+SETUPFILE: " outside "\n\nText.\n"))
      (advice-add 'insert-file-contents :before
                  (lambda (file &rest _) (when (equal (expand-file-name file) outside)
                                           (setq read t)))
                  '((name . pos-seal-test-watch)))
      (unwind-protect (pos-test-scope-plan scope)
        (advice-remove 'insert-file-contents 'pos-seal-test-watch))
      (should-not read))))

(ert-deftest pos-seal/a-rumour-reads-nothing-outside-the-garden ()
  "A link out of the garden's repository gets a rumour, and is not read.
The rumour names where it pointed; the target is not read."
  (pos-test-with-scope
    (let ((outside (expand-file-name "../elsewhere.md" scope)) read)
      (make-directory (expand-file-name ".git" scope))
      (pos-test-write-bytes outside "# Somewhere else\n")
      (pos-test-write-bytes (expand-file-name "trial/r.md" scope) "See [there](../../elsewhere.md).\n")
      (advice-add 'insert-file-contents-literally :before
                  (lambda (file &rest _) (when (equal (expand-file-name file) outside)
                                           (setq read t)))
                  '((name . pos-seal-test-watch)))
      (let ((plan (unwind-protect (pos-test-scope-plan scope)
                    (advice-remove 'insert-file-contents-literally 'pos-seal-test-watch))))
        (should-not read)
        (should (string-match-p "outside the garden, and was not read"
                                (alist-get 'text (aref (alist-get 'rumours plan) 0))))))))

;;;; Checkpoints and repair

(ert-deftest pos-seal/a-checkpoint-waits-for-a-clean-check ()
  "An unregistered file refuses a checkpoint; gone, the heads are recorded.
Beside the scope; and recording them again is no conflict."
  (pos-test-with-scope
    (let ((archive (pos-test-scope-sealed scope)))
      (pos-test-write-bytes (expand-file-name "stray.txt" archive) "stray" #o444)
      (should-error (pos-seal-checkpoint scope) :type 'pos-ledger-refused)
      (delete-file (expand-file-name "stray.txt" archive))
      (let* ((home (pos-seal-checkpoint scope))
             (head (nth 1 (pos-ledger-history archive)))
             (bytes (pos-ledger-json `((schema . 1) (heads . [,head]) (coverage . "tree"))))
             (file (expand-file-name (concat (pos-ledger-sha bytes) ".json") home)))
        (should (equal home (expand-file-name "archive-integrity/checkpoints"
                                              (file-truename scope))))
        (should (equal bytes (pos-ledger-read file)))
        (should (equal home (pos-seal-checkpoint scope)))
        (should-not (pos-seal-findings-p (pos-ledger-check scope)))))))

(ert-deftest pos-seal/an-archive-s-checkpoint-covers-the-archive ()
  "Checkpointing an archive records its head as sealing recorded it.
With archive coverage, the same bytes, so no second file appears."
  (pos-test-with-scope
    (let* ((archive (pos-test-scope-sealed scope))
           (home (expand-file-name "archive-integrity/checkpoints" (file-truename scope)))
           (before (directory-files home nil "\\.json\\'")))
      (should (equal home (pos-seal-checkpoint archive)))
      (should (equal before (directory-files home nil "\\.json\\'"))))))

(ert-deftest pos-seal/repair-protects-only-verified-evidence ()
  "Repair removes a restored write bit, and refuses changed bytes.
A write bit restored to a sealed file is removed; once a sealed file's
bytes have changed, repair refuses."
  (pos-test-with-scope
    (let* ((archive (pos-test-scope-sealed scope))
           (file (expand-file-name "trial/result.md" archive)))
      (set-file-modes file #o644)
      (should (equal '((repaired . 1) (restored . 0) (unregistered . 0)) (pos-seal-repair scope)))
      (should (zerop (logand (file-modes file) #o222)))
      (set-file-modes file #o644)
      (pos-test-write-bytes file "changed" #o644)
      (should-error (pos-seal-repair scope) :type 'pos-ledger-refused)
      (should (/= 0 (logand (file-modes file) #o222))))))

(ert-deftest pos-seal/retirement-preserves-nested-archives ()
  "Staging cleanup must not change the CID of an archive inside the item."
  (pos-test-with-scope
    (let ((archive (expand-file-name "trial/archives" scope)))
      (make-directory (expand-file-name "old/_seal" archive) t)
      (pos-test-write-bytes (expand-file-name "old/record.txt" archive) "recorded")
      (let ((before (pos-cid-directory archive)))
        (pos-test-scope-sealed scope)
        (should (equal before (pos-cid-directory
                               (expand-file-name "archives/trial/archives" scope))))
        (should (file-directory-p
                 (expand-file-name "archives/trial/archives/old/_seal" scope)))))))

(ert-deftest pos-seal/repair-restores-recorded-directories ()
  "Restore empty directories lost on checkout without rewriting ledger events."
  (pos-test-with-scope
    (make-directory (expand-file-name "trial/empty/leaf" scope) t)
    (let* ((archive (pos-test-scope-sealed scope))
           (history (pos-ledger-history archive))
           (events (mapcar #'pos-ledger-read (nth 3 history)))
           (folder (expand-file-name "trial/empty" archive)))
      (delete-directory folder t)
      (should (pos-seal-findings-p (pos-ledger-check scope)))
      (should (equal '((repaired . 0) (restored . 2) (unregistered . 0))
                     (pos-seal-repair scope)))
      (should-not (pos-seal-findings-p (pos-ledger-check scope)))
      (should (equal history (pos-ledger-history archive)))
      (should (equal events (mapcar #'pos-ledger-read (nth 3 history))))
      (should (equal '((repaired . 0) (restored . 0) (unregistered . 0))
                     (pos-seal-repair scope))))))

(ert-deftest pos-seal/repair-restores-recorded-read-bits ()
  "Git loses restrictive read permissions; restore them after verifying bytes."
  (pos-test-with-scope
    (set-file-modes (expand-file-name "trial/result.md" scope) #o600)
    (let* ((archive (pos-test-scope-sealed scope))
           (file (expand-file-name "trial/result.md" archive)))
      (set-file-modes file #o644)
      (should (equal '((repaired . 1) (restored . 0) (unregistered . 0))
                     (pos-seal-repair scope)))
      (should (= #o400 (pos-ledger--mode file)))
      (should-not (pos-seal-findings-p (pos-ledger-check scope))))))

(ert-deftest pos-seal/repair-validates-directories-before-writing ()
  "An obstruction or damaged file refuses restoration before any mutation."
  (dolist (obstruction '(file symlink changed missing executable))
    (ert-info ((symbol-name obstruction))
      (pos-test-with-scope
        (dolist (name '("a" "z/leaf"))
          (make-directory (expand-file-name (concat "trial/" name) scope) t))
        (let* ((archive (pos-test-scope-sealed scope))
               (item (expand-file-name "trial" archive))
               (file (expand-file-name "result.md" item)))
          (delete-directory (expand-file-name "a" item))
          (delete-directory (expand-file-name "z" item) t)
          (pcase obstruction
            ('file (pos-test-write-bytes (expand-file-name "z" item) "obstruction"))
            ('symlink
             (make-directory (expand-file-name "outside" scope))
             (make-symbolic-link (expand-file-name "outside" scope)
                                 (expand-file-name "z" item)))
            ('changed
             (set-file-modes file #o644)
             (pos-test-write-bytes file "changed"))
            ('executable (set-file-modes file #o744))
            ('missing (delete-file file)))
          (should-error (pos-seal-repair scope) :type 'pos-ledger-refused)
          (should-not (file-exists-p (expand-file-name "a" item)))
          (should-not (file-exists-p (expand-file-name "z/leaf" item))))))))

(ert-deftest pos-seal/a-kept-archive-is-checkpointed-and-repaired-by-its-ledger ()
  "A kept archive is checkpointed and repaired from its ledger alone.
Its files are with its keeper: a checkpoint records its head, and
repair protects its ledger's events and looks for nothing else."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let* ((scope (expand-file-name "projects/a" (file-truename dir)))
           (event (car (nth 3 (pos-ledger-history (expand-file-name "archives" scope))))))
      (should (equal (expand-file-name "archive-integrity/checkpoints" scope)
                     (pos-seal-checkpoint scope)))
      (set-file-modes event #o644)
      (should (equal '((repaired . 1) (restored . 0) (unregistered . 0)) (pos-seal-repair scope)))
      (should (zerop (logand (file-modes event) #o222))))))

;;;; Sealing to a keeper

(defmacro pos-seal-test-with-kept (fixture &rest body)
  "Evaluate BODY in FIXTURE's tree, a seal to a keeper, bound as `dir'.
Its recording answers, `tape' gives what of it is left, and `plan' is
the fixture's seal, planned."
  (declare (indent 1))
  `(let ((fixture (pos-fixture "ledger" ,fixture)))
     (pos-fixture-with fixture dir
       (let-alist fixture
         (let* ((tape (pos-test-tape .keeper))
                (pos-remote-send-function tape)
                (pos-remote-keeper-function
                 (lambda (url)
                   (pos-remote-http-create :url url :token (alist-get 'token .keeper))))
                (pos-seal-claims-function (lambda (&rest _) .claims))
                (source (expand-file-name .source dir))
                (plan (pos-seal-plan source (expand-file-name .destination dir)
                                     .ledger_id "2026-09-28")))
           (ignore tape source plan)
           ,@body)))))

(ert-deftest pos-seal/an-item-changed-after-its-keeper-took-it-is-not-removed ()
  "A seal resumed after the keeper has the event removes the item.
Only if it is still what was sealed."
  (pos-seal-test-with-kept "seal-kept-resumed"
    (let ((hash (pos-ledger-sha (pos-ledger-json plan))))
      (pos-test-write-bytes source "changed")
      (pos-test-refused pos-ledger-refused "plan" (pos-seal-apply plan hash))
      (should-not (funcall tape))
      (should (equal "changed" (pos-ledger-read source))))))

(ert-deftest pos-seal/a-plan-is-applied-only-where-it-was-planned-for ()
  "A plan says whether its archive is with a keeper, and which.
One made before the scope's configuration changed is refused, and
nothing is sent."
  (pos-seal-test-with-kept "seal-kept-next"
    (should (equal (alist-get 'url (alist-get 'keeper fixture)) (alist-get 'kept plan)))
    (pos-test-write-bytes (expand-file-name ".pos/config.yaml" dir) "pos: 2\nprojects: projects/\n")
    (pos-test-refused pos-ledger-refused "plan"
      (pos-test-approve plan))
    (should (equal 2 (length (funcall tape))))
    (should (file-exists-p source))))

(ert-deftest pos-seal/a-keeper-that-refuses-leaves-the-item-and-the-ledger ()
  "Refused by its keeper, a seal writes no event and the item stays."
  (pos-seal-test-with-kept "seal-kept-not-allowed"
    (let ((events (nth 2 (pos-ledger-history (alist-get 'archive plan)))))
      (pos-test-refused pos-ledger-refused "access"
        (pos-test-approve plan))
      (should (equal events (nth 2 (pos-ledger-history (alist-get 'archive plan)))))
      (should (file-exists-p source)))))

(ert-deftest pos-seal/a-keeper-with-another-head-is-a-finding ()
  "Asked, a keeper ahead of the ledger is a finding; one level is not.
A keeper that holds what the ledger has is no finding, and one ahead
of it is; what a keeper has erased is not."
  (dolist (expected '(("report-kept-asked" . nil) ("report-kept-erased" . nil)
                      ("report-kept-keeper-ahead" . t)))
    (let ((fixture (pos-fixture "ledger" (car expected))))
      (pos-fixture-with fixture dir
        (pos-test-with-keeper (alist-get 'keeper fixture) nil
          (should (eq (cdr expected)
                      (and (pos-seal-findings-p (pos-ledger-check dir)) t))))))))

(ert-deftest pos-seal/a-check-told-to-stay-offline-asks-no-keeper ()
  "With POS_ARCHIVE_OFFLINE set, a kept archive is reported from its ledger.
Nothing is sent."
  (pos-fixture-with (pos-fixture "ledger" "report-kept-asked") dir
    (let ((process-environment (cons "POS_ARCHIVE_OFFLINE=1" process-environment))
          (pos-ledger-offline nil)
          (pos-remote-send-function (lambda (&rest _) (error "A keeper was asked"))))
      (let ((report (car (pos-ledger-check dir))))
        (should (stringp (alist-get 'kept report)))
        (should (eq :null (alist-get 'keeper report)))))))

(ert-deftest pos-seal/the-tool-commit-is-the-image-s-else-git-s-else-unknown ()
  "The tool's commit is read from COMMIT, from git, or is unknown.
An image writes COMMIT beside the directory of Lisp sources; a checkout
is asked of git; a plain copy of the files knows no commit."
  (pos-test-with-temp-dir dir
    (should-not (pos-seal-tool-commit dir))
    (pos-test-git-init dir)
    (should-not (pos-seal-tool-commit dir))
    (with-temp-file (expand-file-name "README" dir) (insert "pos\n"))
    (pos-test-git dir "add" "README")
    (pos-test-git dir "commit" "-q" "-m" "Begin")
    (should (equal (pos-test-git dir "rev-parse" "HEAD") (pos-seal-tool-commit dir)))
    (with-temp-file (expand-file-name "COMMIT" dir)
      (insert "eac849ed9d3162cf11017715f771458b1d4e60b5\n"))
    (should (equal "eac849ed9d3162cf11017715f771458b1d4e60b5" (pos-seal-tool-commit dir)))
    (with-temp-file (expand-file-name "COMMIT" dir) (insert "not a commit\n"))
    (should-not (pos-seal-tool-commit dir))))

(ert-deftest pos-seal/the-claims-say-where-a-seal-came-from ()
  "The claims name the plan, the tool and what git reads of the repository.
The tool's commit; and the scope, the commit and branch, whether the
tree is dirty, and each remote without the user and password its URL
may hold."
  (pos-fixture-with (pos-fixture "ledger" "seal-kept-first") dir
    (let* ((root (file-truename dir))
           (pos-seal-tool-directory (expand-file-name "tool" root))
           (plan (lambda ()
                   (pos-seal-plan (expand-file-name "projects/a/trial" root)
                                  (expand-file-name "projects/a/archives/trial" root)))))
      (pos-test-same-json
       '(("plan" . "the hash") ("tool" . "poslib") ("scope" . "projects/a"))
       (pos-seal-claims (funcall plan) "the hash"))
      (make-directory pos-seal-tool-directory)
      (with-temp-file (expand-file-name "COMMIT" pos-seal-tool-directory)
        (insert "eac849ed9d3162cf11017715f771458b1d4e60b5\n"))
      (pos-test-same-json
       '(("plan" . "the hash") ("tool" . "poslib")
         ("tool_commit" . "eac849ed9d3162cf11017715f771458b1d4e60b5")
         ("scope" . "projects/a"))
       (pos-seal-claims (funcall plan) "the hash"))
      (delete-directory (expand-file-name ".git" root) t)
      (pos-test-git root "init" "-q" "-b" "trunk")
      (pos-test-git root "remote" "add" "origin" "https://someone:secret@forge.example/some/one.git")
      (pos-test-git root "remote" "add" "mirror" "git@forge.example:some/one.git")
      (pos-test-git root "add" ".pos")
      (pos-test-git root "commit" "-q" "-m" "Configure")
      (pos-test-same-json
       `(("plan" . "the hash") ("tool" . "poslib")
         ("tool_commit" . "eac849ed9d3162cf11017715f771458b1d4e60b5")
         ("scope" . "projects/a")
         ("commit" . ,(pos-test-git root "rev-parse" "HEAD")) ("branch" . "trunk")
         ("dirty" . "true")
         ("remote.origin" . "https://forge.example/some/one.git")
         ("remote.mirror" . "git@forge.example:some/one.git"))
       (pos-seal-claims (funcall plan) "the hash")))))

(provide 'pos-seal-test)
;;; pos-seal-test.el ends here
