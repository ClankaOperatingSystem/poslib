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
(require 'pos-checkpoint)
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
               (scope (pos-path-archive-scope (alist-get 'archive plan))))
          `((plan . ,(pos-seal-test-relative-plan plan dir))
            (event . ((name . ,(file-name-nondirectory (car result)))
                      (encoded . ,(decode-coding-string (pos-bytes-read (car result))
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

(ert-deftest pos-seal/sealing-removes-write-bits ()
  "The sealed files and the ledger event are read-only; CIDs do not change."
  (pos-test-with-scope
    (let* ((plan (pos-test-scope-plan scope))
           (event (car (pos-test-approve plan)))
           (file (pos-test-archived scope "trial/result.md")))
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
           (archive (pos-test-archived scope))
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
      (should-not (file-exists-p (pos-test-archived scope "trial"))))))

(ert-deftest pos-seal/an-interrupted-seal-resumes ()
  "After the move but before the event, the same plan finishes the work;
applied twice, it adds nothing."
  (pos-test-with-scope
    (let* ((plan (pos-test-scope-plan scope))
           (hash (pos-bytes-sha (pos-bytes-json plan))))
      (make-directory (pos-test-archived scope) t)
      (rename-file (expand-file-name "trial" scope) (pos-test-archived scope "trial"))
      (let ((first (pos-seal-apply plan hash)))
        (should (equal first (pos-seal-apply plan hash))))
      (should (equal 1 (pos-history-events (pos-ledger-history (pos-test-archived scope))))))))

(ert-deftest pos-seal/a-new-record-is-staged-then-sealed ()
  "New bytes are staged beside the archive, then sealed like any item."
  (pos-test-with-scope
    (let* ((plan (pos-seal-stage "handover\n" (pos-test-archived scope "journal/h.md")))
           (staged (alist-get 'source plan)))
      (should (string-prefix-p (file-truename (expand-file-name "_seal/" scope)) staged))
      (pos-test-approve plan)
      (should-not (file-exists-p staged))
      (should (equal "handover\n" (pos-bytes-read (pos-test-archived scope "journal/h.md")))))))

(ert-deftest pos-seal/a-new-record-staged-again-replaces-the-first ()
  "Staging for one destination twice leaves one staged file, the second's.
The first plan is refused, the second seals, and _seal is gone."
  (pos-test-with-scope
    (let* ((target (pos-test-archived scope "journal/h.md"))
           (first (pos-seal-stage "draft\n" target))
           (second (pos-seal-stage "handover\n" target)))
      (should (equal (alist-get 'source first) (alist-get 'source second)))
      (should (equal 1 (length (directory-files (expand-file-name "_seal" scope) nil
                                                "\\`new-"))))
      (should (equal 'plan
                     (cadr (should-error
                            (pos-seal-apply first (pos-bytes-sha (pos-bytes-json first)))
                            :type 'pos-ledger-refused))))
      (pos-test-approve second)
      (should (equal "handover\n" (pos-bytes-read target)))
      (should-not (file-exists-p (expand-file-name "_seal" scope))))))

(ert-deftest pos-seal/a-new-record-may-start-an-archive ()
  "A scope with no archives/ yet has an empty one; sealing makes it."
  (pos-test-with-scope
    (delete-directory (pos-test-archived scope))
    (let ((plan (pos-seal-stage "first\n" (pos-test-archived scope "first.txt"))))
      (should (equal 1 (alist-get 'number plan)))
      (pos-test-approve plan)
      (should (equal "first\n" (pos-bytes-read (pos-test-archived scope "first.txt"))))
      (should (file-directory-p (expand-file-name ".clankos/archive-integrity/ledger" scope))))))

(ert-deftest pos-seal/a-refused-new-record-leaves-nothing-staged ()
  "When planning fails, the staged bytes and an emptied _seal/ go."
  (pos-test-with-scope
    (pos-test-write-bytes (pos-test-archived scope "taken.txt") "taken")
    (should-error (pos-seal-stage "new\n" (pos-test-archived scope "taken.txt"))
                  :type 'pos-ledger-refused)
    (should-not (file-exists-p (expand-file-name "_seal" scope)))))

(ert-deftest pos-seal/a-new-record-s-links-are-resolved ()
  "A new record's links are found, and read as written from its destination.
The destination as it is spelled: here beside the scope, where its
archive was before it moved into the scope's configuration directory."
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
                                 (pos-test-archived scope "journal/h.md")
                                 nil scope))
           (links (alist-get 'links plan)))
      (should (equal "rumour" (alist-get 'kind (aref links 0))))
      (should (string-match-p "Rumour of notes.org"
                              (alist-get 'text (aref (alist-get 'rumours plan) 0)))))
    (let ((staged (directory-files (expand-file-name "_seal" scope) nil "\\`new-")))
      (should-error (pos-seal-stage "new\n" (pos-test-archived scope "journal/i.md")
                                    nil (expand-file-name "nowhere" scope))
                    :type 'pos-ledger-refused)
      (should (equal staged
                     (directory-files (expand-file-name "_seal" scope) nil "\\`new-"))))))

(ert-deftest pos-seal/an-id-link-in-an-item-is-resolved-as-a-link-to-its-file ()
  "An id link in an item's Org file is read as a link to the file with the ID.
To another file of the item it cites that file's CID, with the place
of the ID as its search option: none for the file's own ID, a
heading's CUSTOM_ID, else its title.  To a file of the garden outside
the item it cites a rumour of that file.  To an ID no file holds it
is kept, annotated as broken.  The sealed file holds the rewritten
links."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "canon.org" scope)
                          ":PROPERTIES:\n:ID: canon-id\n:END:\n#+TITLE: Canon\n")
    (pos-test-write-bytes
     (expand-file-name "trial/notes.org" scope)
     (concat ":PROPERTIES:\n:ID: notes-id\n:END:\n"
             "* Result\n:PROPERTIES:\n:ID: result-id\n:END:\n"
             "* Other\n:PROPERTIES:\n:ID: other-id\n:CUSTOM_ID: other\n:END:\n"))
    (pos-test-write-bytes
     (expand-file-name "trial/index.org" scope)
     (concat "[[id:notes-id]] [[id:result-id][r]] [[id:other-id]]\n"
             "[[id:canon-id][canon]] [[id:absent-id][gone]]\n"))
    (let* ((plan (pos-test-scope-plan scope))
           (links (seq-filter (lambda (link)
                                (equal "trial/index.org" (alist-get 'file link)))
                              (alist-get 'links plan)))
           (notes (alist-get 'cid (cdr (assoc "trial/notes.org"
                                              (mapcar (lambda (entry)
                                                        (cons (pos-bytes-key (car entry))
                                                              (cdr entry)))
                                                      (alist-get 'add plan))))))
           (rumour (alist-get 'cid (aref (alist-get 'rumours plan) 0))))
      (should (equal (list (list "id:notes-id" "cid" (concat "ipfs://" notes))
                           (list "id:result-id" "cid" (concat "ipfs://" notes "::*Result"))
                           (list "id:other-id" "cid" (concat "ipfs://" notes "::#other"))
                           (list "id:canon-id" "rumour" (concat "ipfs://" rumour))
                           (list "id:absent-id" "broken" "broken:id:absent-id"))
                     (mapcar (lambda (link)
                               (let-alist link (list .from .kind .to)))
                             links)))
      (should (string-match-p "Rumour of canon.org"
                              (alist-get 'text (aref (alist-get 'rumours plan) 0))))
      (pos-test-approve plan)
      (should (equal (concat "[[ipfs://" notes "]] [[ipfs://" notes "::*Result][r]]"
                             " [[ipfs://" notes "::#other]]\n"
                             "[[ipfs://" rumour "][canon]] [[broken:id:absent-id][gone]]\n")
                     (decode-coding-string
                      (pos-bytes-read (pos-test-archived scope "trial/index.org"))
                      'utf-8))))))

(ert-deftest pos-seal/an-id-two-files-hold-is-refused ()
  "An id link whose ID two files hold is refused (unresolved).
The files are read only when the item has an id link: with none, an
ID held twice elsewhere is no matter of the seal's."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "a.org" scope) ":PROPERTIES:\n:ID: twin\n:END:\n")
    (pos-test-write-bytes (expand-file-name "b.org" scope) ":PROPERTIES:\n:ID: twin\n:END:\n")
    (should (pos-test-scope-plan scope))
    (pos-test-write-bytes (expand-file-name "trial/index.org" scope) "[[id:twin]]\n")
    (pos-test-refused pos-ledger-refused 'unresolved (pos-test-scope-plan scope))))

(ert-deftest pos-seal/two-items-may-rumour-one-target-on-one-day ()
  "Each item's rumour of a target names that item, so is its own record;
a rumour already sealed word for word is cited, not sealed again."
  (pos-test-with-scope
    (pos-test-write-bytes (expand-file-name "notes.org" scope) "#+TITLE: Notes\n")
    (let ((seal (lambda (name)
                  (pos-test-write-bytes (expand-file-name (concat name "/r.md") scope)
                                        "See [notes](../notes.org).\n")
                  (let ((plan (pos-seal-plan (expand-file-name name scope)
                                             (expand-file-name (concat pos-test-archive "/" name) scope)
                                             nil "2026-09-28")))
                    (pos-test-approve plan)
                    (alist-get 'destination (aref (alist-get 'rumours plan) 0))))))
      (should-not (equal (funcall seal "first") (funcall seal "second")))
      (should (equal 4 (pos-history-events (pos-ledger-history (pos-test-archived scope))))))))

(ert-deftest pos-seal/a-program-applies-its-own-plan-explicitly ()
  "The write-new command with --apply seals at once; without it, a plan."
  (pos-test-with-scope
    (let* ((target (pos-test-archived scope "journal/h.md"))
           (run (lambda (&rest args)
                  (apply #'pos-test-program "handover\n" "pos-seal-batch"
                         "write-new" target args))))
      (let ((planned (funcall run)))
        (ert-info ((cdr planned)) (should (equal 0 (car planned)))))
      (should-not (file-exists-p target))
      (let ((applied (funcall run "--apply")))
        (ert-info ((cdr applied)) (should (equal 0 (car applied)))))
      (should (equal "handover\n" (pos-bytes-read target)))
      (should-not (file-exists-p (expand-file-name "_seal" scope))))))

(ert-deftest pos-seal/a-program-seals-an-item-explicitly ()
  "The seal command with --apply moves and seals at once; without it, a plan."
  (pos-test-with-scope
    (let* ((source (expand-file-name "trial" scope))
           (target (pos-test-archived scope "trial"))
           (run (lambda (&rest args)
                  (car (apply #'pos-test-program nil "pos-seal-batch"
                              "seal" source target args)))))
      (should (equal 0 (funcall run)))
      (should-not (file-exists-p target))
      (should (equal 0 (funcall run "--apply")))
      (should (file-exists-p (expand-file-name "result.md" target)))
      (should (equal 1 (pos-history-events (pos-ledger-history (pos-test-archived scope))))))))

(ert-deftest pos-seal/a-staging-directory-left-empty-is-not-sealed ()
  "An empty _seal in an item is removed and no event records it.
At any depth; one that holds something is sealed as it is, and another
empty directory is recorded.  The check after the seal is clean."
  (pos-test-with-scope
    (dolist (dir '("trial/_seal" "trial/child/_seal" "trial/kept/_seal" "trial/hollow"))
      (make-directory (expand-file-name dir scope) t))
    (pos-test-write-bytes (expand-file-name "trial/kept/_seal/plan.json" scope) "{}")
    (let* ((archive (pos-test-scope-sealed scope))
           (event (pos-bytes-parse
                   (pos-bytes-read (car (last (pos-history-files (pos-ledger-history archive))))))))
      (should (equal ["trial/child" "trial/hollow"] (alist-get 'empty event)))
      (should-not (file-exists-p (expand-file-name "trial/_seal" archive)))
      (should-not (file-exists-p (expand-file-name "trial/child/_seal" archive)))
      (should (file-exists-p (expand-file-name "trial/kept/_seal/plan.json" archive)))
      (should-not (pos-checkpoint-findings-p (pos-ledger-check scope))))))

(ert-deftest pos-seal/a-new-record-leaves-no-staging-directory ()
  "A record staged by write-new is sealed and _seal, left empty, is gone;
a _seal that holds something else stays."
  (pos-test-with-scope
    (let ((seal (lambda (name)
                  (let ((plan (pos-seal-stage "record\n" (expand-file-name
                                                          (concat pos-test-archive "/" name) scope))))
                    (pos-test-approve plan)))))
      (funcall seal "first.txt")
      (should-not (file-exists-p (expand-file-name "_seal" scope)))
      (pos-test-write-bytes (expand-file-name "_seal/other.json" scope) "{}")
      (funcall seal "second.txt")
      (should (equal '("other.json") (pos-ledger-entry-names (expand-file-name "_seal" scope)))))))

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
           (run (lambda (path) (pos-test-program nil "pos-seal-batch" "link" path))))
      (should (equal (cons 0 (concat (pos-links-link (expand-file-name "trial/result.md" archive))
                                     "\n"))
                     (funcall run (expand-file-name "trial/result.md" archive))))
      (should (equal '(2 . "") (funcall run (expand-file-name "other" archive)))))))

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

(ert-deftest pos-seal/retirement-preserves-nested-archives ()
  "Staging cleanup must not change the CID of an archive inside the item."
  (pos-test-with-scope
    (let ((archive (expand-file-name "trial/archives" scope)))
      (make-directory (expand-file-name "old/_seal" archive) t)
      (pos-test-write-bytes (expand-file-name "old/record.txt" archive) "recorded")
      (let ((before (pos-cid-directory archive)))
        (pos-test-scope-sealed scope)
        (should (equal before (pos-cid-directory
                               (pos-test-archived scope "trial/archives"))))
        (should (file-directory-p
                 (pos-test-archived scope "trial/archives/old/_seal")))))))

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
                (pos-http-send-function tape)
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
    (let ((hash (pos-bytes-sha (pos-bytes-json plan))))
      (pos-test-write-bytes source "changed")
      (pos-test-refused pos-ledger-refused "plan" (pos-seal-apply plan hash))
      (should-not (funcall tape))
      (should (equal "changed" (pos-bytes-read source))))))

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
    (let ((events (pos-history-events (pos-ledger-history (alist-get 'archive plan)))))
      (pos-test-refused pos-ledger-refused "access"
        (pos-test-approve plan))
      (should (equal events (pos-history-events (pos-ledger-history (alist-get 'archive plan)))))
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
                      (and (pos-checkpoint-findings-p (pos-ledger-check dir)) t))))))))

(ert-deftest pos-seal/a-check-told-to-stay-offline-asks-no-keeper ()
  "With POS_ARCHIVE_OFFLINE set, a kept archive is reported from its ledger.
Nothing is sent."
  (pos-fixture-with (pos-fixture "ledger" "report-kept-asked") dir
    (let ((process-environment (cons "POS_ARCHIVE_OFFLINE=1" process-environment))
          (pos-ledger-offline nil)
          (pos-http-send-function (lambda (&rest _) (error "A keeper was asked"))))
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

;;;; Where a writer writes

(ert-deftest pos-seal/a-writer-writes-in-the-selected-directory ()
  "A seal and a checkpoint at a scope write in the directory selected there.
For each configuration directory, holding the scope's configuration or
standing bare, and for none, where .clankos is made: the archive and
its bookkeeping are made there, nothing visible beside them, and no
configuration file is made."
  (dolist (case '((".clanka" . "config.yaml") (".clankos" . "config.yml")
                  (".pos" . "config.yaml") (".clanka") (".pos") (nil)))
    (ert-info ((format "%S" case))
      (pos-test-with-temp-dir dir
        (let* ((scope (expand-file-name "scope" (file-truename dir)))
               (selected (or (car case) ".clankos")))
          (pos-test-write-bytes (expand-file-name "trial/result.md" scope) "result")
          (cond ((cdr case)
                 (pos-test-write-bytes (expand-file-name (concat (car case) "/" (cdr case))
                                                         scope)
                                       "pos: 2\n"))
                ((car case) (make-directory (expand-file-name (car case) scope) t)))
          (let ((plan (pos-seal-plan (expand-file-name "trial" scope)
                                     (expand-file-name "archives/trial" scope))))
            (should (equal (expand-file-name (concat selected "/archives") scope)
                           (alist-get 'archive plan)))
            (pos-test-approve plan))
          (should (equal (expand-file-name (concat selected "/archive-integrity/checkpoints")
                                           scope)
                         (pos-checkpoint-record scope)))
          (should (file-exists-p (expand-file-name (concat selected "/archives/trial/result.md")
                                                   scope)))
          (should (file-directory-p
                   (expand-file-name (concat selected "/archive-integrity/ledger") scope)))
          (should-not (file-exists-p (expand-file-name "archives" scope)))
          (should-not (file-exists-p (expand-file-name "archive-integrity" scope)))
          (unless (cdr case)
            (should-not (pos-path-config-files scope))))))))

(ert-deftest pos-seal/a-writer-refuses-an-archive-beside-its-scope ()
  "A visible archives/ or archive-integrity/ at a scope refuses every writer.
Sealing, staging a new record and checkpointing are refused as
relocation-required, naming what lies beside the scope, and nothing
is moved or made."
  (dolist (beside '("archives" "archive-integrity"))
    (ert-info (beside)
      (pos-test-with-temp-dir dir
        (let ((scope (expand-file-name "scope" (file-truename dir))))
          (pos-test-write-bytes (expand-file-name "trial/result.md" scope) "result")
          (make-directory (expand-file-name beside scope) t)
          (dolist (write (list (lambda () (pos-seal-plan (expand-file-name "trial" scope)
                                                         (expand-file-name "archives/trial"
                                                                           scope)))
                               (lambda () (pos-seal-stage "new\n"
                                                          (expand-file-name "archives/new.md"
                                                                            scope)))
                               (lambda () (pos-checkpoint-record scope))))
            (let ((refused (should-error (funcall write) :type 'pos-ledger-refused)))
              (should (eq 'relocation-required (nth 1 refused)))
              (should (equal (list (expand-file-name beside scope)) (nth 4 refused)))))
          (should (equal (list beside "trial")
                         (pos-ledger-entry-names scope)))
          (should (file-exists-p (expand-file-name "trial/result.md" scope))))))))

;;;; Retirement

(defun pos-seal-test--files (dir)
  "Return each file beneath DIR by its path, with its SHA-256 and mode."
  (sort (mapcar (lambda (file)
                  (list (file-relative-name file dir) (pos-bytes-sha (pos-bytes-read file))
                        (file-modes file)))
                (directory-files-recursively dir "" nil))
        (lambda (a b) (string< (car a) (car b)))))

(defun pos-seal-test--child (garden &optional selected)
  "Make GARDEN's scope trial, with an archive and bookkeeping in SELECTED.
SELECTED is a bare configuration directory made first, by default
none, so that .clankos is selected.  Return the scope."
  (let ((trial (expand-file-name "trial" garden)))
    (when selected (make-directory (expand-file-name selected trial) t))
    (pos-test-write-bytes (expand-file-name "item/result.md" trial) "result")
    (pos-test-write-bytes (expand-file-name "notes.org" trial) "* Notes\n")
    (pos-test-approve (pos-seal-plan (expand-file-name "item" trial)
                                     (expand-file-name "archives/item" trial)))
    (pos-checkpoint-record trial)
    trial))

(ert-deftest pos-seal/a-retired-scope-s-hidden-archive-is-sealed-visible ()
  "Retiring a scope seals its hidden archive and bookkeeping visibly.
They lie at the top of the item.
O03 and A08: for each configuration directory, the sealed item holds
archives/ and archive-integrity/ with their bytes and modes, and no
configuration directory; the child archive's CID is unchanged; the
parent's check, which reads the nested ledger, has no finding; the live
scope and the staged export are gone."
  (dolist (selected '(".clankos" ".clanka" ".pos"))
    (ert-info (selected)
      (pos-test-with-temp-dir dir
        (let* ((garden (file-truename dir))
               (trial (pos-seal-test--child garden selected))
               (hidden (expand-file-name selected trial))
               (child-cid (pos-cid-tree (expand-file-name "archives" hidden)))
               (archived (pos-seal-test--files hidden))
               (plan (pos-seal-plan trial (expand-file-name "archives/trial" garden))))
          (should (equal trial (alist-get 'retires plan)))
          (should (file-directory-p trial))
          (pos-test-approve plan)
          (let ((item (pos-test-archived garden "trial")))
            (should-not (file-exists-p (expand-file-name selected item)))
            (should (equal archived
                           (seq-remove (lambda (file) (equal (car file) "notes.org"))
                                       (pos-seal-test--files item))))
            (should (equal (cdr (assoc "." child-cid))
                           (cdr (assoc "." (pos-cid-tree (expand-file-name "archives" item))))))
            (should (equal "* Notes\n" (pos-bytes-read (expand-file-name "notes.org" item)))))
          (should-not (pos-checkpoint-findings-p (pos-ledger-check garden)))
          (should-not (file-exists-p trial))
          (should-not (file-exists-p (expand-file-name "_seal" garden))))))))

(ert-deftest pos-seal/retirement-refuses-bookkeeping-in-two-places ()
  "A scope with its archive hidden and an archive-integrity/ beside it is refused.
O04: neither copy changes, and nothing is staged."
  (pos-test-with-temp-dir dir
    (let* ((garden (file-truename dir))
           (trial (pos-seal-test--child garden)))
      (pos-test-write-bytes (expand-file-name "archive-integrity/README" trial) "x\n")
      (let ((before (pos-seal-test--files garden)))
        (pos-test-refused pos-ledger-refused "relocation-required"
          (pos-seal-plan trial (expand-file-name "archives/trial" garden)))
        (should (equal before (pos-seal-test--files garden)))))))

(ert-deftest pos-seal/retirement-refuses-other-hidden-files ()
  "Other files in the configuration directory are refused as hidden.
O05: the rule for any hidden file holds for the export, and the
staged export is removed."
  (pos-test-with-temp-dir dir
    (let* ((garden (file-truename dir))
           (trial (pos-seal-test--child garden)))
      (pos-test-write-bytes (expand-file-name ".clankos/people.org" trial) "* People\n")
      (let ((before (pos-seal-test--files garden)))
        (pos-test-refused pos-ledger-refused "hidden"
          (pos-seal-plan trial (expand-file-name "archives/trial" garden)))
        (should (equal before (pos-seal-test--files garden)))))))

(ert-deftest pos-seal/a-retirement-planned-leaves-the-scope-until-applied ()
  "Planning a retirement stages an export and changes nothing in the scope.
O08: planning again replaces the export; a scope changed after review
refuses the plan, sealing nothing; the plan made afresh applies."
  (pos-test-with-temp-dir dir
    (let* ((garden (file-truename dir))
           (trial (pos-seal-test--child garden))
           (before (pos-seal-test--files trial))
           (destination (expand-file-name "archives/trial" garden)))
      (pos-seal-plan trial destination)
      (let ((plan (pos-seal-plan trial destination)))
        (should (equal before (pos-seal-test--files trial)))
        (should (equal 1 (length (directory-files (expand-file-name "_seal" garden) nil
                                                  "\\`export-"))))
        (pos-test-write-bytes (expand-file-name "notes.org" trial) "* Changed\n")
        (pos-test-refused pos-ledger-refused "plan" (pos-test-approve plan))
        (should (file-exists-p trial))
        (should-not (file-exists-p (pos-test-archived garden "trial"))))
      (pos-test-approve (pos-seal-plan trial destination))
      (should-not (file-exists-p trial))
      (should (equal "* Changed\n"
                     (pos-bytes-read (pos-test-archived garden "trial/notes.org")))))))

(ert-deftest pos-seal/a-retirement-export-s-checkpoints-are-checked ()
  "The export's checkpoints must name the ledger it holds, else it is refused.
A scope whose ledger is gone and whose checkpoint remains is refused
before anything is planned, and nothing is left staged."
  (pos-test-with-temp-dir dir
    (let* ((garden (file-truename dir))
           (trial (pos-seal-test--child garden)))
      (pos-seal--remove-tree (expand-file-name ".clankos/archive-integrity/ledger" trial))
      (pos-test-refused pos-ledger-refused "anchor"
        (pos-seal-plan trial (expand-file-name "archives/trial" garden)))
      (should-not (file-exists-p (expand-file-name "_seal" garden))))))

(ert-deftest pos-seal/a-scope-whose-keeper-keeps-its-archive-retires ()
  "A scope whose archive a keeper keeps retires as its ledger and checkpoints.
O06: its archive-integrity/, hidden in .clankos, is at the top of the
item, there being no archives/ anywhere; no keeper is asked; and the
parent's check after the seal, asking no keeper, has no finding."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let* ((garden (file-truename dir))
           (scope (expand-file-name "projects/a" garden))
           (process-environment (cons "POS_ARCHIVE_OFFLINE=1" process-environment)))
      (make-directory (expand-file-name ".clankos" scope))
      (rename-file (expand-file-name "archive-integrity" scope)
                   (expand-file-name ".clankos/archive-integrity" scope))
      ;; The garden's own checkpoint names a head of a ledger the kept
      ;; archive enrols, which its keeper holds and the retired item does not.
      (pos-seal--remove-tree (expand-file-name "archive-integrity" garden))
      (let* ((ledger (pos-seal-test--files (expand-file-name ".clankos" scope)))
             (pos-ledger-ask-function (lambda (&rest _) (error "A keeper was asked")))
             (plan (pos-seal-plan scope (expand-file-name "archives/a" garden))))
        (pos-test-approve plan)
        (let ((item (expand-file-name ".pos/archives/a" garden)))
          (should (equal ledger (pos-seal-test--files item)))
          (should-not (file-exists-p (expand-file-name "archives" item))))
        (should-not (file-exists-p scope))
        (should-not (pos-checkpoint-findings-p (pos-ledger-check garden)))))))

(provide 'pos-seal-test)
;;; pos-seal-test.el ends here
