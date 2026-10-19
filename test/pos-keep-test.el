;;; pos-keep-test.el --- Tests for pos-keep.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The convert, keep and recall fixtures in
;; fixtures/ledger/ are shared with pyposlib: each gives a tree, what
;; the command does to it and what it answers, and for a keeper, each
;; exchange as its keeper recorded it.

;;; Code:

(require 'ert)
(require 'pos-keep)
(require 'pos-fixtures)
(require 'pos-test-support)

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
                            (pos-keep-convert root)
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
                                             (pos-bytes-read file) 'utf-8)))))
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
                              (pos-keep root)
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
                              (pos-keep-recall root)
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
                                        (decode-coding-string (pos-bytes-read path) 'utf-8)
                                        (logand (file-modes path) #o777))
                                  after))))))
                  (should (equal (mapcar (lambda (entry)
                                           (let-alist entry
                                             (if .directory
                                                 (list .path 'directory)
                                               (list .path .text .mode))))
                                         .after)
                                 (sort after (lambda (a b) (string< (car a) (car b)))))))))))))))

;;;; Relocation

(defun pos-keep-test--old-scope (root rel &optional text)
  "Make the scope REL of ROOT hold an archive and bookkeeping beside it.
A file sealed and checkpointed, then moved to the visible layout of
archives/ and archive-integrity/ beside the scope.  TEXT is the file's."
  (let ((scope (expand-file-name rel root)))
    (pos-test-write-bytes (expand-file-name "item/result.md" scope) (or text "result"))
    (pos-test-approve (pos-seal-plan (expand-file-name "item" scope)
                                     (expand-file-name "archives/item" scope)))
    (pos-checkpoint-record scope)
    (let ((selected (pos-ledger-select scope)))
      (dolist (name '("archives" "archive-integrity"))
        (rename-file (expand-file-name (concat selected "/" name) scope)
                     (expand-file-name name scope)))
      (when (directory-empty-p (expand-file-name selected scope))
        (delete-directory (expand-file-name selected scope))))
    scope))

(defun pos-keep-test--commit (root)
  "Commit everything in the repository at ROOT."
  (pos-test-git root "add" "-A")
  (pos-test-git root "commit" "-q" "-m" "State"))

(defun pos-keep-test--files (dir)
  "Return each file beneath DIR by its path, with its SHA-256 and mode.
Paths are relative to DIR, and Git's own directory is left out."
  (sort (delq nil
              (mapcar (lambda (file)
                        (let ((rel (file-relative-name file dir)))
                          (unless (string-prefix-p ".git/" rel)
                            (list rel (pos-bytes-sha (pos-bytes-read file))
                                  (file-modes file)))))
                      (directory-files-recursively dir "" nil)))
        (lambda (a b) (string< (car a) (car b)))))

(defun pos-keep-test--moved (files from to)
  "Return FILES, as `pos-keep-test--files' gives them, with FROM respelled TO."
  (sort (mapcar (lambda (file)
                  (if (string-prefix-p from (car file))
                      (cons (concat to (substring (car file) (length from))) (cdr file))
                    file))
                files)
        (lambda (a b) (string< (car a) (car b)))))

(defmacro pos-keep-test--with-repository (root &rest body)
  "Evaluate BODY with ROOT a new repository on master."
  (declare (indent 1))
  `(pos-test-with-temp-dir dir
     (let ((,root (file-name-as-directory (file-truename dir))))
       (pos-test-git-init ,root)
       ,@body)))

(ert-deftest pos-keep/relocation-renames-and-rewrites-nothing ()
  "Relocation renames a scope's archive and bookkeeping into its directory.
M01 and A06: for each directory a scope may select, the plan names Git
moves of archives/ and archive-integrity/; applying it keeps every
byte and mode, adds no event or checkpoint, and the check reports the
same heads and root CID.  Nothing is committed."
  (dolist (config '((".clanka" . "config.yml") (".pos" . "config.yaml") (nil)))
    (ert-info ((format "%S" config))
      (pos-keep-test--with-repository root
        (let ((selected (or (car config) ".clankos")))
          (when (car config)
            (pos-test-write-bytes (expand-file-name (concat "s/" (car config) "/" (cdr config))
                                                    root)
                                  "pos: 2\n"))
          (pos-keep-test--old-scope root "s")
          (pos-keep-test--commit root)
          (let* ((before (pos-keep-test--files root))
                 (plan (pos-keep-relocate-plan root))
                 (scope (aref (alist-get 'scopes plan) 0)))
            (should (equal 0 (alist-get 'blocked plan)))
            (should (equal selected (alist-get 'selected scope)))
            (should (equal `[((from . "s/archive-integrity")
                              (to . ,(concat "s/" selected "/archive-integrity"))
                              (move . "git"))
                             ((from . "s/archives") (to . ,(concat "s/" selected "/archives"))
                              (move . "git"))]
                           (alist-get 'renames scope)))
            (let* ((result (pos-keep-relocate root))
                   (check (aref (alist-get 'check (aref (alist-get 'scopes result) 0)) 0)))
              (should (equal (concat "s/" selected "/archives") (alist-get 'archive check))))
            (should (equal (pos-keep-test--moved
                            (pos-keep-test--moved before "s/archives/"
                                                  (concat "s/" selected "/archives/"))
                            "s/archive-integrity/" (concat "s/" selected "/archive-integrity/"))
                           (pos-keep-test--files root)))
            (should-not (pos-checkpoint-findings-p (pos-ledger-check root)))
            (should (string-match-p "\\`R" (pos-test-git root "diff" "--cached" "-M"
                                                         "--name-status")))
            (should (equal "State" (pos-test-git root "log" "-1" "--format=%s")))))))))

(ert-deftest pos-keep/relocation-moves-legacy-checkpoints-alone ()
  "A scope with only legacy checkpoints has them moved, byte for byte.
M02: a checkpoint with no heads in .archive-integrity-anchors goes to
archive-integrity/checkpoints in the selected directory."
  (pos-keep-test--with-repository root
    (let* ((bytes (pos-bytes-json '((schema . 1) (heads . []) (coverage . "tree"))))
           (name (concat (pos-bytes-sha bytes) ".json")))
      (pos-test-write-bytes (expand-file-name (concat "s/.archive-integrity-anchors/" name) root)
                            bytes #o444)
      (pos-test-write-bytes (expand-file-name "s/notes.org" root) "* Notes\n")
      (pos-keep-test--commit root)
      (pos-keep-relocate root)
      (let ((moved (expand-file-name (concat "s/.clankos/archive-integrity/checkpoints/" name)
                                     root)))
        (should (equal bytes (pos-bytes-read moved)))
        (should (equal #o444 (logand (file-modes moved) #o777)))
        (should-not (file-exists-p (expand-file-name "s/.archive-integrity-anchors" root)))))))

(ert-deftest pos-keep/every-blocker-is-reported-and-nothing-moves ()
  "Each scope's blockers are reported together, and relocation refuses them.
M03: a changed archive, unclean; an ambiguous choice of directory; a
scope that cannot be written; and bookkeeping Git does not track,
unversioned.  The scopes are as they were."
  (pos-keep-test--with-repository root
    (let ((corrupt (pos-keep-test--old-scope root "corrupt")))
      (pos-keep-test--old-scope root "ambiguous")
      (pos-keep-test--old-scope root "locked")
      (pos-keep-test--old-scope root "loose")
      (make-directory (expand-file-name "ambiguous/.clanka" root))
      (make-directory (expand-file-name "ambiguous/.pos" root))
      (pos-keep-test--commit root)
      (pos-test-git root "rm" "-q" "-r" "--cached" "loose/archive-integrity")
      (let ((file (expand-file-name "archives/item/result.md" corrupt)))
        (set-file-modes file #o644)
        (pos-test-write-bytes file "changed"))
      (set-file-modes (expand-file-name "locked" root) #o555)
      (unwind-protect
          (let* ((before (pos-keep-test--files root))
                 (plan (pos-keep-relocate-plan root))
                 (reasons (mapcar (lambda (s)
                                    (cons (alist-get 'scope s)
                                          (mapcar (lambda (b) (alist-get 'reason b))
                                                  (alist-get 'blockers s))))
                                  (alist-get 'scopes plan))))
            (should (equal 5 (alist-get 'blocked plan)))
            (should (equal '("directory-ambiguity") (cdr (assoc "ambiguous" reasons))))
            (should (equal '("unclean") (cdr (assoc "corrupt" reasons))))
            (should (member "access" (cdr (assoc "locked" reasons))))
            (should (member "unversioned" (cdr (assoc "loose" reasons))))
            (pos-test-refused pos-ledger-refused "blocked" (pos-keep-relocate root))
            (should (equal before (pos-keep-test--files root))))
        (set-file-modes (expand-file-name "locked" root) #o755)))))

(ert-deftest pos-keep/an-existing-destination-blocks-relocation ()
  "A destination that exists blocks relocation, and neither copy changes.
M04: an archive-integrity/ in .clankos already, beside the visible one."
  (pos-keep-test--with-repository root
    (pos-keep-test--old-scope root "s")
    (pos-test-write-bytes (expand-file-name "s/.clankos/archive-integrity/README" root) "x\n")
    (pos-keep-test--commit root)
    (let* ((before (pos-keep-test--files root))
           (scope (aref (alist-get 'scopes (pos-keep-relocate-plan root)) 0)))
      (should (equal '((reason . "destination")
                       (paths . ["s/.clankos/archive-integrity" "s/archive-integrity"])
                       (message . "The destination exists"))
                     (aref (alist-get 'blockers scope) 0)))
      (pos-test-refused pos-ledger-refused "blocked" (pos-keep-relocate root))
      (should (equal before (pos-keep-test--files root))))))

(ert-deftest pos-keep/relocation-resumes-and-then-does-nothing ()
  "A relocation stopped between scopes resumes, and a second run does nothing.
M05: after one of two scopes is moved, a fresh run moves the other
alone.  M06: a run with nothing to move changes nothing, and the
program plans nothing and exits 0."
  (pos-keep-test--with-repository root
    (pos-keep-test--old-scope root "a")
    (pos-keep-test--old-scope root "b")
    (pos-keep-test--commit root)
    (pos-keep-relocate (expand-file-name "a" root))
    (let ((plan (pos-keep-relocate-plan root)))
      (should (equal '("b") (mapcar (lambda (s) (alist-get 'scope s))
                                    (alist-get 'scopes plan)))))
    (pos-keep-relocate root)
    (let ((after (pos-keep-test--files root)))
      (should (equal [] (alist-get 'scopes (pos-keep-relocate root))))
      (should (equal after (pos-keep-test--files root))))
    (should (equal 0 (car (pos-test-program nil "pos-seal-batch" "relocate-plan" root))))))

(ert-deftest pos-keep/a-ledger-made-again-beside-the-scope-is-refused ()
  "After relocation, a ledger recreated beside the scope is refused.
M07: the next read and the next write name both ledgers."
  (pos-keep-test--with-repository root
    (let ((scope (pos-keep-test--old-scope root "s")))
      (pos-keep-test--commit root)
      (pos-keep-relocate root)
      (copy-directory (expand-file-name ".clankos/archive-integrity/ledger" scope)
                      (expand-file-name "archive-integrity/ledger" scope) t t)
      (let ((both (list (expand-file-name ".clankos/archive-integrity/ledger" scope)
                        (expand-file-name "archive-integrity/ledger" scope))))
        (dolist (act (list (lambda () (pos-ledger-history (pos-test-archived scope)))
                           (lambda () (pos-checkpoint-record scope))))
          (let ((refused (should-error (funcall act) :type 'pos-ledger-refused)))
            (should (eq 'ledger-location (nth 1 refused)))
            (should (equal both (nth 4 refused)))))))))

(ert-deftest pos-keep/an-uncommitted-archive-is-renamed-and-excluded-again ()
  "An archive Git ignores is renamed by the file system and excluded anew.
A07: the managed exclusions name the archive at its new path, and the
bookkeeping Git tracks moves by Git's move."
  (pos-keep-test--with-repository root
    (pos-test-write-bytes (expand-file-name ".clanka/config.yml" root) "pos: 2\n")
    (pos-keep-test--old-scope root ".")
    (pos-tree-ignore-archives root)
    (should (eq 0 (car (pos-git root "check-ignore" "-q" "archives/item/result.md"))))
    (pos-keep-test--commit root)
    (let ((scope (aref (alist-get 'scopes (pos-keep-relocate-plan root)) 0)))
      (should (equal [((from . "archive-integrity") (to . ".clanka/archive-integrity")
                       (move . "git"))
                      ((from . "archives") (to . ".clanka/archives") (move . "rename"))]
                     (alist-get 'renames scope))))
    (should (equal "regenerated" (alist-get 'excludes (aref (alist-get 'scopes (pos-keep-relocate root)) 0))))
    (should (file-exists-p (expand-file-name ".clanka/archives/item/result.md" root)))
    (should (eq 0 (car (pos-git root "check-ignore" "-q" ".clanka/archives/item/result.md"))))))

;;;; Retired scopes in earlier layouts

(defun pos-keep-test--old-retired (archive name layout)
  "Write in ARCHIVE the scope NAME, retired in LAYOUT, from the T04-read fixture.
LAYOUT is sibling, internal or anchors: the ledger and checkpoints
beside the scope's archives/, the ledger inside it, or the checkpoints
in .archive-integrity-anchors."
  (seq-doseq (entry (alist-get 'tree (pos-fixture "layout" "T04-read")))
    (let-alist entry
      (let ((path (pcase layout
                    ('internal (replace-regexp-in-string
                                "\\`archive-integrity/ledger/" "archives/.archive-integrity/"
                                .path))
                    ('anchors (replace-regexp-in-string
                               "\\`archive-integrity/checkpoints/"
                               ".archive-integrity-anchors/" .path))
                    (_ .path))))
        (unless (and (eq layout 'internal) (string-prefix-p "archive-integrity/checkpoints/" path))
          (pos-test-write-bytes (expand-file-name (concat name "/" path) archive)
                                (encode-coding-string .text 'utf-8) #o444))))))

(ert-deftest pos-keep/scopes-retired-in-earlier-layouts-are-left-as-they-are ()
  "Scopes retired in each earlier layout are checked clean and not moved.
O07: inside a parent's archive, a scope with its ledger beside its
archive, one with the legacy ledger inside it, and one with legacy
checkpoints are reported clean; relocate-plan lists nothing; and
relocate-apply leaves every byte, mode and path as it was."
  (pos-keep-test--with-repository root
    (let ((archive (pos-test-archived root)))
      (pos-keep-test--old-retired archive "sibling" 'sibling)
      (pos-keep-test--old-retired archive "internal" 'internal)
      (pos-keep-test--old-retired archive "anchors" 'anchors)
      (pos-ledger-write-new
       (let ((event (pos-ledger-event (pos-ledger-inventory archive) nil 1
                                      "0f1e2d3c-4b5a-4968-8778-a6b5c4d3e2f1")))
         (prog1 (expand-file-name (concat ".clankos/archive-integrity/ledger/" (car event))
                                  root)
           (setq archive (cons archive (cdr event)))))
       (cdr archive))
      (setq archive (car archive))
      (pos-checkpoint-record root)
      (pos-keep-test--commit root)
      (should-not (pos-checkpoint-findings-p (pos-ledger-check root)))
      (let ((before (pos-keep-test--files root)))
        (should (equal [] (alist-get 'scopes (pos-keep-relocate-plan root))))
        (should (equal [] (alist-get 'scopes (pos-keep-relocate root))))
        (should (equal before (pos-keep-test--files root)))
        (should-not (pos-checkpoint-findings-p (pos-ledger-check root)))))))

(ert-deftest pos-keep/a-declared-repository-that-is-missing-blocks-relocation ()
  "A child repository a configuration declares and that is not there blocks.
D11: the walk cannot reach its scopes, so the plan reports it as a
mount blocker, and applying refuses with nothing moved."
  (pos-keep-test--with-repository root
    (pos-test-write-bytes (expand-file-name ".clanka/config.yml" root)
                          (concat "pos: 2\nchildren:\n"
                                  "  - path: mounted\n"
                                  "    remote: git@example.org:mounted.git\n"))
    (pos-keep-test--old-scope root "s")
    (pos-keep-test--commit root)
    (let* ((before (pos-keep-test--files root))
           (plan (pos-keep-relocate-plan root))
           (entry (seq-find (lambda (s) (equal "." (alist-get 'scope s)))
                            (alist-get 'scopes plan))))
      (should (equal 1 (alist-get 'blocked plan)))
      (should (equal "mount" (alist-get 'reason (aref (alist-get 'blockers entry) 0))))
      (should (equal ["mounted"] (alist-get 'paths (aref (alist-get 'blockers entry) 0))))
      (pos-test-refused pos-ledger-refused "blocked" (pos-keep-relocate root))
      (should (equal before (pos-keep-test--files root))))))

(provide 'pos-keep-test)
;;; pos-keep-test.el ends here
