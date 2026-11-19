;;; pos-checkpoint-test.el --- Tests for pos-checkpoint.el  -*- lexical-binding: t -*-

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

;; Run: make test.  Each test is named for the rule it pins, on a
;; scope sealed by test/pos-test-support.el, or on a shared fixture.

;;; Code:

(require 'ert)
(require 'pos-checkpoint)
(require 'pos-fixtures)
(require 'pos-test-support)

(ert-deftest pos-seal/a-checkpoint-waits-for-a-clean-check ()
  "An unregistered file refuses a checkpoint; gone, the heads are recorded.
Beside the scope; and recording them again is no conflict."
  (pos-test-with-scope
    (let ((archive (pos-test-scope-sealed scope)))
      (pos-test-write-bytes (expand-file-name "stray.txt" archive) "stray" #o444)
      (should-error (pos-checkpoint-record scope) :type 'pos-ledger-refused)
      (delete-file (expand-file-name "stray.txt" archive))
      (let* ((home (pos-checkpoint-record scope))
             (head (pos-history-head (pos-ledger-history archive)))
             (bytes (pos-bytes-json `((schema . 2) (coverage . "tree")
                                      (archives . ((.clankos/archives . ,head)))
                                      (supersedes . []))))
             (file (expand-file-name (concat (pos-bytes-sha bytes) ".json") home)))
        (should (equal home (expand-file-name ".clankos/archive-integrity/checkpoints"
                                              (file-truename scope))))
        (should (equal bytes (pos-bytes-read file)))
        (should (equal home (pos-checkpoint-record scope)))
        (should-not (pos-checkpoint-findings-p (pos-ledger-check scope)))))))

(ert-deftest pos-seal/an-archive-s-checkpoint-covers-the-archive ()
  "Checkpointing an archive records its head as sealing recorded it.
With archive coverage, the same bytes, so no second file appears."
  (pos-test-with-scope
    (let* ((archive (pos-test-scope-sealed scope))
           (home (expand-file-name ".clankos/archive-integrity/checkpoints" (file-truename scope)))
           (before (directory-files home nil "\\.json\\'")))
      (should (equal home (pos-checkpoint-record archive)))
      (should (equal before (directory-files home nil "\\.json\\'"))))))

(defun pos-checkpoint-test--tree-checkpoints (home)
  "Return the schema 2 checkpoints in HOME, as parsed, oldest first."
  (seq-filter (lambda (value) (eql 2 (alist-get 'schema value)))
              (mapcar (lambda (file) (pos-bytes-parse (pos-bytes-read file)))
                      (sort (directory-files home t "\\.json\\'")
                            #'file-newer-than-file-p))))

(ert-deftest pos-seal/a-tree-s-checkpoint-supersedes-those-of-schema-1 ()
  "A tree's checkpoint names the schema 1 checkpoints of the tree it replaces.
Once, by their hashes: the next names none, and one that covers an
archive is never named."
  (pos-test-with-scope
    (let* ((archive (pos-test-scope-sealed scope))
           (head (pos-history-head (pos-ledger-history archive)))
           (home (expand-file-name ".clankos/archive-integrity/checkpoints"
                                   (file-truename scope)))
           (old (pos-bytes-json `((schema . 1) (heads . [,head]) (coverage . "tree"))))
           (name (pos-bytes-sha old)))
      (pos-test-write-bytes (expand-file-name (concat name ".json") home) old #o444)
      (pos-checkpoint-record scope)
      (should (equal (list (vector name))
                     (mapcar (lambda (value) (alist-get 'supersedes value))
                             (pos-checkpoint-test--tree-checkpoints home))))
      (pos-checkpoint-record scope)
      (should (equal (list [] (vector name))
                     (sort (mapcar (lambda (value) (alist-get 'supersedes value))
                                   (pos-checkpoint-test--tree-checkpoints home))
                           (lambda (a b) (< (length a) (length b)))))))))

(ert-deftest pos-seal/a-child-not-cloned-is-not-a-truncated-history ()
  "A checkpoint of a tree is honoured where a mounted child is not cloned.
The tree mounts the scope by its remote.  With the scope gone, as in a
fresh clone or a worktree, the check passes and names the child as not
present.  A scope that is there without its archive is refused, and so
is a head that a schema 1 checkpoint names, which no schema 2
checkpoint supersedes."
  (pos-test-with-scope
    (let* ((root (file-truename dir))
           (archive (pos-test-scope-sealed scope))
           (head (pos-history-head (pos-ledger-history archive)))
           (away (expand-file-name "_away" root)))
      (pos-test-write-bytes (expand-file-name ".pos/config.yaml" root)
                            "children:\n  - path: scope\n    remote: https://git.example/scope.git\n")
      (let ((home (pos-checkpoint-record root)))
        (should (equal home (expand-file-name ".pos/archive-integrity/checkpoints" root)))
        (should-not (pos-ledger-not-present root))
        (rename-file scope away)
        (should-not (pos-ledger-check root))
        (should (equal (list (expand-file-name "scope" root))
                       (pos-ledger-not-present root)))
        (make-directory scope)
        (pos-test-refused pos-ledger-refused "anchor" (pos-ledger-check root))
        (delete-directory scope)
        (let ((old (pos-bytes-json `((schema . 1) (heads . [,head]) (coverage . "tree")))))
          (pos-test-write-bytes (expand-file-name (concat (pos-bytes-sha old) ".json") home)
                                old #o444))
        (pos-test-refused pos-ledger-refused "anchor" (pos-ledger-check root))
        (rename-file away scope)
        (should-not (pos-checkpoint-findings-p (pos-ledger-check root)))))))

(ert-deftest pos-seal/an-archive-kept-out-of-git-is-held-to-its-ledger ()
  "A checkpoint of a tree is honoured where an uncommitted archive is not there.
The scope is beneath a node and no entry keeps its archive otherwise.
With the archive gone and its ledger left, as a clone or a worktree has
them, the check passes and names the archive as not present.  With an
entry that keeps the archive committed, it is refused."
  (pos-test-with-scope
    (let* ((root (file-truename dir))
           (archive (pos-test-scope-sealed scope))
           (config (expand-file-name ".pos/config.yaml" root)))
      (pos-test-write-bytes config "projects: projects/\n")
      (pos-checkpoint-record root)
      (should-not (pos-ledger-not-present root))
      (pos-test-writable archive)
      (delete-directory archive t)
      (should-not (pos-ledger-check root))
      (should (equal (list archive) (pos-ledger-not-present root)))
      (pos-test-write-bytes config "archives:\n  - scope: scope\n    kept: committed\n")
      (pos-test-refused pos-ledger-refused "anchor" (pos-ledger-check root)))))

(ert-deftest pos-seal/repair-protects-only-verified-evidence ()
  "Repair removes a restored write bit, and refuses changed bytes.
A write bit restored to a sealed file is removed; once a sealed file's
bytes have changed, repair refuses."
  (pos-test-with-scope
    (let* ((archive (pos-test-scope-sealed scope))
           (file (expand-file-name "trial/result.md" archive)))
      (set-file-modes file #o644)
      (should (equal '((repaired . 1) (restored . 0) (unregistered . 0)) (pos-checkpoint-repair scope)))
      (should (zerop (logand (file-modes file) #o222)))
      (set-file-modes file #o644)
      (pos-test-write-bytes file "changed" #o644)
      (should-error (pos-checkpoint-repair scope) :type 'pos-ledger-refused)
      (should (/= 0 (logand (file-modes file) #o222))))))

(ert-deftest pos-seal/repair-restores-recorded-directories ()
  "Restore empty directories lost on checkout without rewriting ledger events."
  (pos-test-with-scope
    (make-directory (expand-file-name "trial/empty/leaf" scope) t)
    (let* ((archive (pos-test-scope-sealed scope))
           (history (pos-ledger-history archive))
           (events (mapcar #'pos-bytes-read (pos-history-files history)))
           (folder (expand-file-name "trial/empty" archive)))
      (delete-directory folder t)
      (should (pos-checkpoint-findings-p (pos-ledger-check scope)))
      (should (equal '((repaired . 0) (restored . 2) (unregistered . 0))
                     (pos-checkpoint-repair scope)))
      (should-not (pos-checkpoint-findings-p (pos-ledger-check scope)))
      (should (equal history (pos-ledger-history archive)))
      (should (equal events (mapcar #'pos-bytes-read (pos-history-files history))))
      (should (equal '((repaired . 0) (restored . 0) (unregistered . 0))
                     (pos-checkpoint-repair scope))))))

(ert-deftest pos-seal/repair-restores-recorded-read-bits ()
  "Git loses restrictive read permissions; restore them after verifying bytes."
  (pos-test-with-scope
    (set-file-modes (expand-file-name "trial/result.md" scope) #o600)
    (let* ((archive (pos-test-scope-sealed scope))
           (file (expand-file-name "trial/result.md" archive)))
      (set-file-modes file #o644)
      (should (equal '((repaired . 1) (restored . 0) (unregistered . 0))
                     (pos-checkpoint-repair scope)))
      (should (= #o400 (pos-ledger-permissions file)))
      (should-not (pos-checkpoint-findings-p (pos-ledger-check scope))))))

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
          (should-error (pos-checkpoint-repair scope) :type 'pos-ledger-refused)
          (should-not (file-exists-p (expand-file-name "a" item)))
          (should-not (file-exists-p (expand-file-name "z/leaf" item))))))))

(ert-deftest pos-seal/a-kept-archive-is-checkpointed-and-repaired-by-its-ledger ()
  "A kept archive is checkpointed and repaired from its ledger alone.
Its files are with its keeper: a checkpoint records its head, and
repair protects its ledger's events and looks for nothing else."
  (pos-fixture-with (pos-fixture "ledger" "report-kept") dir
    (let* ((scope (expand-file-name "projects/a" (file-truename dir)))
           (_ (progn (make-directory (expand-file-name ".clankos" scope))
                     (rename-file (expand-file-name "archive-integrity" scope)
                                  (expand-file-name ".clankos/archive-integrity" scope))))
           (event (car (pos-history-files (pos-ledger-history (pos-test-archived scope))))))
      (should (equal (expand-file-name ".clankos/archive-integrity/checkpoints" scope)
                     (pos-checkpoint-record scope)))
      (set-file-modes event #o644)
      (should (equal '((repaired . 1) (restored . 0) (unregistered . 0)) (pos-checkpoint-repair scope)))
      (should (zerop (logand (file-modes event) #o222))))))

(provide 'pos-checkpoint-test)
;;; pos-checkpoint-test.el ends here
