;;; pos-seal.el --- Seal items into archives -*- lexical-binding: t; -*-

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

;; Sealing, as doc/formats.org specifies: an item moves into an archive
;; in one rename, and a schema 2 ledger event enrols it, with its CIDs,
;; the archive's root CID and the collections it holds.  Always two
;; steps: a plan, reviewed, then its application, which refuses if
;; anything the plan relied on has changed and resumes if interrupted.
;;
;; - `pos-seal-plan': the plan to seal SOURCE at DESTINATION.
;; - `pos-seal-stage': stage new bytes, and plan to seal them.
;; - `pos-seal-apply': apply a plan, given its reviewed hash.
;; - `pos-seal-batch': the command line.

;;; Code:

(require 'pos-cid)
(require 'pos-ledger)

;;;; Items

(defun pos-seal--outermost-archive (path)
  "Return the outermost directory named archives above PATH, or nil."
  (let (found (dir (file-name-directory (directory-file-name path))))
    (while (and dir (not (equal dir (file-name-directory (directory-file-name dir)))))
      (when (equal (file-name-nondirectory (directory-file-name dir)) "archives")
        (setq found (directory-file-name dir)))
      (setq dir (file-name-directory (directory-file-name dir))))
    found))

(defun pos-seal--files (source rel)
  "Return SOURCE's files as (PATH . FILE), refusing hidden and special ones.
PATH is relative to the archive, from REL for SOURCE."
  (cond
   ((string-prefix-p "." (file-name-nondirectory source))
    (pos-ledger--refuse 'hidden "Hidden files are not sealed: %s" source))
   ((file-symlink-p source)
    (pos-ledger--refuse 'link "Symlink in item: %s" source))
   ((file-directory-p source)
    (mapcan (lambda (name)
              (pos-seal--files (expand-file-name name source) (concat rel "/" name)))
            (pos-ledger--entries source)))
   (t (pos-ledger--regular source)
      (list (cons rel source)))))

(defun pos-seal--collections (source rel)
  "Return the collections in SOURCE, as paths relative to the archive from REL."
  (when (file-directory-p source)
    (append (when (pos-ledger--declared-p source) (list rel))
            (mapcan (lambda (name)
                      (pos-seal--collections (expand-file-name name source)
                                             (concat rel "/" name)))
                    (pos-ledger--entries source)))))

(defun pos-seal--entry (file)
  "Return FILE's schema 2 ledger entry."
  (cons (cons 'cid (pos-cid-file file)) (pos-ledger-record file)))

(defun pos-seal--uuid ()
  "Return a random UUID, version 4, in canonical form."
  (let* ((hex (secure-hash 'sha256 (format "%S%S%S" (random t) (current-time) (emacs-pid))))
         (variant (aref "89ab" (% (string-to-number (substring hex 16 17) 16) 4))))
    (format "%s-%s-4%s-%c%s-%s" (substring hex 0 8) (substring hex 8 12)
            (substring hex 13 16) variant (substring hex 17 20) (substring hex 20 32))))

(defun pos-seal--ledger-folder (archive)
  "Return where ARCHIVE's ledger is, or for a new one, beside the archive."
  (let ((folder (pos-ledger-folder archive)))
    (if (or (file-exists-p folder) (file-symlink-p folder))
        folder
      (expand-file-name (concat pos-ledger-integrity "/ledger")
                        (file-name-directory archive)))))

(defun pos-seal--last-id (files)
  "Return the ledger_id of the last of the event FILES that has one."
  (seq-some (lambda (file) (alist-get 'ledger_id (pos-ledger--parse (pos-ledger--read file))))
            (reverse files)))

;;;; Plans

(defun pos-seal-plan (source destination &optional ledger-id)
  "Return the plan to seal SOURCE at DESTINATION, inside an archive.
LEDGER-ID names a new ledger; by default one is made at random."
  (let* ((source (directory-file-name (file-truename (pos-ledger--checked source))))
         (destination (directory-file-name (expand-file-name destination)))
         (archive (pos-seal--outermost-archive destination)))
    (pos-ledger--checked (file-name-directory destination))
    (unless archive
      (pos-ledger--refuse 'destination "Destination is not in an archive: %s" destination))
    (let ((rel (file-relative-name destination archive)))
      (setq archive (file-truename archive)
            destination (expand-file-name rel archive)))
    (when (pos-seal--outermost-archive (concat source "/"))
      (pos-ledger--refuse 'source "Source is already archived: %s" source))
    (unless (or (file-exists-p source) (file-symlink-p source))
      (pos-ledger--refuse 'source "No such item: %s" source))
    (when (or (file-exists-p destination) (file-symlink-p destination))
      (pos-ledger--refuse 'destination "Destination exists: %s" destination))
    (let ((rel (file-relative-name destination archive)))
      (unless (pos-ledger--safe-p rel)
        (pos-ledger--refuse 'destination "Unsafe destination: %s" rel))
      (when (seq-some (lambda (part) (string-prefix-p "." part)) (split-string rel "/"))
        (pos-ledger--refuse 'hidden "Hidden files are not sealed: %s" rel))
      (pcase-let* ((`(,known ,head ,events ,files) (pos-ledger-history archive))
                   (actual (pos-ledger-inventory archive))
                   (`(,missing ,changed ,_)
                    (pos-ledger--differences
                     known (pos-ledger--with-cids known actual (pos-ledger--cids archive)))))
        (when (or missing changed)
          (pos-ledger--refuse 'differs "Existing evidence differs in %s: %S %S"
                              archive missing changed))
        `((schema . 1) (operation . "seal")
          (source . ,source) (destination . ,destination) (archive . ,archive)
          (ledger . ,(pos-seal--ledger-folder archive))
          (number . ,(1+ events)) (previous . ,(or head :null))
          (ledger_id . ,(or (pos-seal--last-id files) ledger-id (pos-seal--uuid)))
          (add . ,(sort (mapcar (lambda (pair) (cons (car pair) (pos-seal--entry (cdr pair))))
                                (pos-seal--files source rel))
                        (lambda (a b) (string< (car a) (car b)))))
          (collections . ,(vconcat (sort (pos-seal--collections source rel) #'string<)))
          (inventory_sha256 . ,(pos-ledger--sha (pos-ledger-json actual))))))))

(defun pos-seal-stage (bytes destination &optional ledger-id)
  "Stage BYTES, a new record, and return the plan to seal them at DESTINATION.
They are staged beside the archive, in _seal/, so the move is one rename.
LEDGER-ID names a new ledger, as for `pos-seal-plan'."
  (let* ((archive (or (pos-seal--outermost-archive (expand-file-name destination))
                      (pos-ledger--refuse 'destination "Destination is not in an archive: %s"
                                          destination)))
         (stage (expand-file-name "_seal" (file-name-directory archive))))
    (make-directory stage t)
    (let ((file (make-temp-file (expand-file-name "new-" stage))))
      (let ((coding-system-for-write 'binary))
        (with-temp-file file
          (set-buffer-multibyte nil)
          (insert bytes)))
      (set-file-modes file #o644)
      (pos-seal-plan file destination ledger-id))))

;;;; Application

(defun pos-seal--protect (file)
  "Remove FILE's write bits."
  (set-file-modes file (logand (pos-ledger--mode file) (lognot #o222)) 'nofollow))

(defun pos-seal--write-new (file bytes)
  "Publish BYTES at FILE, which must not exist, read-only for all."
  (make-directory (file-name-directory file) t)
  (let ((temp (make-temp-file (expand-file-name "_integrity-" (file-name-directory file)))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'binary))
            (with-temp-file temp
              (set-buffer-multibyte nil)
              (insert bytes)))
          (set-file-modes temp #o444)
          (add-name-to-file temp file))
      (delete-file temp))))

(defun pos-seal--entries-of (plan)
  "Return the entries the item now at PLAN's destination has."
  (let-alist plan
    (mapcar (lambda (pair)
              (let ((path (pos-ledger--key (car pair))))
                (cons path (pos-seal--entry (expand-file-name path .archive)))))
            .add)))

(defun pos-seal--checkpoint (archive head)
  "Record HEAD for ARCHIVE in a checkpoint beside it."
  (let* ((bytes (pos-ledger-json `((schema . 1) (heads . [,head]) (coverage . "archive"))))
         (base (file-name-directory archive))
         (folder (if (file-directory-p (expand-file-name pos-ledger-integrity base))
                     (expand-file-name (concat pos-ledger-integrity "/checkpoints") base)
                   (expand-file-name pos-ledger-anchors base)))
         (file (expand-file-name (concat (pos-ledger--sha bytes) ".json") folder)))
    (unless (file-exists-p file)
      (pos-seal--write-new file bytes))))

(defun pos-seal-apply (plan expected)
  "Apply PLAN, whose canonical JSON has the SHA-256 EXPECTED.
Refuse if anything it relied on has changed; resume if interrupted.
Return (EVENT-FILE . ROOT)."
  (unless (equal (pos-ledger--sha (pos-ledger-json plan)) expected)
    (pos-ledger--refuse 'plan "Reviewed plan hash mismatch"))
  (let-alist plan
    (unless (and (eql .schema 1) (equal .operation "seal"))
      (pos-ledger--refuse 'plan "Not a seal plan"))
    (let ((moved (and (not (file-exists-p .source)) (file-exists-p .destination))))
      (unless moved
        (unless (and (file-exists-p .source) (not (file-exists-p .destination)))
          (pos-ledger--refuse 'plan "Neither before nor after the move: %s" .source))
        (unless (equal (pos-ledger--sha (pos-ledger-json (pos-ledger-inventory .archive)))
                       .inventory_sha256)
          (pos-ledger--refuse 'plan "Archive changed since review: %s" .archive))
        (let ((rel (file-relative-name .destination .archive)))
          (unless (equal (pos-ledger-json .add)
                         (pos-ledger-json
                          (sort (mapcar (lambda (pair)
                                          (cons (car pair) (pos-seal--entry (cdr pair))))
                                        (pos-seal--files .source rel))
                                (lambda (a b) (string< (car a) (car b))))))
            (pos-ledger--refuse 'plan "Item changed since review: %s" .source)))
        (make-directory (file-name-directory .destination) t)
        (rename-file .source .destination))
      (unless (equal (pos-ledger-json .add) (pos-ledger-json (pos-seal--entries-of plan)))
        (pos-ledger--refuse 'plan "Item changed after the move: %s" .destination))
      (let* ((root (pos-cid-directory .archive))
             (bytes (pos-ledger-json
                     `((schema . 2) (previous . ,.previous) (ledger_id . ,.ledger_id)
                       (add . ,.add) (root . ,root) (collections . ,.collections))))
             (hash (pos-ledger--sha bytes))
             (file (expand-file-name (format "%08d-%s.json" .number hash) .ledger)))
        (pcase-let ((`(,_ ,head ,events) (pos-ledger-history .archive)))
          (cond ((and (equal head hash) (eql events .number)))
                ((and (equal (or head :null) .previous) (eql (1+ events) .number))
                 (pos-seal--write-new file bytes))
                (t (pos-ledger--refuse 'plan "Ledger changed since review: %s" .archive))))
        (dolist (pair .add)
          (pos-seal--protect (expand-file-name (pos-ledger--key (car pair)) .archive)))
        (pos-seal--checkpoint .archive hash)
        (cons file root)))))

;;;; Command line

(defun pos-seal-batch ()
  "Run a seal command from `command-line-args-left'.
seal SOURCE DESTINATION and write-new DESTINATION, the record on
standard input, print a plan; apply PLAN HASH applies it.  A program
writing records itself uses write-new DESTINATION --apply, which
applies its own plan at once and prints it with the result.  Exit 0
done, 2 refused."
  (condition-case err
      (pcase (prog1 command-line-args-left (setq command-line-args-left nil))
        (`("seal" ,source ,destination)
         (princ (decode-coding-string (pos-ledger-json (pos-seal-plan source destination))
                                      'utf-8)))
        (`("write-new" ,destination . ,rest)
         (unless (member rest '(nil ("--apply")))
           (message "Usage: write-new DESTINATION [--apply]")
           (kill-emacs 2))
         (let* ((bytes (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally "/dev/stdin")
                         (buffer-string)))
                (plan (pos-seal-stage bytes destination)))
           (princ (decode-coding-string
                   (pos-ledger-json
                    (if rest
                        (let* ((hash (pos-ledger--sha (pos-ledger-json plan)))
                               (result (pos-seal-apply plan hash)))
                          `((plan . ,plan) (hash . ,hash)
                            (event . ,(car result)) (root . ,(cdr result))))
                      plan))
                   'utf-8))))
        (`("apply" ,plan-file ,hash)
         (let* ((plan (pos-ledger--parse (pos-ledger--read plan-file)))
                (result (pos-seal-apply plan hash)))
           (princ (decode-coding-string
                   (pos-ledger-json `((event . ,(car result)) (root . ,(cdr result))))
                   'utf-8))))
        (_ (message "Usage: seal SOURCE DESTINATION | write-new DESTINATION [--apply] | apply PLAN HASH")
           (kill-emacs 2)))
    (pos-ledger-refused
     (message "%s: %s" (nth 1 err) (nth 2 err))
     (kill-emacs 2))
    (json-error
     (message "plan: Not a readable plan")
     (kill-emacs 2))))

(provide 'pos-seal)
;;; pos-seal.el ends here
