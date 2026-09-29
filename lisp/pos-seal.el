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
(require 'pos-links)

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

;;;; Links

(defun pos-seal--title (file)
  "Return FILE's title, as `pos-links--title'."
  (pos-links--title file))

(defun pos-seal-rumour (date text)
  "Return (DESTINATION . TEXT), the rumour of TEXT dated DATE.
It is named by its text, so rumours of one target differ in name when
they differ in word."
  (cons (format "rumours/%s-%s.org" date
                (substring (pos-ledger--sha (encode-coding-string text 'utf-8)) 0 12))
        text))

(defun pos-seal--rumour (target scope item date)
  "Return the rumour of TARGET, outside the archive of SCOPE, found sealing ITEM.
As (DESTINATION . TEXT), DESTINATION within the archive, dated DATE."
  (let ((path (file-relative-name target scope)))
    (pos-seal-rumour
     date (concat
           "#+TITLE: Rumour of " path "\n"
           "#+DATE: " date "\n\n"
           "On " date ", sealing =" item "= found a link to =" path
           "=, outside the archive. It was then "
           (pos-links-description target scope)
           ".\n"))))

(defun pos-seal--resolve (source rel archive files collections date &optional written-at)
  "Resolve the links in FILES, the item at SOURCE sealed at REL in ARCHIVE.
Return (ADD LINKS ORIGINALS RUMOURS): the entries of the rewritten files,
every path link with its resolution, the SHA-256 of each file rewritten,
and the rumours to seal first.  COLLECTIONS are the item's; DATE dates
the rumours.  A file's links are read from its directory, or for a file
item, from WRITTEN-AT if given."
  (let* ((scope (file-name-directory archive))
         (base (file-name-as-directory source))
         (inside (lambda (abs) (or (equal abs source) (string-prefix-p base abs))))
         (in-archive (lambda (abs) (if (equal abs source) rel
                                     (concat rel "/" (substring abs (length base))))))
         rumours plans)
    ;; Each file: its links, as (OFFSET TEXT KIND . TO-OR-TARGET).  A
    ;; capsule's are left as written.
    (dolist (pair files)
      (let ((file (cdr pair)) resolved)
        (dolist (link (unless (seq-some (lambda (c)
                                          (and (pos-ledger--within-p (car pair) c)
                                               (pos-ledger-capsule-p
                                                (if (equal c rel) source
                                                  (concat base (substring c (1+ (length rel))))))))
                                        collections)
                        (pos-links-in-file file)))
          (pcase-let* ((`(,offset ,text ,path ,suffix ,whole-offset ,whole) link)
                       (abs (expand-file-name path (if (and written-at (equal file source))
                                                       written-at
                                                     (file-name-directory file)))))
            (push
             (if (funcall inside abs)
                 (let ((target (funcall in-archive abs)))
                   (cond
                    ((seq-some (lambda (c) (and (pos-ledger--within-p (car pair) c)
                                                (pos-ledger--within-p target c)))
                               collections)
                     (list offset text "internal" text))
                    ((assoc target files) (list offset text "cid" :file target suffix))
                    (t (pos-ledger--refuse 'unresolved "Link within the item to %s" abs))))
               (pcase (pos-links-resolve abs scope)
                 (`(cid . ,to) (list offset text "cid" (concat to suffix)))
                 ;; A link to nothing is kept, annotated as broken: the
                 ;; annotation replaces the whole link.
                 (`(broken . ,_)
                  (list whole-offset whole "broken"
                        (pos-links-annotate file link "broken")))
                 (`(rumour . ,target)
                  (let ((rumour (pos-seal--rumour target scope rel date)))
                    (unless (assoc (car rumour) rumours) (push rumour rumours))
                    (list offset text "rumour" :rumour (car rumour) suffix)))))
             resolved)))
        (push (cons (car pair) (nreverse resolved)) plans)))
    (setq rumours (sort rumours (lambda (a b) (string< (car a) (car b)))))
    (let ((rumour-cids (mapcar (lambda (r) (cons (car r) (pos-cid-bytes
                                                          (encode-coding-string (cdr r) 'utf-8))))
                               rumours))
          done links originals add)
      ;; Rewrite files after the files they cite, whose CIDs change with them.
      (while plans
        (let ((ready (seq-find (lambda (plan)
                                 (seq-every-p (lambda (l) (or (not (eq (nth 3 l) :file))
                                                              (assoc (nth 4 l) done)))
                                              (cdr plan)))
                               plans)))
          (unless ready
            (pos-ledger--refuse 'loop "Links loop within the item: %s"
                                (mapconcat #'car plans ", ")))
          (setq plans (delq ready plans))
          (let* ((file (cdr (assoc (car ready) files)))
                 (rewrites
                  (mapcar (lambda (l)
                            (pcase l
                              (`(,offset ,text ,kind :file ,target ,suffix)
                               (list offset text kind
                                     (concat "ipfs://" (cdr (assoc target done)) suffix)))
                              (`(,offset ,text ,kind :rumour ,dest ,suffix)
                               (list offset text kind
                                     (concat "ipfs://" (cdr (assoc dest rumour-cids)) suffix)))
                              (_ l)))
                          (cdr ready)))
                 (bytes (pos-ledger--read file))
                 (changed (seq-filter (lambda (w) (not (equal (nth 1 w) (nth 3 w)))) rewrites))
                 (new (pos-links-rewrite bytes (mapcar (lambda (w) (list (nth 0 w) (nth 1 w)
                                                                         (nth 3 w)))
                                                       changed)))
                 (entry (list (cons 'cid (pos-cid-bytes new))
                              (cons 'mode (logand (pos-ledger--mode file) (lognot #o222)))
                              (cons 'sha256 (pos-ledger--sha new))
                              (cons 'size (length new)))))
            (when changed
              (push (cons (car ready) (pos-ledger--sha bytes)) originals))
            (dolist (w rewrites)
              (push `((file . ,(car ready)) (offset . ,(nth 0 w)) (from . ,(nth 1 w))
                      (kind . ,(nth 2 w)) (to . ,(nth 3 w)))
                    links))
            (push (cons (car ready) (alist-get 'cid entry)) done)
            (push (cons (car ready) entry) add))))
      (list (sort add (lambda (a b) (string< (car a) (car b))))
            (vconcat (sort links (lambda (a b)
                                   (let ((fa (alist-get 'file a)) (fb (alist-get 'file b)))
                                     (if (equal fa fb) (< (alist-get 'offset a) (alist-get 'offset b))
                                       (string< fa fb))))))
            (sort originals (lambda (a b) (string< (car a) (car b))))
            (vconcat (mapcar (lambda (r) `((destination . ,(car r)) (text . ,(cdr r))
                                           (cid . ,(cdr (assoc (car r) rumour-cids)))))
                             rumours))))))

;;;; Plans

(defun pos-seal-plan (source destination &optional ledger-id date as-destination)
  "Return the plan to seal SOURCE at DESTINATION, inside an archive.
LEDGER-ID names a new ledger; by default one is made at random.  DATE,
by default today's, dates any rumours the item's links need.  With
AS-DESTINATION, a file's links are read as written from DESTINATION, not
from where it lies."
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
      (pcase-let* ((`(,known ,head ,events ,files ,_ ,collections ,items)
                    (pos-ledger-history archive))
                   (_ (let ((within (seq-find (lambda (i) (pos-ledger--within-p rel i))
                                              (append items collections))))
                        (when within
                          (pos-ledger--refuse 'sealed "Destination is within sealed %s: %s"
                                              within rel))))
                   (actual (pos-ledger-inventory archive))
                   (`(,missing ,changed ,_)
                    (pos-ledger--differences
                     known (pos-ledger--with-cids known actual (pos-ledger--cids archive)))))
        (when (or missing changed)
          (pos-ledger--refuse 'differs "Existing evidence differs in %s: %S %S"
                              archive missing changed))
        (let ((collections (sort (pos-seal--collections source rel) #'string<)))
          (pcase-let ((`(,add ,links ,originals ,rumours)
                       (pos-seal--resolve source rel archive (pos-seal--files source rel)
                                          collections
                                          (or date (format-time-string "%Y-%m-%d"))
                                          (and as-destination
                                               (file-name-directory destination)))))
            ;; A rumour sealed already, word for word, is cited, not sealed again.
            (setq rumours
                  (vconcat
                   (seq-remove
                    (lambda (rumour)
                      (let ((there (expand-file-name (alist-get 'destination rumour) archive)))
                        (when (file-exists-p there)
                          (unless (equal (pos-ledger--read there)
                                         (encode-coding-string (alist-get 'text rumour) 'utf-8))
                            (pos-ledger--refuse 'destination "Rumour destination exists: %s"
                                                there))
                          t)))
                    rumours)))
            `((schema . 2) (operation . "seal")
              (source . ,source) (destination . ,destination) (archive . ,archive)
              (ledger . ,(pos-seal--ledger-folder archive))
              (number . ,(1+ events)) (previous . ,(or head :null))
              (ledger_id . ,(or (pos-seal--last-id files) ledger-id (pos-seal--uuid)))
              (add . ,add) (collections . ,(vconcat collections))
              (links . ,links) (originals . ,originals) (rumours . ,rumours)
              (inventory_sha256 . ,(pos-ledger--sha (pos-ledger-json actual))))))))))

(defun pos-seal-stage (bytes destination &optional ledger-id)
  "Stage BYTES, a new record, and return the plan to seal them at DESTINATION.
They are staged beside the archive, in _seal/, so the move is one rename,
with DESTINATION's extension, by which their links are found; the links
are read as written from DESTINATION.  If planning fails, nothing is
left staged.
LEDGER-ID names a new ledger, as for `pos-seal-plan'."
  (let* ((archive (or (pos-seal--outermost-archive (expand-file-name destination))
                      (pos-ledger--refuse 'destination "Destination is not in an archive: %s"
                                          destination)))
         (stage (expand-file-name "_seal" (file-name-directory archive))))
    (make-directory stage t)
    (let ((file (make-temp-file (expand-file-name "new-" stage) nil
                               (file-name-extension destination t))))
      (let ((coding-system-for-write 'binary))
        (with-temp-file file
          (set-buffer-multibyte nil)
          (insert bytes)))
      (set-file-modes file #o644)
      (condition-case err
          (pos-seal-plan file destination ledger-id nil t)
        (error
         (delete-file file)
         (when (directory-empty-p stage) (delete-directory stage))
         (signal (car err) (cdr err)))))))

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

(defun pos-seal--event (plan destination add collections)
  "Write the schema 2 event of PLAN sealing ADD at DESTINATION; return its hash."
  (let-alist plan
    (pcase-let* ((`(,_ ,head ,events) (pos-ledger-history .archive))
                 (bytes (pos-ledger-json
                         `((schema . 2) (previous . ,(or head :null)) (ledger_id . ,.ledger_id)
                           (item . ,(file-relative-name destination .archive))
                           (add . ,add) (root . ,(pos-cid-directory .archive))
                           (collections . ,collections))))
                 (hash (pos-ledger--sha bytes)))
      (pos-seal--write-new (expand-file-name (format "%08d-%s.json" (1+ events) hash) .ledger)
                           bytes)
      hash)))

(defun pos-seal--sealed (plan)
  "Return the items PLAN's events have sealed so far, refusing a stranger's.
Events since PLAN's previous head must seal PLAN's rumours, then its item,
in order."
  (let-alist plan
    (let* ((files (nth 3 (pos-ledger-history .archive)))
           (hashes (mapcar (lambda (f) (pos-ledger--sha (pos-ledger--read f))) files))
           (since (if (eq .previous :null) files
                    (let ((at (seq-position hashes .previous)))
                      (unless at
                        (pos-ledger--refuse 'plan "Ledger changed since review: %s" .archive))
                      (nthcdr (1+ at) files))))
           (expected (append (mapcar (lambda (r) (alist-get 'destination r)) .rumours)
                             (list (file-relative-name .destination .archive))))
           (sealed (mapcar (lambda (f) (alist-get 'item (pos-ledger--parse (pos-ledger--read f))))
                           since)))
      (unless (equal sealed (seq-take expected (length sealed)))
        (pos-ledger--refuse 'plan "Ledger changed since review: %s" .archive))
      sealed)))

(defun pos-seal--rewrite-source (plan)
  "Rewrite the links in PLAN's item where it lies, if not done already."
  (let-alist plan
    (let ((rel (file-relative-name .destination .archive)))
      (pcase-dolist (`(,path . ,original) .originals)
        (let* ((path (pos-ledger--key path))
               (file (if (equal path rel) .source
                       (expand-file-name (substring path (1+ (length rel))) .source)))
               (bytes (pos-ledger--read file)))
          (cond
           ((equal (pos-ledger--sha bytes) original)
            (let ((new (pos-links-rewrite
                        bytes (mapcar (lambda (l) (let-alist l (list .offset .from .to)))
                                      (seq-filter (lambda (l)
                                                    (let-alist l
                                                      (and (equal .file path)
                                                           (not (equal .from .to)))))
                                                  .links))))
                  (modes (file-modes file)))
              (let ((coding-system-for-write 'binary))
                (with-temp-file file
                  (set-buffer-multibyte nil)
                  (insert new)))
              (set-file-modes file modes)))
           ((equal (pos-ledger--sha bytes)
                   (alist-get 'sha256 (cdr (assoc path (mapcar (lambda (a) (cons (pos-ledger--key (car a)) (cdr a))) .add))))))
           (t (pos-ledger--refuse 'plan "Item changed since review: %s" file))))))))

(defun pos-seal-apply (plan expected)
  "Apply PLAN, whose canonical JSON has the SHA-256 EXPECTED.
Seal its rumours, then its item, rewriting the item's links first.
Refuse if anything it relied on has changed; resume if interrupted.
Return (EVENT-FILE . ROOT)."
  (unless (equal (pos-ledger--sha (pos-ledger-json plan)) expected)
    (pos-ledger--refuse 'plan "Reviewed plan hash mismatch"))
  (let-alist plan
    (unless (and (eql .schema 2) (equal .operation "seal"))
      (pos-ledger--refuse 'plan "Not a seal plan"))
    (let* ((sealed (pos-seal--sealed plan))
           (rel (file-relative-name .destination .archive))
           (add-of (lambda (paths)
                     (sort (mapcar (lambda (p) (cons p (pos-seal--entry (expand-file-name p .archive))))
                                   paths)
                           (lambda (a b) (string< (car a) (car b)))))))
      (when (and (null sealed) (not (file-exists-p .destination)))
        (unless (equal (pos-ledger--sha (pos-ledger-json (pos-ledger-inventory .archive)))
                       .inventory_sha256)
          (pos-ledger--refuse 'plan "Archive changed since review: %s" .archive)))
      ;; Rumours first: each an item of its own.
      (seq-doseq (rumour .rumours)
        (let-alist rumour
          (let ((there (expand-file-name .destination (alist-get 'archive plan))))
            (unless (member .destination sealed)
              (unless (file-exists-p there)
                (pos-seal--write-new there (encode-coding-string .text 'utf-8)))
              (unless (equal (pos-ledger--read there) (encode-coding-string .text 'utf-8))
                (pos-ledger--refuse 'plan "Rumour differs from its plan: %s" there))
              (pos-seal--event plan there (funcall add-of (list .destination)) [])))))
      ;; Then the item, its links rewritten where it lies, then moved.
      (unless (file-exists-p .destination)
        (unless (file-exists-p .source)
          (pos-ledger--refuse 'plan "Neither before nor after the move: %s" .source))
        (pos-seal--rewrite-source plan)
        (unless (equal (pos-ledger-json .add)
                       (pos-ledger-json
                        (sort (mapcar (lambda (pair)
                                        (cons (car pair) (pos-seal--entry (cdr pair))))
                                      (pos-seal--files .source rel))
                              (lambda (a b) (string< (car a) (car b))))))
          (pos-ledger--refuse 'plan "Item changed since review: %s" .source))
        (make-directory (file-name-directory .destination) t)
        (rename-file .source .destination))
      (unless (equal (pos-ledger-json .add)
                     (pos-ledger-json (funcall add-of (mapcar (lambda (a) (pos-ledger--key (car a)))
                                                              .add))))
        (pos-ledger--refuse 'plan "Item changed after the move: %s" .destination))
      (unless (member rel sealed)
        (pos-seal--event plan .destination .add .collections))
      (dolist (pair .add)
        (pos-seal--protect (expand-file-name (pos-ledger--key (car pair)) .archive)))
      (seq-doseq (rumour .rumours)
        (pos-seal--protect (expand-file-name (alist-get 'destination rumour) .archive)))
      (let ((files (nth 3 (pos-ledger-history .archive))))
        (pos-seal--checkpoint .archive (nth 1 (pos-ledger-history .archive)))
        (cons (car (last files))
              (alist-get 'root (pos-ledger--parse (pos-ledger--read (car (last files))))))))))

;;;; Checkpoints and repair

(defun pos-seal-findings-p (report)
  "Return non-nil if REPORT, from `pos-ledger-check', has any finding."
  (seq-some (lambda (archive)
              (let-alist archive
                (or (eq .head :null)
                    (seq-some (lambda (list) (> (length list) 0))
                              (list .changed .missing .new .writable
                                    .checkpoint_writable .hidden .undeclared))
                    (and (not (eq .recorded_root :null))
                         (not (equal .recorded_root .root))))))
            report))

(defun pos-seal--anchor-home (root)
  "Return the directory for ROOT's new checkpoints: beside it, else legacy."
  (let* ((root (file-truename (pos-ledger--checked root)))
         (base (if (equal (file-name-nondirectory root) "archives")
                   (file-name-directory root)
                 root)))
    (if (file-directory-p (expand-file-name pos-ledger-integrity base))
        (expand-file-name (concat pos-ledger-integrity "/checkpoints") base)
      (expand-file-name pos-ledger-anchors base))))

(defun pos-seal-checkpoint (root)
  "Record the ledger heads of every archive under ROOT; return where.
Refused unless the check is clean of changed, missing and new files and
of write bits.  The checkpoint covers an archive if ROOT is one, else
the tree; one with the same bytes already recorded is kept."
  (let ((report (pos-ledger-check root)))
    (when (seq-some (lambda (archive)
                      (let-alist archive
                        (seq-some (lambda (list) (> (length list) 0))
                                  (list .changed .missing .new .writable
                                        .checkpoint_writable))))
                    report)
      (pos-ledger--refuse 'unclean "Enrol new records and restore permissions \
before checkpointing: %s" root))
    (let* ((heads (sort (seq-uniq (seq-remove (lambda (h) (eq h :null))
                                              (mapcar (lambda (a) (alist-get 'head a))
                                                      report)))
                        #'string<))
           (home (pos-seal--anchor-home root)))
      (when heads
        (let* ((coverage (if (equal (file-name-nondirectory
                                     (directory-file-name (file-truename root)))
                                    "archives")
                             "archive" "tree"))
               (bytes (pos-ledger-json `((schema . 1) (heads . ,(vconcat heads))
                                         (coverage . ,coverage))))
               (file (expand-file-name (concat (pos-ledger--sha bytes) ".json") home)))
          (cond ((not (file-exists-p file)) (pos-seal--write-new file bytes))
                ((not (equal (pos-ledger--read file) bytes))
                 (pos-ledger--refuse 'checkpoint "Checkpoint conflict: %s" file)))))
      home)))

(defun pos-seal-repair (root)
  "Remove write bits from the verified evidence under ROOT.
Enrolled files, ledger events and checkpoints; never enrols.  Refused if
any enrolled file changed or is missing.  Return an alist: repaired, the
files changed, and unregistered, the files the ledgers do not know."
  (let ((report (pos-ledger-check root)) (count 0))
    (when (seq-some (lambda (a) (or (> (length (alist-get 'changed a)) 0)
                                    (> (length (alist-get 'missing a)) 0)))
                    report)
      (pos-ledger--refuse 'differs "Evidence changed or is missing; repair refused: %s"
                          root))
    (let ((protect (lambda (file)
                     (when (pos-ledger--writable-p file)
                       (set-file-modes file (logand (pos-ledger--mode file) (lognot #o222))
                                       'nofollow)
                       (setq count (1+ count))))))
      (dolist (a report)
        (let ((archive (alist-get 'archive a)))
          (pcase-let ((`(,known ,_ ,_ ,files) (pos-ledger-history archive)))
            (dolist (pair known) (funcall protect (expand-file-name (car pair) archive)))
            (mapc protect files))))
      (mapc protect (pos-ledger--checkpoint-files root (pos-ledger-roots root))))
    `((repaired . ,count)
      (unregistered . ,(apply #'+ (mapcar (lambda (a) (length (alist-get 'new a))) report))))))

;;;; Command line

(defconst pos-seal-usage
  "Usage: COMMAND ...  (help prints this; Emacs itself takes --help)

  seal SOURCE DESTINATION [--apply]
      print the plan to seal SOURCE at DESTINATION, in an archive
  write-new DESTINATION [--apply]
      print the plan to seal a new record, read from standard input
  apply PLAN HASH
      apply a reviewed plan, named by its hash
  check ROOT
      report every archive under ROOT, as JSON
  checkpoint ROOT
      record the ledger heads under ROOT, once the check is clean
  repair ROOT
      remove write bits from verified evidence; never enrols

--apply applies a program's own plan at once and prints both.
Exit 0 done or clean, 1 findings, 2 refused.
"
  "The command line's usage.")

(defun pos-seal-batch ()
  "Run a command from `command-line-args-left', as in `pos-seal-usage'."
  (condition-case err
      (pcase (prog1 command-line-args-left (setq command-line-args-left nil))
        (`("seal" ,source ,destination . ,rest)
         (unless (member rest '(nil ("--apply")))
           (message "Usage: seal SOURCE DESTINATION [--apply]")
           (kill-emacs 2))
         (let ((plan (pos-seal-plan source destination)))
           (princ (decode-coding-string
                   (pos-ledger-json
                    (if rest
                        (let* ((hash (pos-ledger--sha (pos-ledger-json plan)))
                               (result (pos-seal-apply plan hash)))
                          `((plan . ,plan) (hash . ,hash)
                            (event . ,(car result)) (root . ,(cdr result))))
                      plan))
                   'utf-8))))
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
        (`("check" ,root)
         (let ((report (vconcat (pos-ledger-check root))))
           (princ (decode-coding-string (pos-ledger-json report) 'utf-8))
           (kill-emacs (if (pos-seal-findings-p report) 1 0))))
        (`("checkpoint" ,root)
         (princ (pos-ledger-json `((checkpointed . ,(pos-seal-checkpoint root))))))
        (`("repair" ,root)
         (princ (pos-ledger-json (pos-seal-repair root))))
        (`(,(or "help" "-h" "--help")) (princ pos-seal-usage))
        (_ (message "%s" pos-seal-usage)
           (kill-emacs 2)))
    (pos-ledger-refused
     (message "%s: %s" (nth 1 err) (nth 2 err))
     (kill-emacs 2))
    (json-error
     (message "plan: Not a readable plan")
     (kill-emacs 2))))

(provide 'pos-seal)
;;; pos-seal.el ends here
