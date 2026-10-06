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
;; in one rename, and a ledger event enrols it, with its CIDs, the
;; archive's root CID and the collections it holds: a schema 3 event, a
;; DAG-JSON block named by its CID, unless the ledger is still of schema
;; 1 or 2, which `pos-seal-convert' brings to schema 3.  Always two
;; steps: a plan, reviewed, then its application, which refuses if
;; anything the plan relied on has changed and resumes if interrupted.
;;
;; An archive a keeper keeps is sealed to its keeper, over
;; doc/remote-archive-protocol.txt: each event is sent with its files,
;; written to the ledger once the keeper has it, and the item is then
;; removed from where it lay.
;;
;; - `pos-seal-plan': the plan to seal SOURCE at DESTINATION.
;; - `pos-seal-stage': stage new bytes, and plan to seal them.
;; - `pos-seal-apply': apply a plan, given its reviewed hash.
;; - `pos-seal-convert': bring schema 2 ledgers to schema 3.
;; - `pos-seal-keep': move archives on disk to their keepers.
;; - `pos-seal-batch': the command line.

;;; Code:

(require 'pos-cid)
(require 'pos-ledger)
(require 'pos-links)
(require 'pos-remote)

(declare-function pos-index-bytes "pos-index" (uri &optional directory))

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
  (pos-ledger-identity files))

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
LEDGER-ID names a new ledger; by default it takes the id its scope's
entry names, else one made at random.  DATE, by default today's, dates
any rumours the item's links need.  With AS-DESTINATION, a file's links
are read as written from DESTINATION, not from where it lies."
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
                   (named (pos-ledger-named archive))
                   (_ (pos-ledger--as-named archive files))
                   (_ (when (and named ledger-id (not (equal named ledger-id)))
                        (pos-ledger--refuse
                         'identity "Not the ledger its entry names, %s: %s" named archive)))
                   (kept (pos-ledger-kept archive))
                   (_ (when (and kept head (not (pos-ledger--event-cid-p head)))
                        (pos-ledger--refuse
                         'kept "A keeper keeps a ledger of schema 3; convert this one first: %s"
                         archive)))
                   ;; Nothing of a kept archive is on disk to compare: what
                   ;; it holds is what its ledger enrols.
                   (actual (if kept (pos-ledger--kept-entries archive known)
                             (pos-ledger-inventory archive)))
                   (`(,missing ,changed ,_)
                    (unless kept
                      (pos-ledger--differences
                       known
                       (pos-ledger--with-cids known actual (pos-ledger--cids archive))))))
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
                      (let ((there (expand-file-name (alist-get 'destination rumour) archive))
                            (enrolled (and kept (assoc (alist-get 'destination rumour)
                                                       known))))
                        (cond
                         (enrolled
                          (unless (equal (alist-get 'cid (cdr enrolled))
                                         (alist-get 'cid rumour))
                            (pos-ledger--refuse 'destination "Rumour destination exists: %s"
                                                there))
                          t)
                         ((file-exists-p there)
                          (unless (equal (pos-ledger--read there)
                                         (encode-coding-string (alist-get 'text rumour) 'utf-8))
                            (pos-ledger--refuse 'destination "Rumour destination exists: %s"
                                                there))
                          t))))
                    rumours)))
            `((schema . 2) (operation . "seal")
              (source . ,source) (destination . ,destination) (archive . ,archive)
              (ledger . ,(pos-seal--ledger-folder archive))
              (number . ,(1+ events)) (previous . ,(or head :null))
              (ledger_id . ,(or (pos-seal--last-id files) named ledger-id (pos-seal--uuid)))
              (add . ,add) (collections . ,(vconcat collections))
              (links . ,links) (originals . ,originals) (rumours . ,rumours)
              (inventory_sha256 . ,(pos-ledger--sha (pos-ledger-json actual)))
              ,@(when kept `((kept . ,kept))))))))))

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

(defun pos-seal--empty-under (top rel)
  "Return the directories at or under TOP holding nothing IPFS would add.
As paths from REL for TOP; see `pos-seal--empty'."
  (when (and (file-directory-p top) (not (file-symlink-p top)))
    (let ((names (seq-remove (lambda (name) (string-prefix-p "." name))
                             (pos-ledger--entries top))))
      (if names
          (mapcan (lambda (name)
                    (pos-seal--empty-under (expand-file-name name top)
                                           (if (string-empty-p rel) name
                                             (concat rel "/" name))))
                  names)
        (unless (string-empty-p rel) (list rel))))))

(defun pos-seal--empty (top rel)
  "Return the empty directories at or under TOP, as sorted paths from REL.
Hidden entries count for nothing and hidden directories are not entered,
as IPFS leaves both out."
  (sort (pos-seal--empty-under top rel) #'string<))

(defun pos-seal--staging-p (dir)
  "Return non-nil if DIR is a staging directory left empty: _seal, holding nothing."
  (and (equal (file-name-nondirectory (directory-file-name dir)) "_seal")
       (file-directory-p dir) (not (file-symlink-p dir))
       (directory-empty-p dir)))

(defun pos-seal--leave-stage (source)
  "Remove the directory SOURCE lay in, if a staging directory left empty.
A record staged by write-new leaves it so."
  (let ((stage (file-name-directory (directory-file-name source))))
    (when (pos-seal--staging-p stage) (delete-directory stage))))

(defun pos-seal--drop-staging (top)
  "Remove each staging directory left empty under TOP, an item.
Hidden directories and nested archives are not entered, nor links followed."
  (when (and (file-directory-p top) (not (file-symlink-p top)))
    (dolist (name (pos-ledger--entries top))
      (let ((path (expand-file-name name top)))
        (cond ((or (string-prefix-p "." name) (equal name "archives")))
              ((pos-seal--staging-p path) (delete-directory path))
              (t (pos-seal--drop-staging path)))))))

(defun pos-seal--event-of (plan item add collections empty)
  "Return the event of PLAN sealing ADD as ITEM, as (FILE NAME BYTES).
FILE is its ledger file's name and NAME what names it; COLLECTIONS and
EMPTY are the item's collections and empty directories.
A ledger with no event yet, or one whose head is a block, takes a schema 3
event: a DAG-JSON block named by its CID, its root the fold of what the
ledger enrols.  A schema 1 or 2 ledger takes a schema 2 event, named by
its hash, until it is converted."
  (let-alist plan
    (pcase-let* ((`(,entries ,head ,events ,_ ,_ ,_ ,_ ,empties)
                  (pos-ledger-history .archive))
                 (blocks (or (null head) (pos-ledger--event-cid-p head)))
                 (bytes
                  (if blocks
                      (pos-ledger-block
                       `((schema . 3)
                         (previous . ,(if head (pos-ledger--link head) :null))
                         (ledger_id . ,.ledger_id) (item . ,item) (add . ,add)
                         (root . ,(cdr (assoc "." (pos-ledger-fold
                                                   (append add entries)
                                                   (append empty empties)))))
                         (collections . ,collections) (empty . ,(vconcat empty))))
                    (pos-ledger-json
                     `((schema . 2) (previous . ,(or head :null)) (ledger_id . ,.ledger_id)
                       (item . ,item)
                       (add . ,add) (root . ,(pos-cid-directory .archive))
                       (collections . ,collections)))))
                 (name (if blocks (pos-ledger--event-cid bytes) (pos-ledger--sha bytes))))
      (list (format "%08d-%s.json" (1+ events) name) name bytes))))

(defun pos-seal--event (plan destination add collections)
  "Write the event of PLAN sealing ADD at DESTINATION; return what names it.
COLLECTIONS are the item's."
  (let* ((item (file-relative-name destination (alist-get 'archive plan)))
         (event (pos-seal--event-of plan item add collections
                                    (pos-seal--empty destination item))))
    (pos-seal--write-new (expand-file-name (nth 0 event) (alist-get 'ledger plan))
                         (nth 2 event))
    (nth 1 event)))

(defun pos-seal--sealed (plan)
  "Return the items PLAN's events have sealed so far, refusing a stranger's.
Events since PLAN's previous head must seal PLAN's rumours, then its item,
in order."
  (let-alist plan
    (let* ((files (nth 3 (pos-ledger-history .archive)))
           (hashes (mapcar (lambda (f) (substring (file-name-nondirectory f) 9 -5)) files))
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
    (unless (equal .kept (pos-ledger-kept .archive))
      (pos-ledger--refuse 'plan "The archive is not kept as planned: %s" .archive))
    (if .kept
        (pos-seal--apply-kept plan expected)
      (pos-seal--apply-on-disk plan))))

(defun pos-seal--apply-on-disk (plan)
  "Apply PLAN, already checked, to an archive on disk.
Return (EVENT-FILE . ROOT)."
  (let-alist plan
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
        (pos-seal--drop-staging .source)
        (unless (equal (pos-ledger-json .add)
                       (pos-ledger-json
                        (sort (mapcar (lambda (pair)
                                        (cons (car pair) (pos-seal--entry (cdr pair))))
                                      (pos-seal--files .source rel))
                              (lambda (a b) (string< (car a) (car b))))))
          (pos-ledger--refuse 'plan "Item changed since review: %s" .source))
        (make-directory (file-name-directory .destination) t)
        (rename-file .source .destination))
      (pos-seal--leave-stage .source)
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

;;;; Sealing to a keeper

(defvar pos-seal-claims-function #'pos-seal-claims
  "The function a seal's claims are made with, given its plan and hash.")

(defun pos-seal-claims (plan expected)
  "Return the claims of this client about the seal of PLAN.
An alist of strings by name, for a keeper to record: the plan's hash
EXPECTED, where there was a plan, and the tool, and of the scope's
repository, where there is one and git reads it, the scope's path, the
commit and branch it is at, whether its working tree is dirty, and each
remote's URL less any user and password."
  (let* ((scope (file-name-directory (alist-get 'archive plan)))
         (root (locate-dominating-file scope ".git"))
         (claims `(,@(when expected `(("plan" . ,expected))) ("tool" . "poslib"))))
    (when root
      (setq root (directory-file-name (expand-file-name root)))
      (let* ((process-environment
              (cons (concat "GIT_CEILING_DIRECTORIES=" (file-name-directory root))
                    process-environment))
             (git (lambda (&rest args)
                    (with-temp-buffer
                      (and (eq 0 (ignore-errors
                                   (apply #'process-file "git" nil (list t nil) nil
                                          "-C" root args)))
                           (string-trim-right (buffer-string) "\n")))))
             (commit (funcall git "rev-parse" "--verify" "-q" "HEAD"))
             (branch (funcall git "symbolic-ref" "--short" "-q" "HEAD"))
             (status (funcall git "status" "--porcelain"))
             (remotes (funcall git "config" "--get-regexp" "^remote\\..*\\.url$")))
        (push (cons "scope" (directory-file-name (file-relative-name scope root))) claims)
        (when (and commit (not (string-empty-p commit))) (push (cons "commit" commit) claims))
        (when (and branch (not (string-empty-p branch))) (push (cons "branch" branch) claims))
        (when status
          (push (cons "dirty" (if (string-empty-p status) "false" "true")) claims))
        (dolist (line (and remotes (split-string remotes "\n" t)))
          (when (string-match "\\`\\(remote\\..*\\)\\.url \\(.*\\)\\'" line)
            (push (cons (match-string 1 line)
                        (replace-regexp-in-string
                         "\\`\\([a-z][a-z0-9+.-]*://\\)[^/@]*@" "\\1"
                         (match-string 2 line)))
                  claims)))))
    claims))

(defun pos-seal--catch-up (archive ledger keeper)
  "Bring ARCHIVE's ledger, in LEDGER, up to date with KEEPER.
A seal interrupted between the keeper's answer and the ledger's file
leaves the keeper an event ahead: the events the ledger lacks are
fetched and written.  Refuse `chain', with the ledger as it was, unless
the keeper's events continue this ledger's and end at the keeper's
head."
  (let* ((described (pos-remote-describe keeper))
         (theirs (alist-get 'events described))
         (head (let ((h (alist-get 'head described))) (unless (eq h :null) h)))
         (events (nth 2 (pos-ledger-history archive)))
         written)
    (condition-case err
        (progn
          (dotimes (i (max 0 (- theirs events)))
            (let* ((number (+ events i 1))
                   (bytes (pos-remote-event keeper number))
                   (file (expand-file-name
                          (format "%08d-%s.json" number (pos-ledger--event-cid bytes))
                          ledger)))
              (pos-seal--write-new file bytes)
              (push file written)))
          (let ((caught (condition-case nil
                            (pos-ledger-history archive)
                          (pos-ledger-refused nil))))
            (unless (and caught (equal head (nth 1 caught)) (equal theirs (nth 2 caught)))
              (pos-ledger--refuse 'chain "The keeper holds another ledger than this one: %s"
                                  archive))))
      (pos-ledger-refused
       (mapc #'delete-file written)
       (when (and (zerop events) (file-directory-p ledger)
                  (null (pos-ledger--entries ledger)))
         (delete-directory ledger))
       (signal (car err) (cdr err))))))

(defun pos-seal--apply-kept (plan expected)
  "Apply PLAN, whose hash is EXPECTED, to an archive kept by a keeper.
Each event is sent with its files, written to the ledger once the
keeper has it, and the item is then removed from where it lay.
Interrupted, it resumes.  Return (EVENT-FILE . ROOT)."
  (let-alist plan
    (let* ((keeper (funcall pos-remote-keeper-function .kept))
           (claims (funcall pos-seal-claims-function plan expected))
           (rel (file-relative-name .destination .archive))
           (_ (pos-seal--catch-up .archive .ledger keeper))
           (sealed (pos-seal--sealed plan))
           (entries-of
            (lambda ()
              (sort (mapcar (lambda (pair) (cons (car pair) (pos-seal--entry (cdr pair))))
                            (pos-seal--files .source rel))
                    (lambda (a b) (string< (car a) (car b))))))
           (send
            (lambda (item add collections empty files)
              (pcase-let ((`(,file ,_ ,bytes)
                           (pos-seal--event-of plan item add collections empty)))
                (pos-remote-append keeper file bytes files claims)
                (pos-seal--write-new (expand-file-name file .ledger) bytes)))))
      (when (and (null sealed)
                 (not (equal (pos-ledger--sha
                              (pos-ledger-json
                               (pos-ledger--kept-entries
                                .archive (car (pos-ledger-history .archive)))))
                             .inventory_sha256)))
        (pos-ledger--refuse 'plan "Archive changed since review: %s" .archive))
      ;; Rumours first: each an item of its own, of the one file its text is.
      (seq-doseq (rumour .rumours)
        (let-alist rumour
          (unless (member .destination sealed)
            (let* ((bytes (encode-coding-string .text 'utf-8))
                   (cid (pos-cid-bytes bytes)))
              (funcall send .destination
                       `((,.destination . ((cid . ,cid) (mode . #o444)
                                           (sha256 . ,(pos-ledger--sha bytes))
                                           (size . ,(length bytes)))))
                       [] nil (list (cons cid bytes)))))))
      ;; Then the item, its links rewritten where it lies, and sent.
      (unless (member rel sealed)
        (unless (or (file-exists-p .source) (file-symlink-p .source))
          (pos-ledger--refuse 'plan "The item is not where it was planned: %s" .source))
        (pos-seal--rewrite-source plan)
        (pos-seal--drop-staging .source)
        (let ((entries (funcall entries-of)))
          (unless (equal (pos-ledger-json .add) (pos-ledger-json entries))
            (pos-ledger--refuse 'plan "Item changed since review: %s" .source))
          (funcall send rel .add .collections (pos-seal--empty .source rel)
                   (seq-uniq
                    (mapcar (lambda (pair)
                              (cons (alist-get 'cid (cdr (assoc (car pair) entries)))
                                    (pos-ledger--read (cdr pair))))
                            (pos-seal--files .source rel))
                    (lambda (a b) (equal (car a) (car b)))))))
      ;; The keeper has the item and the ledger says so: the copy here goes.
      (when (or (file-exists-p .source) (file-symlink-p .source))
        (unless (equal (pos-ledger-json .add) (pos-ledger-json (funcall entries-of)))
          (pos-ledger--refuse 'plan "Item changed since it was sealed: %s" .source))
        (if (file-directory-p .source)
            (delete-directory .source t)
          (delete-file .source)))
      (pos-seal--leave-stage .source)
      (pcase-let ((`(,_ ,head ,_ ,files) (pos-ledger-history .archive)))
        (pos-seal--checkpoint .archive head)
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
                                    .hidden .undeclared))
                    (and (not (eq .recorded_root :null))
                         (not (equal .recorded_root .root)))
                    ;; A keeper asked, whose head is not the ledger's.
                    (and (not (eq .keeper :null))
                         (not (equal (alist-get 'head .keeper) .head))))))
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
                                  (list .changed .missing .new .writable))))
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

(defun pos-seal--missing-directories (archive known recorded empty)
  "Return missing directories in ARCHIVE needed by the ledger's EMPTY paths.
KNOWN entries and EMPTY must fold to RECORDED.  Check every existing
component before returning paths in parent-before-child order."
  (when empty
    (unless (equal recorded (cdr (assoc "." (pos-ledger-fold known empty))))
      (pos-ledger--refuse 'root "Ledger cannot reconstruct the recorded root: %s" archive))
    (let (missing)
      (dolist (name empty)
        (let ((path archive))
          (dolist (part (split-string name "/"))
            (setq path (expand-file-name part path))
            (cond
             ((or (file-symlink-p path)
                  (and (file-exists-p path) (not (file-directory-p path))))
              (pos-ledger--refuse 'differs "Recorded directory is obstructed: %s" path))
             ((not (file-exists-p path))
              (unless (member path missing) (push path missing)))))))
      (nreverse missing))))

(defun pos-seal--read-bits-only-p (file entry)
  "Return non-nil if FILE differs from recorded ENTRY only in read or write bits."
  (let ((actual (if (assq 'cid entry) (pos-seal--entry file)
                  (pos-ledger-record file))))
    (seq-every-p
     (lambda (pair)
       (let ((now (alist-get (car pair) actual)))
         (if (eq (car pair) 'mode)
             (equal (logand now (lognot #o444)) (logand (cdr pair) (lognot #o444)))
           (equal now (cdr pair)))))
     entry)))

(defun pos-seal-repair (root)
  "Restore recorded empty directories and protect verified evidence under ROOT.
Never enrols or rewrites a ledger.  Restores recorded read bits, but refuses
changed bytes or other mode bits, and missing files.
Return an alist: repaired, the files protected; restored,
the directories created; and unregistered, the files the ledgers do not know."
  (let ((report (pos-ledger-check root)) (count 0) (restored 0) directories)
    (when (seq-some (lambda (a) (> (length (alist-get 'missing a)) 0)) report)
      (pos-ledger--refuse 'differs "Evidence changed or is missing; repair refused: %s"
                          root))
    ;; Validate every archive and destination before changing any of them.
    (dolist (a report)
      (when (eq (alist-get 'kept a) :null)
        (let* ((archive (alist-get 'archive a))
               (history (pos-ledger-history archive)))
          (seq-doseq (name (alist-get 'changed a))
            (unless (pos-seal--read-bits-only-p
                     (expand-file-name name archive) (cdr (assoc name (nth 0 history))))
              (pos-ledger--refuse 'differs "Evidence changed; repair refused: %s" archive)))
          (setq directories
                (append directories
                        (pos-seal--missing-directories
                         archive (nth 0 history) (nth 4 history) (nth 7 history)))))))
    (dolist (directory directories)
      (make-directory directory)
      (setq restored (1+ restored)))
    (let ((protect (lambda (file &optional recorded-mode)
                     (pos-ledger--regular file)
                     (let* ((mode (pos-ledger--mode file))
                            (target (or recorded-mode (logand mode (lognot #o222)))))
                       (unless (= mode target)
                         (set-file-modes file target 'nofollow)
                         (setq count (1+ count)))))))
      (dolist (a report)
        (let ((archive (alist-get 'archive a)))
          (pcase-let ((`(,known ,_ ,_ ,files) (pos-ledger-history archive)))
            ;; A keeper holds a kept archive's files; only its ledger is here.
            (when (eq (alist-get 'kept a) :null)
              (dolist (pair known)
                (funcall protect (expand-file-name (car pair) archive)
                         (alist-get 'mode (cdr pair)))))
            (mapc protect files))))
      (mapc protect (pos-ledger--checkpoint-files root (pos-ledger-roots root))))
    `((repaired . ,count) (restored . ,restored)
      (unregistered . ,(apply #'+ (mapcar (lambda (a) (length (alist-get 'new a))) report))))))

;;;; Conversion

(defun pos-seal-convert (root)
  "Bring each schema 2 ledger under ROOT to schema 3, by one event.
The event links the head as a block, names the hash it had, and enrols
the empty directories the archive holds, so that the fold of the ledger
is the archive's CID.  Refused unless every archive is as its ledger
says, before any event is written.  Return an alist: converted, each an
archive with its event file and new head; and skipped, each an archive
with the reason."
  (let (converted skipped pending)
    (dolist (archive (pos-ledger-roots root))
      (pcase-let* ((`(,entries ,head ,events ,files ,recorded)
                    (pos-ledger-history archive))
                   (reason
                    (cond ((null head) "no ledger")
                          ((pos-ledger--event-cid-p head) "schema 3")
                          ((seq-some (lambda (e) (not (assq 'cid (cdr e)))) entries)
                           "legacy entries"))))
        (if reason
            (push `((archive . ,archive) (reason . ,reason)) skipped)
          (let* ((cids (condition-case err
                           (pos-cid-tree archive)
                         (pos-cid-sharding-unsupported
                          (pos-ledger--refuse 'sharding-unsupported "%S" (cdr err)))))
                 (actual (pos-ledger--with-cids
                          entries (pos-ledger-inventory archive) cids)))
            (pcase-let ((`(,missing ,changed ,new)
                         (pos-ledger--differences entries actual)))
              (when (or missing changed new
                        (not (equal recorded (cdr (assoc "." cids)))))
                (pos-ledger--refuse
                 'unclean "The archive is not as its ledger says; check it first: %s"
                 archive)))
            (let* ((empty (pos-seal--empty archive ""))
                   (folded (cdr (assoc "." (pos-ledger-fold entries empty)))))
              (unless (equal folded recorded)
                (pos-ledger--refuse
                 'root "The ledger does not account for the archive's CID: %s" archive))
              (let* ((head-file (car (last files)))
                     (bytes (pos-ledger-block
                             `((schema . 3) (kind . "conversion") (from . ,head)
                               (previous . ,(pos-ledger--link
                                             (pos-ledger--event-cid
                                              (pos-ledger--as-block
                                               (pos-ledger--read head-file)))))
                               (ledger_id . ,(pos-seal--last-id files))
                               (empty . ,(vconcat empty)) (root . ,folded))))
                     (name (pos-ledger--event-cid bytes))
                     (file (expand-file-name (format "%08d-%s.json" (1+ events) name)
                                             (file-name-directory head-file))))
                (push (list archive file bytes name) pending)))))))
    (pcase-dolist (`(,archive ,file ,bytes ,name) (nreverse pending))
      (pos-seal--write-new file bytes)
      (pos-seal--checkpoint archive name)
      (push `((archive . ,archive) (event . ,file) (head . ,name)) converted))
    `((converted . ,(vconcat (nreverse converted)))
      (skipped . ,(vconcat (nreverse skipped))))))

;;;; Keeping an existing archive

(defun pos-seal--added-paths (files entries)
  "Return (ADDED . FIRST) for a ledger of event FILES enrolling ENTRIES.
ADDED is, for each event in order, the paths it enrolled as the ledger
now has them: an event before a schema 2 conversion enrolled paths that
conversion may have renamed or removed.  FIRST is the index of the first
schema 3 event, or nil."
  (let* ((values (mapcar (lambda (file) (pos-ledger--parse (pos-ledger--read file)))
                         files))
         (conversion (seq-position values nil
                                   (lambda (value _)
                                     (and (eql (alist-get 'schema value) 2)
                                          (equal (alist-get 'kind value) "conversion")))))
         (rename (and conversion
                      (mapcar (lambda (pair) (cons (pos-ledger--key (car pair)) (cdr pair)))
                              (alist-get 'rename (nth conversion values)))))
         (index -1))
    (cons (mapcar
           (lambda (value)
             (setq index (1+ index))
             (let ((paths (mapcar (lambda (pair) (pos-ledger--key (car pair)))
                                  (alist-get 'add value))))
               (when (and conversion (< index conversion))
                 (setq paths (mapcar (lambda (path) (or (cdr (assoc path rename)) path))
                                     paths)))
               (seq-filter (lambda (path) (assoc path entries)) paths)))
           values)
          (seq-position values nil
                        (lambda (value _) (eql (alist-get 'schema value) 3))))))

(defun pos-seal-keep (root)
  "Move to its keeper each archive under ROOT that is to be kept by one.
That is, each whose scope's entry gives it to a keeper and whose files
are still on disk.  The keeper is sent the events it lacks, in order,
each with the files it enrolled; the first schema 3 event also with
whatever enrolled before it was not sent in this run, since that event
is where a keeper requires them.  Once the keeper holds the ledger's
head and its root, the archive is removed from disk.  Refused unless
every such archive is as its ledger says, with a ledger of schema 3
beside it and nothing hidden, before anything is sent.  Interrupted, it
resumes.  Return an alist: kept, each an archive with its keeper and
the events and files sent; and skipped, each an archive with the
reason."
  (let (kept skipped pending)
    (dolist (archive (pos-ledger-roots root))
      (pcase-let* ((url (pos-ledger-kept archive))
                   (`(,entries ,head ,_ ,files ,recorded ,_ ,_ ,empty)
                    (pos-ledger-history archive))
                   (reason (cond ((null url) "on disk")
                                 ((null head) "no ledger")
                                 ((null (pos-ledger-inventory archive)) "kept"))))
        (if reason
            (push `((archive . ,archive) (reason . ,reason)) skipped)
          (pos-ledger--as-named archive files)
          (unless (pos-ledger--event-cid-p head)
            (pos-ledger--refuse
             'kept "A keeper keeps a ledger of schema 3; convert this one first: %s"
             archive))
          (when (string-prefix-p (file-name-as-directory archive) (car files))
            (pos-ledger--refuse
             'ledger "A kept archive's ledger lies beside it, not inside: %s" archive))
          (let* ((cids (condition-case err
                           (pos-cid-tree archive)
                         (pos-cid-sharding-unsupported
                          (pos-ledger--refuse 'sharding-unsupported "%S" (cdr err)))))
                 (actual (pos-ledger--with-cids
                          entries (pos-ledger-inventory archive) cids)))
            (pcase-let ((`(,missing ,changed ,new)
                         (pos-ledger--differences entries actual)))
              (when (or missing changed new
                        (not (equal recorded (cdr (assoc "." cids))))
                        (not (equal recorded
                                    (cdr (assoc "." (pos-ledger-fold entries empty))))))
                (pos-ledger--refuse
                 'unclean "The archive is not as its ledger says; check it first: %s"
                 archive)))
            (when (seq-some (lambda (pair)
                              (seq-some (lambda (part) (string-prefix-p "." part))
                                        (split-string (car pair) "/")))
                            entries)
              (pos-ledger--refuse
               'hidden "A keeper holds no hidden file, and this ledger enrols one: %s"
               archive))
            (push (list archive url entries head files recorded) pending)))))
    (pcase-dolist (`(,archive ,url ,entries ,head ,files ,recorded) (nreverse pending))
      (let* ((keeper (funcall pos-remote-keeper-function url))
             (claims (funcall pos-seal-claims-function `((archive . ,archive)) nil))
             (described (pos-remote-describe keeper))
             (held (alist-get 'events described))
             (names (mapcar #'file-name-nondirectory files))
             sent)
        (when (or (> held (length files))
                  (and (> held 0)
                       (not (equal (alist-get 'head described)
                                   (substring (nth (1- held) names) 9 -5)))))
          (pos-ledger--refuse 'chain "The keeper holds another ledger than this one: %s"
                              archive))
        (pcase-let ((`(,added . ,first) (pos-seal--added-paths files entries)))
          (dotimes (i (length files))
            (when (>= i held)
              (let (batch)
                (dolist (path (if (eql i first)
                                  (apply #'append (seq-take added (1+ i)))
                                (nth i added)))
                  (let ((cid (alist-get 'cid (cdr (assoc path entries)))))
                    (unless (or (member cid sent) (assoc cid batch))
                      (push (cons cid (pos-ledger--read (expand-file-name path archive)))
                            batch))))
                (pos-remote-append keeper (nth i names) (pos-ledger--read (nth i files))
                                   batch claims)
                (setq sent (append (mapcar #'car batch) sent))))))
        (let ((now (pos-remote-describe keeper)))
          (unless (and (equal head (alist-get 'head now))
                       (equal (length files) (alist-get 'events now))
                       (equal recorded (alist-get 'root now)))
            (pos-ledger--refuse 'chain "The keeper does not hold this ledger as it is: %s"
                                archive)))
        (delete-directory archive t)
        (push `((archive . ,archive) (keeper . ,url)
                (events . ,(- (length files) held)) (files . ,(length sent)))
              kept)))
    `((kept . ,(vconcat (nreverse kept)))
      (skipped . ,(vconcat (nreverse skipped))))))

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
      restore recorded directories and read bits; remove write bits; never enrols
  convert ROOT
      bring each schema 2 ledger under ROOT to schema 3, once it is clean
  keep ROOT
      move to its keeper each archive under ROOT a keeper is to keep
  link PATH
      print the ipfs:// link to PATH, a sealed path in an archive
  fetch LINK
      print the bytes of the archived file LINK names, an ipfs:// link,
      from a scope above the current directory: on disk or its keeper's
  sign-in URL
      sign in to the keeper at URL, in a browser, and keep the token

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
        (`("convert" ,root)
         (princ (decode-coding-string (pos-ledger-json (pos-seal-convert root))
                                      'utf-8)))
        (`("keep" ,root)
         (princ (decode-coding-string (pos-ledger-json (pos-seal-keep root))
                                      'utf-8)))
        (`("link" ,path)
         (princ (pos-links-link path))
         (terpri))
        (`("fetch" ,link)
         (require 'pos-index)
         ;; The file's bytes as they are: `princ' would encode them.
         (send-string-to-terminal (pos-index-bytes link)))
        (`("sign-in" ,url)
         (princ (json-serialize (pos-signin url) :false-object :false))
         (terpri))
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
