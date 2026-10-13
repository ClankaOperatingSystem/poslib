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
;; 1 or 2, which `pos-keep-convert' brings to schema 3.  Always two
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
;; - `pos-seal-claims', `pos-seal-claims-function': what this client
;;   says of a seal to a keeper, and the function that says it.
;; - `pos-seal-send-event', `pos-seal-empty-directories': an event sent
;;   to a keeper with its files, and the directories an item holds
;;   that enrol nothing; pos-keep.el uses both.
;; - `pos-seal-batch': the command line, in pos-cli.el.
;;
;; Checkpoints and repair are pos-checkpoint.el's; bringing a ledger
;; to schema 3, and moving an archive to its keeper and back, are
;; pos-keep.el's.
;; - `pos-seal-uuid': a random UUID, version 4, in canonical form.
;; - `pos-seal-rumour': (DESTINATION . TEXT), the rumour of TEXT dated
;;   DATE.
;; - `pos-seal-rumour-about': (DESTINATION . TEXT), the rumour of
;;   TARGET, outside the archive of SCOPE.
;; - `pos-seal-tool-directory': the directory this library is installed
;;   in: the parent of its Lisp directory.
;; - `pos-seal-tool-commit': the commit the tool installed in DIRECTORY
;;   is at, or nil.
;; - `pos-seal-held-at-once': cIDs a keeper is asked about in one
;;   request: the least it takes.

;;; Code:

(require 'cl-lib)
(require 'pos-bytes)
(require 'pos-path)
(require 'pos-cid)
(require 'pos-git)
(require 'pos-ledger)
(require 'pos-links)
(require 'pos-remote)

;;;; Items

(defun pos-seal--files (source rel)
  "Return SOURCE's files as (PATH . FILE), refusing hidden and special ones.
PATH is relative to the archive, from REL for SOURCE."
  (cond
   ((string-prefix-p "." (file-name-nondirectory source))
    (pos-ledger-refuse 'hidden "Hidden files are not sealed: %s" source))
   ((file-symlink-p source)
    (pos-ledger-refuse 'link "Symlink in item: %s" source))
   ((file-directory-p source)
    (mapcan (lambda (name)
              (pos-seal--files (expand-file-name name source) (concat rel "/" name)))
            (pos-ledger-entry-names source)))
   (t (pos-ledger-regular source)
      (list (cons rel source)))))

(defun pos-seal--collections (source rel)
  "Return the collections in SOURCE, as paths relative to the archive from REL."
  (when (file-directory-p source)
    (append (when (pos-ledger-declared-p source) (list rel))
            (mapcan (lambda (name)
                      (pos-seal--collections (expand-file-name name source)
                                             (concat rel "/" name)))
                    (pos-ledger-entry-names source)))))

(defun pos-seal-uuid ()
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

;;;; Links

(defun pos-seal-rumour (date text)
  "Return (DESTINATION . TEXT), the rumour of TEXT dated DATE.
It is named by its text, so rumours of one target differ in name when
they differ in word."
  (cons (format "rumours/%s-%s.org" date
                (substring (pos-bytes-sha (encode-coding-string text 'utf-8)) 0 12))
        text))

(defun pos-seal-rumour-about (target scope date found why)
  "Return (DESTINATION . TEXT), the rumour of TARGET, outside the archive of SCOPE.
Dated DATE.  The text says what FOUND the target, as \"sealing =x=
found a link to\", and WHY it is a rumour, as \", outside the
archive\"; then what the target was, as `pos-links-description' has it."
  (let ((path (file-relative-name target scope)))
    (pos-seal-rumour
     date (concat "#+TITLE: Rumour of " path "\n#+DATE: " date "\n\n"
                  "On " date ", " found " =" path "=" why ". It was then "
                  (pos-links-description target scope) ".\n"))))

(defun pos-seal--rumour (target scope item date)
  "Return the rumour of TARGET, outside the archive of SCOPE, found sealing ITEM.
As (DESTINATION . TEXT), DESTINATION within the archive, dated DATE."
  (pos-seal-rumour-about target scope date
                         (concat "sealing =" item "= found a link to")
                         ", outside the archive"))

(defun pos-seal--ids (scope files)
  "Return where each Org ID is held, for the id links in FILES, or nil.
As `pos-links-ids' gives it, over the Org files among FILES and those
of the garden of SCOPE.  Nothing is read, and nil returned, unless an
Org file of FILES has an id link.  FILES is an alist of sealed path
and file."
  (when (seq-some
         (lambda (pair)
           (and (string-suffix-p ".org" (cdr pair))
                (string-search "[[id:" (pos-bytes-read (cdr pair)))))
         files)
    (pos-links-ids
     (seq-uniq
      (append (pos-links-org-files (pos-links-garden scope))
              (seq-filter (lambda (file) (string-suffix-p ".org" file))
                          (mapcar #'cdr files)))))))

(defun pos-seal--classify (source rel archive files collections date written-at)
  "Return (PLANS . RUMOURS) for the links in FILES of the item at SOURCE.
The item is to be sealed at REL in ARCHIVE.  PLANS holds, for each
file, its sealed path and a `pos-resolution' for each of its links; a
link to another file of the item has a target and no text yet, and a
capsule's links are left as written.  RUMOURS holds (DESTINATION .
TEXT) for each rumour the links need, dated DATE.  COLLECTIONS and
WRITTEN-AT are as `pos-seal--resolve' takes them.  Refuse `unresolved'
a link within the item to a file it does not hold."
  (let* ((scope (file-name-directory archive))
         (base (file-name-as-directory source))
         (inside (lambda (abs) (or (equal abs source) (string-prefix-p base abs))))
         (in-archive (lambda (abs) (if (equal abs source) rel
                                     (concat rel "/" (substring abs (length base))))))
         (ids (pos-seal--ids scope files))
         rumours plans)
    (dolist (pair files)
      (let ((file (cdr pair)) resolved)
        (dolist (link (unless (seq-some (lambda (c)
                                          (and (pos-path-within-p (car pair) c)
                                               (pos-ledger-capsule-p
                                                (if (equal c rel) source
                                                  (concat base (substring c (1+ (length rel))))))))
                                        collections)
                        (pos-links-in-file file ids)))
          (pcase-let* (((cl-struct pos-link text path suffix) link)
                       (abs (expand-file-name path (if (and written-at (equal file source))
                                                       written-at
                                                     (file-name-directory file)))))
            (push
             (if (funcall inside abs)
                 (let ((target (funcall in-archive abs)))
                   (cond
                    ((seq-some (lambda (c) (and (pos-path-within-p (car pair) c)
                                                (pos-path-within-p target c)))
                               collections)
                     (pos-resolution-create :file (car pair) :link link
                                            :kind "internal" :to text))
                    ((assoc target files)
                     (pos-resolution-create :file (car pair) :link link
                                            :kind "cid" :target target :suffix suffix))
                    (t (pos-ledger-refuse 'unresolved "Link within the item to %s" abs))))
               (pcase (pos-links-resolve abs scope)
                 (`(cid . ,to)
                  (pos-resolution-create :file (car pair) :link link
                                         :kind "cid" :to (concat to suffix)))
                 ;; A link to nothing is kept, annotated as broken: the
                 ;; annotation replaces the whole link.
                 (`(broken . ,_)
                  (pos-resolution-create
                   :file (car pair) :link link
                   :kind "broken" :to (pos-links-annotate file link "broken")))
                 (`(rumour . ,target)
                  (let ((rumour (pos-seal--rumour target scope rel date)))
                    (unless (assoc (car rumour) rumours) (push rumour rumours))
                    (pos-resolution-create :file (car pair) :link link
                                           :kind "rumour" :rumour (car rumour)
                                           :suffix suffix)))))
             resolved)))
        (push (cons (car pair) (nreverse resolved)) plans)))
    (cons plans rumours)))

(defun pos-seal--rewrite (plans files rumour-cids)
  "Return (ADD LINKS ORIGINALS) for PLANS, the links of FILES resolved.
PLANS is as `pos-seal--classify' returns it.  A file is rewritten after
the files it cites, whose CIDs change with them: each link is given
the CID it awaits, a file's from those rewritten before it and a
rumour's from RUMOUR-CIDS, an alist of destination and CID.  ADD holds
the entry of each file as rewritten, LINKS every link as a plan
records it, and ORIGINALS the SHA-256 each rewritten file had.  No
file is written.  Refuse `loop' if files cite one another in a loop."
  (let (done links originals add)
    (while plans
      (let ((ready (seq-find (lambda (plan)
                               (seq-every-p (lambda (r)
                                              (or (not (pos-resolution-target r))
                                                  (assoc (pos-resolution-target r) done)))
                                            (cdr plan)))
                             plans)))
        (unless ready
          (pos-ledger-refuse 'loop "Links loop within the item: %s"
                             (mapconcat #'car plans ", ")))
        (setq plans (delq ready plans))
        (dolist (r (cdr ready))
          (pcase-let (((cl-struct pos-resolution to target rumour suffix) r))
            (unless to
              (setf (pos-resolution-to r)
                    (concat "ipfs://"
                            (cdr (if target (assoc target done) (assoc rumour rumour-cids)))
                            suffix)))))
        (let* ((file (cdr (assoc (car ready) files)))
               (bytes (pos-bytes-read file))
               (changed (seq-remove (lambda (r) (equal (pos-resolution-from r)
                                                       (pos-resolution-to r)))
                                    (cdr ready)))
               (new (pos-links-rewrite bytes (mapcar #'pos-resolution-rewrite changed)))
               (entry (list (cons 'cid (pos-cid-bytes new))
                            (cons 'mode (logand (pos-ledger-permissions file) (lognot #o222)))
                            (cons 'sha256 (pos-bytes-sha new))
                            (cons 'size (length new)))))
          (when changed
            (push (cons (car ready) (pos-bytes-sha bytes)) originals))
          (dolist (r (cdr ready))
            (push (pos-resolution-alist r) links))
          (push (cons (car ready) (alist-get 'cid entry)) done)
          (push (cons (car ready) entry) add))))
    (list add links originals)))

(defun pos-seal--resolve (source rel archive files collections date &optional written-at)
  "Resolve the links in FILES, the item at SOURCE sealed at REL in ARCHIVE.
Return (ADD LINKS ORIGINALS RUMOURS): the entries of the rewritten files,
every path link with its resolution, the SHA-256 of each file rewritten,
and the rumours to seal first.  COLLECTIONS are the item's; DATE dates
the rumours.  A file's links are read from its directory, or for a file
item, from WRITTEN-AT if given.  An id link in an Org file is read as a
link to the file that holds its ID, among the item's files and the
Org files of the scope's garden."
  (pcase-let* ((`(,plans . ,rumours)
                (pos-seal--classify source rel archive files collections date written-at))
               (rumours (seq-sort-by #'car #'string< rumours))
               (rumour-cids (mapcar (lambda (r) (cons (car r) (pos-cid-bytes
                                                               (encode-coding-string (cdr r) 'utf-8))))
                                    rumours))
               (`(,add ,links ,originals) (pos-seal--rewrite plans files rumour-cids)))
    (list (seq-sort-by #'car #'string< add)
          (vconcat (pos-links-in-order links))
          (seq-sort-by #'car #'string< originals)
          (vconcat (mapcar (lambda (r) `((destination . ,(car r)) (text . ,(cdr r))
                                         (cid . ,(cdr (assoc (car r) rumour-cids)))))
                           rumours)))))

;;;; Plans

(defun pos-seal--check-place (source destination rel)
  "Refuse unless SOURCE may be sealed at DESTINATION, REL in its archive.
SOURCE must exist outside any archive (source); DESTINATION must not
exist and REL must be a safe path (destination) with no hidden part
\(hidden)."
  (when (pos-links-archive-of (concat source "/"))
    (pos-ledger-refuse 'source "Source is already archived: %s" source))
  (unless (or (file-exists-p source) (file-symlink-p source))
    (pos-ledger-refuse 'source "No such item: %s" source))
  (when (or (file-exists-p destination) (file-symlink-p destination))
    (pos-ledger-refuse 'destination "Destination exists: %s" destination))
  (unless (pos-path-safe-p rel)
    (pos-ledger-refuse 'destination "Unsafe destination: %s" rel))
  (when (seq-some (lambda (part) (string-prefix-p "." part)) (split-string rel "/"))
    (pos-ledger-refuse 'hidden "Hidden files are not sealed: %s" rel)))

(defun pos-seal--check-unsealed (rel history)
  "Refuse `sealed' if REL is within an item or collection of HISTORY."
  (let ((within (seq-find (lambda (i) (pos-path-within-p rel i))
                          (append (pos-history-items history)
                                  (pos-history-collections history)))))
    (when within
      (pos-ledger-refuse 'sealed "Destination is within sealed %s: %s" within rel))))

(defun pos-seal--named (archive history ledger-id)
  "Return the id ARCHIVE's entry names for its ledger, or nil.
Refuse `identity' unless the ledger of HISTORY is the one named, and
LEDGER-ID, if given, is the id named."
  (let ((named (pos-ledger-named archive)))
    (pos-ledger-as-named archive (pos-history-files history))
    (when (and named ledger-id (not (equal named ledger-id)))
      (pos-ledger-refuse
       'identity "Not the ledger its entry names, %s: %s" named archive))
    named))

(defun pos-seal--keeper (archive history)
  "Return the base URL of ARCHIVE's keeper, or nil if it is kept on disk.
Refuse `kept' a kept archive whose ledger, of HISTORY, is not of schema 3."
  (let ((kept (pos-ledger-kept archive))
        (head (pos-history-head history)))
    (when (and kept head (not (pos-ledger-event-cid-p head)))
      (pos-ledger-refuse
       'kept "A keeper keeps a ledger of schema 3; convert this one first: %s"
       archive))
    kept))

(defun pos-seal--evidence (archive known kept)
  "Return the inventory of ARCHIVE, which is to be as KNOWN enrols.
Refuse `differs' if an enrolled file is missing or changed.  Nothing
of an archive KEPT by a keeper is on disk to compare: what it holds is
what its ledger enrols."
  (if kept
      (pos-ledger-kept-entries archive known)
    (pcase-let (((cl-struct pos-comparison missing changed inventory)
                 (pos-ledger-against-disk archive known)))
      (when (or missing changed)
        (pos-ledger-refuse 'differs "Existing evidence differs in %s: %S %S"
                           archive missing changed))
      inventory)))

(defun pos-seal--new-rumours (rumours archive known kept)
  "Return those of RUMOURS that ARCHIVE does not hold yet, as a vector.
A rumour sealed already, word for word, is cited, not sealed again.
Refuse `destination' if another text is where a rumour would go.  KNOWN
is what the ledger enrols, which says what an archive KEPT by a keeper
holds."
  (vconcat
   (seq-remove
    (lambda (rumour)
      (let ((there (expand-file-name (alist-get 'destination rumour) archive))
            (enrolled (and kept (assoc (alist-get 'destination rumour) known))))
        (cond
         (enrolled
          (unless (equal (alist-get 'cid (cdr enrolled)) (alist-get 'cid rumour))
            (pos-ledger-refuse 'destination "Rumour destination exists: %s" there))
          t)
         ((file-exists-p there)
          (unless (equal (pos-bytes-read there)
                         (encode-coding-string (alist-get 'text rumour) 'utf-8))
            (pos-ledger-refuse 'destination "Rumour destination exists: %s" there))
          t))))
    rumours)))

(defun pos-seal-plan (source destination &optional ledger-id date as-destination)
  "Return the plan to seal SOURCE at DESTINATION, inside an archive.
LEDGER-ID names a new ledger; by default it takes the id its scope's
entry names, else one made at random.  DATE, by default today's, dates
any rumours the item's links need.  With AS-DESTINATION, a file's links
are read as written from DESTINATION, not from where it lies; when it
is a directory, they are read as written from there."
  (let* ((source (directory-file-name (file-truename (pos-ledger-checked source))))
         (destination (directory-file-name (expand-file-name destination)))
         (archive (pos-links-archive-of destination)))
    (pos-ledger-checked (file-name-directory destination))
    (unless archive
      (pos-ledger-refuse 'destination "Destination is not in an archive: %s" destination))
    (let* ((rel (file-relative-name destination archive))
           (archive (file-truename archive))
           (destination (expand-file-name rel archive)))
      (pos-seal--check-place source destination rel)
      (let ((history (pos-ledger-history archive)))
        (pos-seal--check-unsealed rel history)
        (pcase-let* (((cl-struct pos-history (entries known) head events files) history)
                     (named (pos-seal--named archive history ledger-id))
                     (kept (pos-seal--keeper archive history))
                     (actual (pos-seal--evidence archive known kept))
                     (collections (sort (pos-seal--collections source rel) #'string<))
                     (`(,add ,links ,originals ,rumours)
                      (pos-seal--resolve source rel archive (pos-seal--files source rel)
                                         collections
                                         (or date (format-time-string "%Y-%m-%d"))
                                         (cond ((stringp as-destination)
                                                (file-name-as-directory as-destination))
                                               (as-destination
                                                (file-name-directory destination)))))
                     (rumours (pos-seal--new-rumours rumours archive known kept)))
          `((schema . 2) (operation . "seal")
            (source . ,source) (destination . ,destination) (archive . ,archive)
            (ledger . ,(pos-seal--ledger-folder archive))
            (number . ,(1+ events)) (previous . ,(or head :null))
            (ledger_id . ,(or (pos-ledger-identity files) named ledger-id (pos-seal-uuid)))
            (add . ,add) (collections . ,(vconcat collections))
            (links . ,links) (originals . ,originals) (rumours . ,rumours)
            (inventory_sha256 . ,(pos-bytes-sha (pos-bytes-json actual)))
            ,@(when kept `((kept . ,kept)))))))))

(defun pos-seal-stage (bytes destination &optional ledger-id links-from)
  "Stage BYTES, a new record, and return the plan to seal them at DESTINATION.
They are staged beside the archive, in _seal/, so the move is one rename,
with DESTINATION's extension, by which their links are found; the links
are read as written from DESTINATION, or from LINKS-FROM, a directory,
where a program composed the record somewhere else.  The staged file is
named for DESTINATION, so staging for it again replaces what was staged
before.  If planning fails, nothing is left staged.
LEDGER-ID names a new ledger, as for `pos-seal-plan'."
  (let* ((archive (or (pos-links-archive-of (expand-file-name destination))
                      (pos-ledger-refuse 'destination "Destination is not in an archive: %s"
                                         destination)))
         (stage (expand-file-name "_seal" (file-name-directory archive)))
         (links-from (and links-from
                          (let ((dir (file-truename (expand-file-name links-from))))
                            (unless (file-directory-p dir)
                              (pos-ledger-refuse 'source "No such directory to read links from: %s"
                                                 links-from))
                            dir))))
    (make-directory stage t)
    (let ((file (expand-file-name
                 (concat "new-"
                         (substring (pos-bytes-sha
                                     (encode-coding-string
                                      (file-relative-name (expand-file-name destination) archive)
                                      'utf-8))
                                    0 16)
                         (file-name-extension destination t))
                 stage)))
      (pos-bytes-write file bytes)
      (set-file-modes file #o644)
      (condition-case err
          (pos-seal-plan file destination ledger-id nil (or links-from t))
        (error
         (delete-file file)
         (when (directory-empty-p stage) (delete-directory stage))
         (signal (car err) (cdr err)))))))

;;;; Application

(defun pos-seal--entries (pairs)
  "Return the entries of PAIRS, each (PATH . FILE), as (PATH . ENTRY), sorted."
  (seq-sort-by #'car #'string<
               (mapcar (lambda (pair) (cons (car pair) (pos-ledger-record-with-cid (cdr pair))))
                       pairs)))

(defun pos-seal--sealed-result (archive)
  "Checkpoint the head of ARCHIVE, just sealed, and return (EVENT-FILE . ROOT).
EVENT-FILE is the ledger's last event and ROOT the root it records."
  (pcase-let (((cl-struct pos-history head files) (pos-ledger-history archive)))
    (pos-ledger-checkpoint-head archive head)
    (cons (car (last files))
          (alist-get 'root (pos-bytes-parse (pos-bytes-read (car (last files))))))))

(defun pos-seal--protect (file)
  "Remove FILE's write bits."
  (set-file-modes file (logand (pos-ledger-permissions file) (lognot #o222)) 'nofollow))

(defun pos-seal--empty-under (top rel)
  "Return the directories at or under TOP holding nothing IPFS would add.
As paths from REL for TOP; see `pos-seal-empty-directories'."
  (when (and (file-directory-p top) (not (file-symlink-p top)))
    (let ((names (seq-remove (lambda (name) (string-prefix-p "." name))
                             (pos-ledger-entry-names top))))
      (if names
          (mapcan (lambda (name)
                    (pos-seal--empty-under (expand-file-name name top)
                                           (if (string-empty-p rel) name
                                             (concat rel "/" name))))
                  names)
        (unless (string-empty-p rel) (list rel))))))

(defun pos-seal-empty-directories (top rel)
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
    (dolist (name (pos-ledger-entry-names top))
      (let ((path (expand-file-name name top)))
        (cond ((or (string-prefix-p "." name) (equal name "archives")))
              ((pos-seal--staging-p path) (delete-directory path))
              (t (pos-seal--drop-staging path)))))))

(cl-defstruct (pos-seal--event-made (:constructor pos-seal--event-made-make)
                                    (:copier nil))
  "An event made for a seal.
Its ledger FILE's name, the NAME that names it, its hash or its CID,
and its BYTES."
  file name bytes)

(defun pos-seal--event-of (plan item add collections empty)
  "Return the event of PLAN sealing ADD as ITEM, a `pos-seal--event-made'.
COLLECTIONS and EMPTY are the item's collections and empty directories.
A ledger with no event yet, or one whose head is a block, takes a schema 3
event: a DAG-JSON block named by its CID, its root the fold of what the
ledger enrols.  A schema 1 or 2 ledger takes a schema 2 event, named by
its hash, until it is converted."
  (let-alist plan
    (pcase-let* (((cl-struct pos-history entries head events (empty empties))
                  (pos-ledger-history .archive))
                 (blocks (or (null head) (pos-ledger-event-cid-p head)))
                 (bytes
                  (if blocks
                      (pos-bytes-block
                       `((schema . 3)
                         (previous . ,(if head (pos-ledger-link head) :null))
                         (ledger_id . ,.ledger_id) (item . ,item) (add . ,add)
                         (root . ,(cdr (assoc "." (pos-ledger-fold
                                                   (append add entries)
                                                   (append empty empties)))))
                         (collections . ,collections) (empty . ,(vconcat empty))))
                    (pos-bytes-json
                     `((schema . 2) (previous . ,(or head :null)) (ledger_id . ,.ledger_id)
                       (item . ,item)
                       (add . ,add) (root . ,(pos-cid-directory .archive))
                       (collections . ,collections)))))
                 (name (if blocks (pos-ledger-event-cid bytes) (pos-bytes-sha bytes))))
      (pos-seal--event-made-make :file (pos-ledger-event-file-name (1+ events) name)
                                 :name name :bytes bytes))))

(defun pos-seal--event (plan destination add collections)
  "Write the event of PLAN sealing ADD at DESTINATION; return what names it.
COLLECTIONS are the item's."
  (let* ((item (file-relative-name destination (alist-get 'archive plan)))
         (event (pos-seal--event-of plan item add collections
                                    (pos-seal-empty-directories destination item))))
    (pos-ledger-write-new (expand-file-name (pos-seal--event-made-file event)
                                            (alist-get 'ledger plan))
                          (pos-seal--event-made-bytes event))
    (pos-seal--event-made-name event)))

(defun pos-seal--sealed (plan)
  "Return the items PLAN's events have sealed so far, refusing a stranger's.
Events since PLAN's previous head must seal PLAN's rumours, then its item,
in order."
  (let-alist plan
    (let* ((files (pos-history-files (pos-ledger-history .archive)))
           (hashes (mapcar #'pos-ledger-event-file-id files))
           (since (if (eq .previous :null) files
                    (let ((at (seq-position hashes .previous)))
                      (unless at
                        (pos-ledger-refuse 'plan "Ledger changed since review: %s" .archive))
                      (nthcdr (1+ at) files))))
           (expected (append (mapcar (lambda (r) (alist-get 'destination r)) .rumours)
                             (list (file-relative-name .destination .archive))))
           (sealed (mapcar (lambda (f) (alist-get 'item (pos-bytes-parse (pos-bytes-read f))))
                           since)))
      (unless (equal sealed (seq-take expected (length sealed)))
        (pos-ledger-refuse 'plan "Ledger changed since review: %s" .archive))
      sealed)))

(defun pos-seal--rewrite-source (plan)
  "Rewrite the links in PLAN's item where it lies, if not done already."
  (let-alist plan
    (let ((rel (file-relative-name .destination .archive)))
      (pcase-dolist (`(,path . ,original) .originals)
        (let* ((path (pos-bytes-key path))
               (file (if (equal path rel) .source
                       (expand-file-name (substring path (1+ (length rel))) .source)))
               (bytes (pos-bytes-read file)))
          (cond
           ((equal (pos-bytes-sha bytes) original)
            (let ((new (pos-links-rewrite bytes (pos-links-edits .links path)))
                  (modes (file-modes file)))
              (pos-bytes-write file new)
              (set-file-modes file modes)))
           ((equal (pos-bytes-sha bytes)
                   (alist-get 'sha256 (cdr (assoc path (mapcar (lambda (a) (cons (pos-bytes-key (car a)) (cdr a))) .add))))))
           (t (pos-ledger-refuse 'plan "Item changed since review: %s" file))))))))

(defun pos-seal-apply (plan expected)
  "Apply PLAN, whose canonical JSON has the SHA-256 EXPECTED.
Seal its rumours, then its item, rewriting the item's links first.
Refuse if anything it relied on has changed; resume if interrupted.
Return (EVENT-FILE . ROOT)."
  (unless (equal (pos-bytes-sha (pos-bytes-json plan)) expected)
    (pos-ledger-refuse 'plan "Reviewed plan hash mismatch"))
  (let-alist plan
    (unless (and (eql .schema 2) (equal .operation "seal"))
      (pos-ledger-refuse 'plan "Not a seal plan"))
    (unless (equal .kept (pos-ledger-kept .archive))
      (pos-ledger-refuse 'plan "The archive is not kept as planned: %s" .archive))
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
                     (pos-seal--entries
                      (mapcar (lambda (p) (cons p (expand-file-name p .archive))) paths)))))
      (when (and (null sealed) (not (file-exists-p .destination)))
        (unless (equal (pos-bytes-sha (pos-bytes-json (pos-ledger-inventory .archive)))
                       .inventory_sha256)
          (pos-ledger-refuse 'plan "Archive changed since review: %s" .archive)))
      ;; Rumours first: each an item of its own.
      (seq-doseq (rumour .rumours)
        (let-alist rumour
          (let ((there (expand-file-name .destination (alist-get 'archive plan))))
            (unless (member .destination sealed)
              (unless (file-exists-p there)
                (pos-ledger-write-new there (encode-coding-string .text 'utf-8)))
              (unless (equal (pos-bytes-read there) (encode-coding-string .text 'utf-8))
                (pos-ledger-refuse 'plan "Rumour differs from its plan: %s" there))
              (pos-seal--event plan there (funcall add-of (list .destination)) [])))))
      ;; Then the item, its links rewritten where it lies, then moved.
      (unless (file-exists-p .destination)
        (unless (file-exists-p .source)
          (pos-ledger-refuse 'plan "Neither before nor after the move: %s" .source))
        (pos-seal--rewrite-source plan)
        (pos-seal--drop-staging .source)
        (unless (equal (pos-bytes-json .add)
                       (pos-bytes-json (pos-seal--entries (pos-seal--files .source rel))))
          (pos-ledger-refuse 'plan "Item changed since review: %s" .source))
        (make-directory (file-name-directory .destination) t)
        (rename-file .source .destination))
      (pos-seal--leave-stage .source)
      (unless (equal (pos-bytes-json .add)
                     (pos-bytes-json (funcall add-of (mapcar (lambda (a) (pos-bytes-key (car a)))
                                                             .add))))
        (pos-ledger-refuse 'plan "Item changed after the move: %s" .destination))
      (unless (member rel sealed)
        (pos-seal--event plan .destination .add .collections))
      (dolist (pair .add)
        (pos-seal--protect (expand-file-name (pos-bytes-key (car pair)) .archive)))
      (seq-doseq (rumour .rumours)
        (pos-seal--protect (expand-file-name (alist-get 'destination rumour) .archive)))
      (pos-seal--sealed-result .archive))))

;;;; Sealing to a keeper

(defvar pos-seal-claims-function #'pos-seal-claims
  "The function a seal's claims are made with, given its plan and hash.")

(defvar pos-seal-tool-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "The directory this library is installed in: the parent of its Lisp directory.
Its COMMIT file, or the repository git reads there, says which commit
the tool is at.")

(defun pos-seal-tool-commit (&optional directory)
  "Return the commit the tool installed in DIRECTORY is at, or nil.
DIRECTORY defaults to `pos-seal-tool-directory'.  It is read from the
COMMIT file an image writes beside the Lisp directory, else asked of git
where DIRECTORY is a repository; nil where neither says."
  (let* ((directory (expand-file-name (or directory pos-seal-tool-directory)))
         (file (expand-file-name "COMMIT" directory)))
    (cond
     ((file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((commit (string-trim (buffer-string))))
          (and (string-match-p "\\`[0-9a-f]+\\'" commit) commit))))
     ((file-directory-p (expand-file-name ".git" directory))
      (let* ((process-environment
              (cons (concat "GIT_CEILING_DIRECTORIES="
                            (directory-file-name
                             (file-name-directory (directory-file-name directory))))
                    process-environment))
             (commit (ignore-errors
                       (pos-git-line directory "rev-parse" "--verify" "-q" "HEAD"))))
        (and commit (not (string-empty-p commit)) commit))))))

(defun pos-seal-claims (plan expected)
  "Return the claims of this client about the seal of PLAN.
An alist of strings by name, for a keeper to record: the plan's hash
EXPECTED, where there was a plan, the tool and the commit it is at, where
it knows it, and of the scope's repository, where there is one and git
reads it, the scope's path, the commit and branch it is at, whether its
working tree is dirty, and each remote's URL less any user and password."
  (let* ((scope (file-name-directory (alist-get 'archive plan)))
         (root (locate-dominating-file scope ".git"))
         (tool-commit (pos-seal-tool-commit))
         (claims `(,@(when expected `(("plan" . ,expected))) ("tool" . "poslib")
                   ,@(when tool-commit `(("tool_commit" . ,tool-commit))))))
    (when root
      (setq root (directory-file-name (expand-file-name root)))
      (let* ((process-environment
              (cons (concat "GIT_CEILING_DIRECTORIES=" (file-name-directory root))
                    process-environment))
             (git (lambda (&rest args)
                    (ignore-errors (apply #'pos-git-line root args))))
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
head.  Return the keeper's description."
  (let* ((described (pos-remote-describe keeper))
         (theirs (alist-get 'events described))
         (head (let ((h (alist-get 'head described))) (unless (eq h :null) h)))
         (events (pos-history-events (pos-ledger-history archive)))
         written)
    (condition-case err
        (progn
          (dotimes (i (max 0 (- theirs events)))
            (let* ((number (+ events i 1))
                   (bytes (pos-remote-event keeper number))
                   (file (expand-file-name
                          (pos-ledger-event-file-name number (pos-ledger-event-cid bytes))
                          ledger)))
              (pos-ledger-write-new file bytes)
              (push file written)))
          (let ((caught (condition-case nil
                            (pos-ledger-history archive)
                          (pos-ledger-refused nil))))
            (unless (and caught (equal head (pos-history-head caught))
                         (equal theirs (pos-history-events caught)))
              (pos-ledger-refuse 'chain "The keeper holds another ledger than this one: %s"
                                 archive))))
      (pos-ledger-refused
       (mapc #'delete-file written)
       (when (and (zerop events) (file-directory-p ledger)
                  (null (pos-ledger-entry-names ledger)))
         (delete-directory ledger))
       (signal (car err) (cdr err))))
    described))

(defconst pos-seal-held-at-once 1024
  "CIDs a keeper is asked about in one request: the least it takes.")

(defun pos-seal--ensure-held (keeper files)
  "Have KEEPER hold every block of FILES, an alist of CID and bytes.
Under version 2: it is asked which blocks it lacks, in sorted order and at most
`pos-seal-held-at-once' at a time, and is put each of those.  A seal
interrupted while putting resumes here, since what is held is not put
again."
  (let (blocks)
    (dolist (file files)
      (dolist (block (pos-cid-blocks (cdr file)))
        (unless (assoc (car block) blocks)
          (push block blocks))))
    (let ((cids (sort (mapcar #'car blocks) #'string<)))
      (while cids
        (let ((batch (seq-take cids pos-seal-held-at-once)))
          (setq cids (seq-drop cids pos-seal-held-at-once))
          (dolist (missing (pos-remote-held keeper batch))
            (pos-remote-put keeper missing (cdr (assoc missing blocks)))))))))

(defun pos-seal-send-event (keeper version name event files claims &optional following)
  "Append to KEEPER the event EVENT named NAME, with FILES as VERSION takes them.
Inside the append under version 1; put as blocks first under version 2.
CLAIMS and FOLLOWING are as `pos-remote-append' takes them."
  (if (eql version 2)
      (progn (pos-seal--ensure-held keeper files)
             (pos-remote-append keeper name event nil claims following))
    (pos-remote-append keeper name event files claims following)))

(defun pos-seal--apply-kept (plan expected)
  "Apply PLAN, whose hash is EXPECTED, to an archive kept by a keeper.
Each event is sent with its files, inside the append or as blocks put
first by the keeper's version, written to the ledger once the keeper
has it, and the item is then removed from where it lay.  Interrupted,
it resumes.  Return (EVENT-FILE . ROOT)."
  (let-alist plan
    (let* ((keeper (funcall pos-remote-keeper-function .kept))
           (claims (funcall pos-seal-claims-function plan expected))
           (rel (file-relative-name .destination .archive))
           (version (pos-remote-version (pos-seal--catch-up .archive .ledger keeper)))
           (sealed (pos-seal--sealed plan))
           (entries-of (lambda () (pos-seal--entries (pos-seal--files .source rel))))
           (send
            (lambda (item add collections empty files)
              (pcase-let (((cl-struct pos-seal--event-made file bytes)
                           (pos-seal--event-of plan item add collections empty)))
                (pos-seal-send-event keeper version file bytes files claims)
                (pos-ledger-write-new (expand-file-name file .ledger) bytes)))))
      (when (and (null sealed)
                 (not (equal (pos-bytes-sha
                              (pos-bytes-json
                               (pos-ledger-kept-entries
                                .archive (pos-history-entries (pos-ledger-history .archive)))))
                             .inventory_sha256)))
        (pos-ledger-refuse 'plan "Archive changed since review: %s" .archive))
      ;; Rumours first: each an item of its own, of the one file its text is.
      (seq-doseq (rumour .rumours)
        (let-alist rumour
          (unless (member .destination sealed)
            (let* ((bytes (encode-coding-string .text 'utf-8))
                   (cid (pos-cid-bytes bytes)))
              (funcall send .destination
                       `((,.destination . ((cid . ,cid) (mode . #o444)
                                           (sha256 . ,(pos-bytes-sha bytes))
                                           (size . ,(length bytes)))))
                       [] nil (list (cons cid bytes)))))))
      ;; Then the item, its links rewritten where it lies, and sent.
      (unless (member rel sealed)
        (unless (or (file-exists-p .source) (file-symlink-p .source))
          (pos-ledger-refuse 'plan "The item is not where it was planned: %s" .source))
        (pos-seal--rewrite-source plan)
        (pos-seal--drop-staging .source)
        (let ((entries (funcall entries-of)))
          (unless (equal (pos-bytes-json .add) (pos-bytes-json entries))
            (pos-ledger-refuse 'plan "Item changed since review: %s" .source))
          (funcall send rel .add .collections (pos-seal-empty-directories .source rel)
                   (seq-uniq
                    (mapcar (lambda (pair)
                              (cons (alist-get 'cid (cdr (assoc (car pair) entries)))
                                    (pos-bytes-read (cdr pair))))
                            (pos-seal--files .source rel))
                    (lambda (a b) (equal (car a) (car b)))))))
      ;; The keeper has the item and the ledger says so: the copy here goes.
      (when (or (file-exists-p .source) (file-symlink-p .source))
        (unless (equal (pos-bytes-json .add) (pos-bytes-json (funcall entries-of)))
          (pos-ledger-refuse 'plan "Item changed since it was sealed: %s" .source))
        (if (file-directory-p .source)
            (delete-directory .source t)
          (delete-file .source)))
      (pos-seal--leave-stage .source)
      (pos-seal--sealed-result .archive))))

(autoload 'pos-seal-batch "pos-cli")

(provide 'pos-seal)
;;; pos-seal.el ends here
