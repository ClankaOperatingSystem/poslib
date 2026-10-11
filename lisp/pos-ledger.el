;;; pos-ledger.el --- Archive integrity ledgers -*- lexical-binding: t; -*-

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

;; Reads and checks archive ledgers as doc/formats.org specifies, in
;; lockstep with pyposlib, and makes a new event's bytes.  `pos-seal'
;; writes them to the ledger.
;;
;; - `pos-ledger-inventory': what an archive holds.
;; - `pos-ledger-history': what its ledger enrolled, a `pos-history'.
;; - `pos-ledger-fold-cids': the archive's CIDs from the ledger alone.
;; - `pos-ledger-event': a new event's name and bytes.
;; - `pos-ledger-event-file-name', `pos-ledger-event-file-id': an event
;;   file's name from its number and id, and the id in a name.
;; - `pos-ledger-kept': the keeper an archive is kept by, if it has one.
;; - `pos-ledger-named': the id an archive's ledger is to have, if named.
;; - `pos-ledger-check': the report on every archive under a root.
;; - `pos-ledger-refuse': signal a refusal, for this module and its callers.
;;
;; JSON values are Lisp values: objects alists, arrays vectors, strings,
;; integers and :null.  Refusals signal `pos-ledger-refused' with a kind
;; symbol and a message.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pos-bytes)
(require 'pos-cid)
(require 'pos-path)

(defconst pos-ledger-integrity "archive-integrity"
  "The folder beside an archive holding its ledger and checkpoints.")

(defconst pos-ledger-directory ".archive-integrity"
  "The legacy ledger's directory, at the top of an archive.")

(defconst pos-ledger-anchors ".archive-integrity-anchors"
  "The legacy checkpoints' directory, beside the root they cover.")

(defconst pos-ledger-declaration "#+COLLECTION: t"
  "The line in a collection's README.org that declares it one.")

(declare-function pos-tree-read-config "pos-tree" (text))
(declare-function pos-remote-describe "pos-remote" (archive))
(defvar pos-remote-keeper-function)

(defvar pos-ledger-offline nil
  "Non-nil if a check is to ask no keeper.
The environment's POS_ARCHIVE_OFFLINE, set to anything, says the same.")

(define-error 'pos-ledger-refused "Archive integrity refused")

(defun pos-ledger-refuse (kind format &rest args)
  "Signal a refusal of KIND, with a message from FORMAT and ARGS."
  (signal 'pos-ledger-refused (list kind (apply #'format format args))))

;;;; DAG-JSON

(defun pos-ledger--plain-p (value)
  "Return non-nil if the JSON VALUE is only what a block may hold.
No object repeats a key, and the key / is a link's alone: an object of
that one key and a CID."
  (cond ((vectorp value) (seq-every-p #'pos-ledger--plain-p value))
        ((consp value)
         (and (equal (length value) (length (seq-uniq (mapcar #'car value))))
              (or (not (assq '/ value))
                  (and (null (cdr value))
                       (stringp (cdar value))
                       (string-match-p "\\`b[a-z2-7]+\\'" (cdar value))))
              (seq-every-p (lambda (pair) (pos-ledger--plain-p (cdr pair))) value)))
        (t t)))

(defun pos-ledger--strict (bytes file)
  "Return the value of BYTES, refusing unless BYTES is its one DAG-JSON block.
FILE names what was read, for the refusal."
  (let ((value (condition-case nil
                   (let ((parsed (pos-bytes-parse bytes)))
                     (and (pos-ledger--plain-p parsed)
                          (equal (pos-bytes-block parsed) bytes)
                          (list parsed)))
                 (error nil))))
    (unless value
      (pos-ledger-refuse 'encoding "Not a DAG-JSON block: %s" file))
    (car value)))

(defun pos-ledger-event-cid (bytes)
  "Return the CID a schema 3 event of BYTES is named by."
  (pos-cid-block pos-cid-dag-json bytes))

(defun pos-ledger-event-cid-p (value)
  "Return non-nil if VALUE has the form of a schema 3 event's CID."
  (and (stringp value)
       (string-match-p "\\`baguqeera[a-z2-7]\\{52\\}\\'" value)
       t))

(defun pos-ledger-link (cid)
  "Return a DAG-JSON link to CID."
  `((/ . ,cid)))

(defun pos-ledger-as-block (bytes)
  "Return BYTES, an event written with a final newline, as a block without it."
  (if (string-suffix-p "\n" bytes) (substring bytes 0 -1) bytes))

;;;; Files

(defun pos-ledger-regular (file)
  "Return FILE's attributes, refusing all but a regular file of one link."
  (let ((attributes (file-attributes file)))
    (unless (and attributes (null (file-attribute-type attributes))
                 (= 1 (file-attribute-link-number attributes)))
      (pos-ledger-refuse 'link "Expected a regular file with one link: %s" file))
    attributes))

(defun pos-ledger-permissions (file)
  "Return FILE's permission bits."
  (logand (file-modes file 'nofollow) #o7777))

(defun pos-ledger--writable-p (file)
  "Return non-nil if FILE has a write bit."
  (pos-ledger-regular file)
  (/= 0 (logand (pos-ledger-permissions file) #o222)))

(defun pos-ledger-record (file)
  "Return FILE's entry: its SHA-256, size and mode less write bits."
  (let* ((before (pos-ledger-regular file))
         (bytes (pos-bytes-read file))
         (after (pos-ledger-regular file)))
    (unless (and (equal (file-attribute-inode-number before)
                        (file-attribute-inode-number after))
                 (equal (file-attribute-size before) (file-attribute-size after))
                 (equal (file-attribute-modification-time before)
                        (file-attribute-modification-time after)))
      (pos-ledger-refuse 'changed "File changed while reading: %s" file))
    (list (cons 'mode (logand (pos-ledger-permissions file) (lognot #o222)))
          (cons 'sha256 (pos-bytes-sha bytes))
          (cons 'size (length bytes)))))

(defun pos-ledger-entry-names (dir)
  "Return DIR's entry names, sorted."
  (sort (directory-files dir nil directory-files-no-dot-files-regexp) #'string<))

(defun pos-ledger-inventory (archive)
  "Return ARCHIVE's inventory: an alist of relative path and entry.
An archive not yet made holds nothing."
  (let (result)
    (named-let walk ((dir archive) (rel nil))
      (dolist (name (unless (and (null rel) (not (file-exists-p dir))
                                 (not (file-symlink-p dir)))
                      (pos-ledger-entry-names dir)))
        (let ((path (expand-file-name name dir))
              (child (if rel (concat rel "/" name) name)))
          (cond
           ((and (null rel) (equal name pos-ledger-directory))
            (unless (and (file-directory-p path) (not (file-symlink-p path)))
              (pos-ledger-refuse 'ledger "Integrity metadata must be a directory")))
           ((file-symlink-p path)
            (pos-ledger-refuse 'link "Symlink in archive: %s" path))
           ((file-directory-p path) (walk path child))
           (t (push (cons child (pos-ledger-record path)) result))))))
    (seq-sort-by #'car #'string< result)))

;;;; History

(defun pos-ledger--keys (object)
  "Return OBJECT's keys as sorted strings."
  (sort (mapcar (lambda (pair) (pos-bytes-key (car pair))) object) #'string<))

(defun pos-ledger--uuid-p (string)
  "Return non-nil if STRING is a UUID in canonical form."
  (and (stringp string)
       (string-match-p (concat "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}"
                               "-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'")
                       string)))

(defun pos-ledger--hex-p (value)
  "Return non-nil if VALUE is a lower-case hex SHA-256."
  (and (stringp value) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value)))

(defun pos-ledger--cid-p (value)
  "Return non-nil if VALUE has the form of a base32 CID."
  (and (stringp value) (string-match-p "\\`b[a-z2-7]+\\'" value)))

(defun pos-ledger--entry-p (entry schema)
  "Return non-nil if ENTRY is a well-formed ledger entry of SCHEMA."
  (and (listp entry)
       (if (eql schema 2)
           (and (equal (pos-ledger--keys entry) '("cid" "mode" "sha256" "size"))
                (pos-ledger--cid-p (alist-get 'cid entry)))
         (equal (pos-ledger--keys entry) '("mode" "sha256" "size")))
       (pos-ledger--hex-p (alist-get 'sha256 entry))
       (natnump (alist-get 'size entry))
       (natnump (alist-get 'mode entry))
       (zerop (logand (alist-get 'mode entry) #o222))))

(defun pos-ledger-folder (archive)
  "Return ARCHIVE's ledger folder: beside it, else the legacy one inside."
  (let ((beside (expand-file-name (concat pos-ledger-integrity "/ledger")
                                  (file-name-directory (directory-file-name archive))))
        (inside (expand-file-name pos-ledger-directory archive)))
    (cond ((and (or (file-exists-p beside) (file-symlink-p beside))
                (or (file-exists-p inside) (file-symlink-p inside)))
           (pos-ledger-refuse 'ledger "Two ledgers for one archive: %s" archive))
          ((or (file-exists-p beside) (file-symlink-p beside)) beside)
          (t inside))))

(defun pos-ledger--convert (entries event file)
  "Return ENTRIES after the conversion EVENT, in the event FILE.
Refuse unless it removes, renames or converts every legacy entry, each
from its fingerprint, and adds only new paths."
  (let* ((legacy (seq-filter (lambda (e) (not (assq 'cid (cdr e)))) entries))
         (remove (append (alist-get 'remove event) nil))
         (rename (mapcar (lambda (p) (cons (pos-bytes-key (car p)) (cdr p)))
                         (alist-get 'rename event)))
         (convert (mapcar (lambda (p) (cons (pos-bytes-key (car p)) (cdr p)))
                          (alist-get 'convert event)))
         (kept (seq-remove (lambda (e) (member (car e) remove)) entries))
         (moved (mapcar (lambda (e) (cons (or (cdr (assoc (car e) rename)) (car e)) (cdr e)))
                        kept))
         (bad (lambda () (pos-ledger-refuse 'entry "Invalid conversion: %s" file))))
    (unless (and (vectorp (alist-get 'remove event)) (listp (alist-get 'add event))
                 (listp (alist-get 'rename event)) (listp (alist-get 'convert event))
                 (seq-every-p (lambda (p) (assoc p legacy)) remove)
                 (seq-every-p (lambda (p) (and (assoc (car p) legacy) (not (member (car p) remove))
                                               (stringp (cdr p)) (pos-path-safe-p (cdr p))))
                              rename)
                 (equal (length (seq-uniq (mapcar #'car moved))) (length moved)))
      (funcall bad))
    ;; Every legacy entry left is converted, at its new path, from its fingerprint.
    (dolist (e moved)
      (let ((old (cdr e)))
        (unless (assq 'cid old)
          (let ((new (cdr (assoc (car e) convert))))
            (unless (and new (equal (alist-get 'from new) (alist-get 'sha256 old))
                         (pos-ledger--entry-p (assq-delete-all 'from (copy-sequence new)) 2))
              (funcall bad))))))
    (unless (seq-every-p (lambda (c) (let ((e (cdr (assoc (car c) moved))))
                                       (and e (not (assq 'cid e)))))
                         convert)
      (funcall bad))
    (let ((result (mapcar (lambda (e)
                            (let ((new (cdr (assoc (car e) convert))))
                              (if new (cons (car e) (assq-delete-all 'from (copy-sequence new))) e)))
                          moved)))
      (dolist (pair (alist-get 'add event))
        (let ((name (pos-bytes-key (car pair))))
          (unless (and (pos-path-safe-p name) (not (assoc name result))
                       (pos-ledger--entry-p (cdr pair) 2))
            (funcall bad))
          (push (cons name (cdr pair)) result)))
      result)))

(defun pos-ledger--collections-p (value)
  "Return non-nil if VALUE is a sorted vector of distinct safe paths."
  (and (vectorp value)
       (seq-every-p (lambda (p) (and (stringp p) (pos-path-safe-p p))) value)
       (equal (append value nil)
              (seq-uniq (sort (append value nil) #'string<)))))

(defconst pos-ledger--event-name
  "\\`\\([0-9]\\{8\\}\\)-\\([0-9a-f]\\{64\\}\\|baguqeera[a-z2-7]\\{52\\}\\)\\.json\\'"
  "An event file's name: its number, and its hash or, in schema 3, its CID.")

(defun pos-ledger-event-file-name (number id)
  "Return the name of the event file numbered NUMBER, of the hash or CID ID."
  (format "%08d-%s.json" number id))

(defun pos-ledger-event-file-id (file)
  "Return the hash or CID in the name of the event file FILE."
  (substring (file-name-nondirectory file) 9 -5))

(cl-defstruct (pos-history (:constructor pos-history--make) (:copier nil))
  "What an archive's ledger enrolled and recorded.
ENTRIES is an alist of path and entry; HEAD the last event's hash or,
in schema 3, its CID; EVENTS their number; FILES the event files; ROOT
the archive CID the last event recorded, if it records one; COLLECTIONS
every path sealed as a collection, sorted; ITEMS the path of each item
a schema 2 or 3 event sealed, sorted; EMPTY the empty directories the
schema 3 events enrolled, sorted.  HEAD-CID is the last event's CID as
a block, whatever its schema; LEDGER-ID the id the events name, if they
name one; SCHEMA the last event's; CONVERTED non-nil once an event has
converted the ledger to schema 2."
  entries head events files root collections items empty
  head-cid ledger-id schema converted)

(cl-defstruct (pos-event (:constructor pos-event--make) (:copier nil))
  "An event file, read and checked against the history before it.
FILE is its path, ID the hash or CID in its name and BYTES what it
holds.  FIELDS is the event as parsed, an alist, and SHAPE which of
`pos-ledger--shapes' it has."
  file id bytes fields shape)

(defconst pos-ledger--shapes
  '((add 1
         ("add" "previous" "schema")
         ("add" "ledger_id" "previous" "schema"))
    (seal 2
          ("add" "collections" "item" "ledger_id" "previous" "root" "schema"))
    (convert 2
             ("add" "collections" "convert" "kind" "ledger_id" "previous"
              "remove" "rename" "root" "schema"))
    (seal-blocks 3
                 ("add" "collections" "empty" "item" "ledger_id" "previous"
                  "root" "schema"))
    (to-blocks 3
               ("empty" "from" "kind" "ledger_id" "previous" "root" "schema")))
  "The shapes an event may have, each (SHAPE SCHEMA KEYS...).
An event has SHAPE if its schema is SCHEMA and its keys, sorted, are
one of KEYS.")

(defun pos-ledger--shape (fields)
  "Return the shape among `pos-ledger--shapes' that FIELDS has, or nil.
FIELDS is an event as parsed.  If it has a kind, the kind is conversion."
  (when (listp fields)
    (let ((keys (pos-ledger--keys fields)))
      (and (or (not (member "kind" keys))
               (equal (alist-get 'kind fields) "conversion"))
           (car (seq-find (pcase-lambda (`(,_ ,schema . ,key-sets))
                            (and (eql (alist-get 'schema fields) schema)
                                 (member keys key-sets)))
                          pos-ledger--shapes))))))

(defun pos-ledger--follows-p (shape fields history)
  "Return non-nil if an event of SHAPE, with FIELDS, may follow HISTORY."
  (pcase-let* (((cl-struct pos-history entries head schema converted) history)
               (legacy (seq-some (lambda (e) (not (assq 'cid (cdr e)))) entries)))
    (pcase shape
      ('add (not (memq schema '(2 3))))
      ('seal (not (eql schema 3)))
      ('convert (and (not converted) legacy))
      ('seal-blocks (memq schema '(nil 3)))
      ('to-blocks (and (eql schema 2) (not legacy)
                       (equal (alist-get 'from fields) head))))))

(defun pos-ledger--read-event (file history)
  "Return the event in FILE, a `pos-event', as the one that follows HISTORY.
Refuse a file whose name is not next in sequence or does not name its
bytes (sequence), and an event that has no shape, may not follow
HISTORY or does not name the event before it (chain)."
  (pos-ledger-regular file)
  (let* ((bytes (pos-bytes-read file))
         (name (file-name-nondirectory file))
         (id (and (string-match pos-ledger--event-name name)
                  (= (1+ (pos-history-events history))
                     (string-to-number (match-string 1 name)))
                  (match-string 2 name)))
         (blocked (pos-ledger-event-cid-p id)))
    (unless (and id (equal id (if blocked (pos-ledger-event-cid bytes)
                                (pos-bytes-sha bytes))))
      (pos-ledger-refuse 'sequence "Ledger sequence/hash failure: %s" file))
    (let* ((fields (if blocked (pos-ledger--strict bytes file)
                     (pos-bytes-parse bytes)))
           (shape (pos-ledger--shape fields))
           (version (and shape (alist-get 'schema fields)))
           (follows (cond ((and (eql version 3) (pos-history-head-cid history))
                           (pos-ledger-link (pos-history-head-cid history)))
                          ((pos-history-head history))
                          (t :null))))
      (unless (and shape
                   (pos-ledger--follows-p shape fields history)
                   (eq (eql version 3) blocked)
                   (equal (alist-get 'previous fields) follows))
        (pos-ledger-refuse 'chain "Ledger chain failure: %s" file))
      (pos-event--make :file file :id id :bytes bytes :fields fields :shape shape))))

(defun pos-ledger--fold-event (history event)
  "Add to HISTORY what EVENT, a `pos-event' read to follow it, records.
Refuse a ledger id that changes or is dropped (identity), and a root,
item, collection, empty directory or entry that is not valid (entry)."
  (pcase-let (((cl-struct pos-event file id bytes fields shape) event))
    (let-alist fields
      (setf (pos-history-schema history) .schema)
      (cond
       (.ledger_id
        (unless (and (pos-ledger--uuid-p .ledger_id)
                     (or (null (pos-history-ledger-id history))
                         (equal .ledger_id (pos-history-ledger-id history))))
          (pos-ledger-refuse 'identity "Ledger identity changed"))
        (setf (pos-history-ledger-id history) .ledger_id))
       ((pos-history-ledger-id history)
        (pos-ledger-refuse 'identity "Ledger identity removed")))
      (setf (pos-history-root history) (unless (eq shape 'add) .root))
      (pcase shape
        ('convert
         (setf (pos-history-converted history) t
               (pos-history-entries history)
               (pos-ledger--convert (pos-history-entries history) fields file))
         (unless (and (pos-ledger--cid-p .root)
                      (pos-ledger--collections-p .collections))
           (pos-ledger-refuse 'entry "Invalid root or collections: %s" file))
         (setf (pos-history-collections history)
               (append .collections (pos-history-collections history))))
        ((or 'seal 'seal-blocks)
         (unless (and (pos-ledger--cid-p .root)
                      (stringp .item) (pos-path-safe-p .item)
                      (pos-ledger--collections-p .collections)
                      (seq-every-p (lambda (p) (pos-path-within-p p .item))
                                   .collections)
                      (seq-every-p (lambda (pair)
                                     (pos-path-within-p
                                      (pos-bytes-key (car pair)) .item))
                                   .add))
           (pos-ledger-refuse 'entry "Invalid root, item or collections: %s" file))
         (when (eq shape 'seal-blocks)
           (unless (and (pos-ledger--collections-p .empty)
                        (seq-every-p (lambda (p) (pos-path-within-p p .item))
                                     .empty))
             (pos-ledger-refuse 'entry "Invalid empty directories: %s" file)))
         (push .item (pos-history-items history))
         (setf (pos-history-collections history)
               (append .collections (pos-history-collections history))))
        ('to-blocks
         (unless (and (pos-ledger--cid-p .root)
                      (pos-ledger--collections-p .empty))
           (pos-ledger-refuse 'entry "Invalid root or empty directories: %s" file))))
      (when (eql .schema 3)
        (setf (pos-history-empty history)
              (append .empty (pos-history-empty history))))
      (let ((add (unless (memq shape '(convert to-blocks)) .add)))
        (unless (listp add)
          (pos-ledger-refuse 'entry "Invalid ledger additions"))
        (dolist (pair add)
          (let ((name (pos-bytes-key (car pair))))
            (unless (pos-path-safe-p name)
              (pos-ledger-refuse 'entry "Unsafe relative path: %s" name))
            (when (or (equal (car (split-string name "/")) pos-ledger-directory)
                      (assoc name (pos-history-entries history)))
              (pos-ledger-refuse
               'entry "Ledger cannot replace an earlier entry or index itself"))
            (unless (pos-ledger--entry-p (cdr pair) (if (eql .schema 1) 1 2))
              (pos-ledger-refuse 'entry "Invalid ledger entry"))
            (push (cons name (cdr pair)) (pos-history-entries history))))))
    (setf (pos-history-head history) id
          (pos-history-head-cid history)
          (if (pos-ledger-event-cid-p id) id
            (pos-ledger-event-cid (pos-ledger-as-block bytes))))
    (cl-incf (pos-history-events history))
    (push file (pos-history-files history))))

(defun pos-ledger-history (archive)
  "Return ARCHIVE's ledger as a `pos-history': what it enrolled and recorded.
No ledger is a history of no events, with every other field nil."
  (let ((folder (pos-ledger-folder archive))
        (history (pos-history--make :events 0)))
    (cond
     ((not (or (file-exists-p folder) (file-symlink-p folder)))
      history)
     ((or (file-symlink-p folder) (not (file-directory-p folder)))
      (pos-ledger-refuse 'ledger "Invalid ledger: %s" folder))
     (t
      (dolist (name (pos-ledger-entry-names folder))
        (pos-ledger--fold-event
         history (pos-ledger--read-event (expand-file-name name folder) history)))
      (pcase-let (((cl-struct pos-history entries files collections items empty)
                   history))
        (unless files
          (pos-ledger-refuse 'empty "Empty ledger needs investigation: %s" folder))
        (setf (pos-history-entries history) (seq-sort-by #'car #'string< entries)
              (pos-history-files history) (nreverse files)
              (pos-history-collections history)
              (seq-uniq (sort collections #'string<))
              (pos-history-items history) (sort items #'string<)
              (pos-history-empty history) (seq-uniq (sort empty #'string<))))
      history))))

(defun pos-ledger-event (add previous number &optional ledger-id)
  "Return (NAME . BYTES), event NUMBER enrolling ADD after PREVIOUS.
ADD is an alist of path and entry; PREVIOUS a hash or nil."
  (let ((bytes (pos-bytes-json
                `((schema . 1) (previous . ,(or previous :null)) (add . ,add)
                  ,@(when ledger-id `((ledger_id . ,ledger-id)))))))
    (cons (pos-ledger-event-file-name number (pos-bytes-sha bytes)) bytes)))

;;;; Kept archives

(defun pos-ledger--entry (archive)
  "Return the entry for ARCHIVE's scope in its node's configuration.
ARCHIVE's scope is the directory holding it, and its node the nearest
directory at or above the scope that holds a configuration, as
doc/pos-directory.txt names one.  The entry is the one under archives
there whose scope is the scope's path from the node, or nil: for a
scope it does not name, or beneath no node.  Refuse `config' for a
configuration that is refused, and for two in one node."
  (let* ((scope (file-name-directory (directory-file-name (expand-file-name archive))))
         (node (locate-dominating-file scope #'pos-path-config-files))
         (files (and node (pos-path-config-files node))))
    (when (cdr files)
      (pos-ledger-refuse 'config "Configuration refused (two-configurations): %s"
                         node))
    (when files
      (require 'pos-tree)
      (let* ((file (expand-file-name (car files) node))
             (config
              (condition-case err
                  (pos-tree-read-config
                   (decode-coding-string (pos-bytes-read file) 'utf-8))
                (pos-tree-refused
                 (pos-ledger-refuse 'config "Configuration refused (%s): %s"
                                    (cadr err) file))))
             (path (directory-file-name
                    (file-relative-name scope (expand-file-name node)))))
        (seq-find (lambda (a) (equal (alist-get 'scope a) path))
                  (alist-get 'archives config))))))

(defun pos-ledger-kept (archive)
  "Return the base URL of ARCHIVE's keeper, or nil if it is kept on disk.
A keeper has the archive of a scope whose entry in its node's
configuration is kept remote; every other is on disk."
  (let ((entry (pos-ledger--entry archive)))
    (and entry (equal (alist-get 'kept entry) "remote")
         (alist-get 'url entry))))

(defun pos-ledger-named (archive)
  "Return the id ARCHIVE's ledger is to have, or nil if none is named.
The ledger key of the scope's entry in its node's configuration: a
keeper may be bound to a ledger's id before the
ledger has an event, so the entry says what the id is."
  (alist-get 'ledger (pos-ledger--entry archive)))

(defun pos-ledger-identity (files)
  "Return the ledger_id of the last of the event FILES that has one."
  (seq-some (lambda (file)
              (alist-get 'ledger_id (pos-bytes-parse (pos-bytes-read file))))
            (reverse files)))

(defun pos-ledger-as-named (archive files)
  "Refuse `identity' unless ARCHIVE's ledger, of event FILES, is the one named.
A ledger with no event yet, or an archive no entry names a ledger for,
is not refused."
  (let ((named (pos-ledger-named archive)))
    (when (and named files (not (equal named (pos-ledger-identity files))))
      (pos-ledger-refuse 'identity "Not the ledger its entry names, %s: %s"
                         named archive))))

(defun pos-ledger-kept-here (dir)
  "Return the archive of the scope DIR, if kept by a keeper and not here.
DIR has a ledger beside where its archive would be, and no archives."
  (let ((archive (expand-file-name "archives" dir)))
    (and (not (file-exists-p archive)) (not (file-symlink-p archive))
         (file-directory-p (expand-file-name (concat pos-ledger-integrity "/ledger")
                                             dir))
         (pos-ledger-kept archive)
         archive)))

(defun pos-ledger-kept-entries (archive known)
  "Return KNOWN, the ledger's entries, as the inventory of the kept ARCHIVE.
Refuse `kept' if ARCHIVE has files on disk: it is not with its keeper yet."
  (when (pos-ledger-inventory archive)
    (pos-ledger-refuse 'kept "Kept by a keeper, and has files on disk: %s" archive))
  known)

(defun pos-ledger--ask (url cids)
  "Return what the keeper at URL has of a ledger, for its archive's report.
CIDS is what the ledger folds to.  An alist: head and events as the
keeper describes them, and erased, the paths the ledger enrols whose
bytes the keeper has erased, sorted.  Nil if no keeper is to be asked.
Refuse as the keeper does."
  (unless (or pos-ledger-offline
              (not (member (getenv "POS_ARCHIVE_OFFLINE") '(nil ""))))
    (require 'pos-remote)
    (let* ((described (pos-remote-describe (funcall pos-remote-keeper-function url)))
           (erased (append (alist-get 'erased described) nil)))
      `((head . ,(alist-get 'head described))
        (events . ,(alist-get 'events described))
        (erased . ,(vconcat
                    (sort (delq nil (mapcar (lambda (pair)
                                              (and (member (cdr pair) erased)
                                                   (not (equal (car pair) "."))
                                                   (car pair)))
                                            cids))
                          #'string<)))))))

;;;; Discovery and checkpoints

(defun pos-ledger-checked (root)
  "Return ROOT absolute, refusing a symlink in it or its ancestors.
The system aliases /tmp and /var are allowed."
  (let ((path (directory-file-name (expand-file-name root))))
    (named-let up ((part path))
      (when (and (file-symlink-p part) (not (member part '("/tmp" "/var"))))
        (pos-ledger-refuse 'link "Symlink: %s" part))
      (let ((parent (directory-file-name (file-name-directory part))))
        (unless (equal parent part) (up parent))))
    path))

(defun pos-ledger--path< (a b)
  "Return non-nil if path A precedes B, component by component."
  (let ((as (split-string a "/")) (bs (split-string b "/")))
    (while (and as bs (equal (car as) (car bs)))
      (setq as (cdr as) bs (cdr bs)))
    (cond ((null as) (and bs t))
          ((null bs) nil)
          (t (string< (car as) (car bs))))))

(defun pos-ledger-roots (root)
  "Return the outermost archives under ROOT, in path order.
An archive a keeper keeps is found by its ledger, with no directory of
its own."
  (let ((root (file-truename (pos-ledger-checked root))) found)
    (unless (or (file-directory-p root)
                (and (equal (file-name-nondirectory root) "archives")
                     (pos-ledger-kept-here (file-name-directory root))))
      (pos-ledger-refuse 'root "Root must be an existing directory"))
    (if (equal (file-name-nondirectory root) "archives")
        (list root)
      (named-let walk ((dir root))
        (when-let* ((kept (pos-ledger-kept-here dir)))
          (push kept found))
        (dolist (name (pos-ledger-entry-names dir))
          (let ((path (expand-file-name name dir)))
            (when (and (file-directory-p path)
                       (not (string-match-p "\\`[._]" name)))
              (when (file-symlink-p path)
                (pos-ledger-refuse 'link "Symlink in discovery: %s" path))
              (if (equal name "archives")
                  (push path found)
                (walk path))))))
      (sort found #'pos-ledger--path<))))

(defun pos-ledger--anchor-homes (root)
  "Return the checkpoint directories covering ROOT, legacy and current."
  (let* ((root (file-truename (pos-ledger-checked root)))
         (base (if (equal (file-name-nondirectory root) "archives")
                   (file-name-directory root)
                 root)))
    (list (expand-file-name pos-ledger-anchors base)
          (expand-file-name (concat pos-ledger-integrity "/checkpoints") base))))

(defun pos-ledger-checkpoint-files (root archives)
  "Return the checkpoint files covering ROOT and ARCHIVES."
  (let (files)
    (dolist (folder (sort (delete-dups (mapcan #'pos-ledger--anchor-homes
                                                (cons root archives)))
                          #'pos-ledger--path<))
      (cond
       ((not (or (file-exists-p folder) (file-symlink-p folder))))
       ((or (file-symlink-p folder) (not (file-directory-p folder)))
        (pos-ledger-refuse 'checkpoint "Invalid checkpoint directory"))
       (t (setq files (append files (mapcar (lambda (n) (expand-file-name n folder))
                                            (pos-ledger-entry-names folder)))))))
    files))

(defun pos-ledger--ledger-file-p (file)
  "Return non-nil if FILE is in a ledger folder, legacy or current."
  (let* ((parent (directory-file-name (file-name-directory file)))
         (grandparent (directory-file-name (file-name-directory parent))))
    (or (equal (file-name-nondirectory parent) pos-ledger-directory)
        (and (equal (file-name-nondirectory parent) "ledger")
             (equal (file-name-nondirectory grandparent) pos-ledger-integrity)))))

(defun pos-ledger--ledger-hashes (archive)
  "Return the hash and block CID of every ledger event in or beside ARCHIVE.
Its own ledger's events and those of every ledger within it; an event
goes by one or the other.  Of an archive a keeper keeps, the ledgers
within are known by the names their events are enrolled under."
  (let ((folder (pos-ledger-folder archive)))
    (append
     (mapcan (lambda (file)
               (pos-ledger-regular file)
               (let ((bytes (pos-bytes-read file)))
                 (list (pos-bytes-sha bytes) (pos-ledger-event-cid bytes))))
             (append
              (when (file-directory-p folder)
                (directory-files folder t "\\.json\\'"))
              (when (file-directory-p archive)
                (seq-filter #'pos-ledger--ledger-file-p
                            (directory-files-recursively archive "\\.json\\'"
                                                         nil nil nil)))))
     (when (pos-ledger-kept archive)
       (delq nil
             (mapcar (lambda (pair)
                       (pcase (reverse (split-string (car pair) "/"))
                         ((and `(,name . ,folders)
                               (guard (or (equal (car folders) pos-ledger-directory)
                                          (and (equal (car folders) "ledger")
                                               (equal (cadr folders)
                                                      pos-ledger-integrity))))
                               (guard (string-match pos-ledger--event-name name)))
                          (match-string 2 name))))
                     (pos-history-entries (pos-ledger-history archive))))))))

(defun pos-ledger--check-anchors (root archives)
  "Refuse unless every head a checkpoint for ROOT names is in ARCHIVES."
  (let ((archive-root (equal (file-name-nondirectory (directory-file-name root))
                             "archives"))
        required present)
    (dolist (file (pos-ledger-checkpoint-files root archives))
      (pos-ledger-regular file)
      (let* ((bytes (pos-bytes-read file))
             (value (and (equal (file-name-nondirectory file)
                                (concat (pos-bytes-sha bytes) ".json"))
                         (pos-bytes-parse bytes))))
        (unless value
          (pos-ledger-refuse 'checkpoint "Checkpoint hash failure: %s" file))
        (unless (and (listp value)
                     (equal (pos-ledger--keys value) '("coverage" "heads" "schema"))
                     (eql 1 (alist-get 'schema value))
                     (member (alist-get 'coverage value) '("archive" "tree"))
                     (vectorp (alist-get 'heads value))
                     (seq-every-p (lambda (head) (or (pos-ledger--hex-p head)
                                                     (pos-ledger-event-cid-p head)))
                                  (alist-get 'heads value)))
          (pos-ledger-refuse 'checkpoint "Invalid checkpoint"))
        (when (or (not archive-root) (equal (alist-get 'coverage value) "archive"))
          (setq required (append (alist-get 'heads value) required)))))
    (dolist (archive archives)
      (setq present (append (pos-ledger--ledger-hashes archive) present)))
    (let ((absent (sort (seq-uniq (seq-difference required present)) #'string<)))
      (when absent
        (pos-ledger-refuse
         'anchor "Missing anchored ledger (archive removed or history truncated): %s"
         (string-join absent ", "))))))

;;;; Check

(defun pos-ledger-cids (archive)
  "Return the CIDs of ARCHIVE and everything in it, as `pos-cid-tree'.
Nil if ARCHIVE is not yet made."
  (and (file-exists-p archive) (pos-cid-tree archive)))

(defun pos-ledger-fold (entries &optional empty)
  "Return the CIDs the enrolled ENTRIES and the EMPTY directories give.
As `pos-cid-tree' gives them from disk, the root as \".\".  A hidden
entry is left out, as IPFS leaves it out.  Refuses `entry' when an
entry records no CID, as a legacy ledger's do."
  (pos-cid-inventory
   (delq nil
         (mapcar (lambda (pair)
                   (let ((path (pos-bytes-key (car pair)))
                         (entry (cdr pair)))
                     (unless (assq 'cid entry)
                       (pos-ledger-refuse 'entry "No CID enrolled for %s" path))
                     (unless (seq-some (lambda (part) (string-prefix-p "." part))
                                       (split-string path "/"))
                       (list path (alist-get 'cid entry) (alist-get 'size entry)))))
                 entries))
   empty))

(defun pos-ledger-fold-cids (archive)
  "Return ARCHIVE's CIDs from its ledger alone, as `pos-cid-tree' gives them.
Every enrolled file's CID as recorded, every directory's derived from
them, the empty ones as recorded, the root as \".\"; nothing is read from
the archive itself.  Refuses `entry' when an enrolled entry records no
CID, as a legacy ledger's do."
  (let ((history (pos-ledger-history archive)))
    (pos-ledger-fold (pos-history-entries history) (pos-history-empty history))))

(defun pos-ledger-with-cids (known actual cids)
  "Return ACTUAL with a CID from CIDS in each entry KNOWN records one for."
  (mapcar (lambda (pair)
            (let ((recorded (cdr (assoc (car pair) known))))
              (if (and cids (assq 'cid recorded))
                  (cons (car pair) (cons (cons 'cid (cdr (assoc (car pair) cids)))
                                         (cdr pair)))
                pair)))
          actual))

(defun pos-ledger--hidden (inventory)
  "Return INVENTORY's hidden paths, other than those in legacy ledger folders."
  (seq-filter (lambda (path)
                (let ((parts (split-string path "/")))
                  (and (seq-some (lambda (p) (string-prefix-p "." p)) parts)
                       (not (seq-some (lambda (p) (member p (list pos-ledger-directory
                                                                   pos-ledger-anchors)))
                                      parts)))))
              (mapcar #'car inventory)))

(defun pos-ledger-capsule-p (directory)
  "Return non-nil if DIRECTORY has a capsule's manifest.
A capsule is a frozen snapshot, kept byte for byte: its manifest.json
has schema_version 1, entries and an entrypoint."
  (let ((manifest (expand-file-name "manifest.json" directory)))
    (and (file-regular-p manifest)
         (let ((value (ignore-errors (pos-bytes-parse (pos-bytes-read manifest)))))
           (and (listp value) value
                (eql 1 (alist-get 'schema_version value))
                (assq 'entries value) (assq 'entrypoint value))))))

(defun pos-ledger-declared-p (directory)
  "Return non-nil if DIRECTORY declares itself a collection.
Its README.org has the declaration line, or it is a capsule."
  (let ((readme (expand-file-name "README.org" directory)))
    (or (and (file-regular-p readme)
             (member (encode-coding-string pos-ledger-declaration 'utf-8)
                     (split-string (pos-bytes-read readme) "\n")))
        (pos-ledger-capsule-p directory))))

(defun pos-ledger--undeclared (archive collections)
  "Return those of COLLECTIONS in ARCHIVE that no longer declare themselves."
  (seq-remove (lambda (path) (pos-ledger-declared-p (expand-file-name path archive)))
              collections))

(defun pos-ledger-differences (known actual)
  "Return the missing, changed and new paths between KNOWN and ACTUAL."
  (let (missing changed new)
    (dolist (pair known)
      (let ((now (assoc (car pair) actual)))
        (cond ((null now) (push (car pair) missing))
              ((not (equal (seq-sort-by #'car #'string< (cdr pair))
                           (seq-sort-by #'car #'string< (cdr now))))
               (push (car pair) changed)))))
    (dolist (pair actual)
      (unless (assoc (car pair) known) (push (car pair) new)))
    (list (sort missing #'string<) (sort changed #'string<) (sort new #'string<))))

(defun pos-ledger-check (root)
  "Return the check report on every archive under ROOT.
A list of alists, one an archive: archive, kept, keeper, head, events,
files, writable, missing, changed, new, root, recorded_root, hidden and
undeclared; the lists as vectors, so the report is a JSON value.  An
archive a keeper keeps is reported from its ledger, nothing of it being
on disk to read, and its keeper is asked what it holds unless
`pos-ledger-offline' says not.  A write bit on a ledger event or a
checkpoint is not reported: each is named by a hash of its bytes, and a
clone does not keep the bit."
  (let* ((archives (pos-ledger-roots root))
         (_ (pos-ledger--check-anchors root archives))
         reports)
    (dolist (archive archives)
      (pcase-let* (((cl-struct pos-history (entries known) head events files (root recorded)
                               collections empty)
                    (pos-ledger-history archive))
                   (_ (pos-ledger-as-named archive files))
                   (kept (pos-ledger-kept archive))
                   (actual (if kept (pos-ledger-kept-entries archive known)
                             (pos-ledger-inventory archive)))
                   (cids (if kept (and known (pos-ledger-fold known empty))
                           (pos-ledger-cids archive)))
                   (`(,missing ,changed ,new)
                    (unless kept
                      (pos-ledger-differences
                       known (pos-ledger-with-cids known actual cids))))
                   (writable
                    (unless kept
                      (sort (seq-filter (lambda (name)
                                          (pos-ledger--writable-p
                                           (expand-file-name name archive)))
                                        (mapcar #'car actual))
                            #'string<))))
        (push `((archive . ,archive) (kept . ,(or kept :null))
                (keeper . ,(or (and kept (pos-ledger--ask kept cids)) :null))
                (head . ,(or head :null)) (events . ,events)
                (files . ,(length actual)) (writable . ,(vconcat writable))
                (root . ,(or (cdr (assoc "." cids)) :null))
                (recorded_root . ,(or recorded :null))
                (hidden . ,(vconcat (pos-ledger--hidden actual)))
                (undeclared . ,(vconcat (unless kept
                                          (pos-ledger--undeclared archive collections))))
                (missing . ,(vconcat missing)) (changed . ,(vconcat changed))
                (new . ,(vconcat new)))
              reports)))
    (nreverse reports)))

(provide 'pos-ledger)
;;; pos-ledger.el ends here
