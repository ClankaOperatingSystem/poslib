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
;; lockstep with pyposlib.  Writing them is not here yet.
;;
;; - `pos-ledger-json': DAG-JSON bytes of a value, and a newline.
;; - `pos-ledger-block': the same without the newline, a block.
;; - `pos-ledger-inventory': what an archive holds.
;; - `pos-ledger-history': what its ledger enrolled.
;; - `pos-ledger-fold-cids': the archive's CIDs from the ledger alone.
;; - `pos-ledger-event': a new event's name and bytes.
;; - `pos-ledger-check': the report on every archive under a root.
;;
;; JSON values are Lisp values: objects alists, arrays vectors, strings,
;; integers and :null.  Refusals signal `pos-ledger-refused' with a kind
;; symbol and a message.

;;; Code:

(require 'subr-x)
(require 'pos-cid)

(defconst pos-ledger-integrity "archive-integrity"
  "The folder beside an archive holding its ledger and checkpoints.")

(defconst pos-ledger-directory ".archive-integrity"
  "The legacy ledger's directory, at the top of an archive.")

(defconst pos-ledger-anchors ".archive-integrity-anchors"
  "The legacy checkpoints' directory, beside the root they cover.")

(defconst pos-ledger-declaration "#+COLLECTION: t"
  "The line in a collection's README.org that declares it one.")

(define-error 'pos-ledger-refused "Archive integrity refused")

(defun pos-ledger--refuse (kind format &rest args)
  "Signal a refusal of KIND, with a message from FORMAT and ARGS."
  (signal 'pos-ledger-refused (list kind (apply #'format format args))))

;;;; DAG-JSON

(defun pos-ledger--utf8< (a b)
  "Return non-nil if the string A precedes B as UTF-8 bytes."
  (string< (encode-coding-string a 'utf-8 t) (encode-coding-string b 'utf-8 t)))

(defun pos-ledger--key (key)
  "Return KEY, a symbol or string, as a string."
  (if (symbolp key) (symbol-name key) key))

(defun pos-ledger--string (string)
  "Return STRING as a JSON string literal."
  (concat "\""
          (mapconcat
           (lambda (c)
             (pcase c
               (?\" "\\\"") (?\\ "\\\\")
               (?\b "\\b") (?\f "\\f") (?\n "\\n") (?\r "\\r") (?\t "\\t")
               ((pred (> #x20)) (format "\\u%04x" c))
               (_ (string c))))
           string "")
          "\""))

(defun pos-ledger--encode (value)
  "Return VALUE as canonical JSON text, without the final newline."
  (cond
   ((eq value :null) "null")
   ((stringp value) (pos-ledger--string value))
   ((integerp value)
    (unless (<= (- (expt 2 63)) value (1- (expt 2 63)))
      (error "No DAG-JSON for an integer of more than 64 bits: %S" value))
    (number-to-string value))
   ((vectorp value) (concat "[" (mapconcat #'pos-ledger--encode value ",") "]"))
   ((listp value)
    (concat "{"
            (mapconcat (lambda (pair)
                         (concat (pos-ledger--string (pos-ledger--key (car pair)))
                                 ":" (pos-ledger--encode (cdr pair))))
                       (sort (copy-sequence value)
                             (lambda (a b) (pos-ledger--utf8< (pos-ledger--key (car a))
                                                              (pos-ledger--key (car b)))))
                       ",")
            "}"))
   (t (error "No canonical JSON for %S" value))))

(defun pos-ledger-json (value)
  "Return the canonical JSON bytes of VALUE, with its final newline."
  (encode-coding-string (concat (pos-ledger--encode value) "\n") 'utf-8))

(defun pos-ledger--parse (bytes)
  "Return the JSON value in BYTES."
  (json-parse-string (decode-coding-string bytes 'utf-8) :object-type 'alist
                     :null-object :null :false-object :false))

(defun pos-ledger--sha (bytes)
  "Return the lower-case hex SHA-256 of BYTES."
  (secure-hash 'sha256 bytes))

(defun pos-ledger-block (value)
  "Return VALUE as a DAG-JSON block: its canonical bytes, with no newline."
  (encode-coding-string (pos-ledger--encode value) 'utf-8))

(defun pos-ledger--plain-p (value)
  "Return non-nil if the JSON VALUE holds only what a block may.
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
                   (let ((parsed (pos-ledger--parse bytes)))
                     (and (pos-ledger--plain-p parsed)
                          (equal (pos-ledger-block parsed) bytes)
                          (list parsed)))
                 (error nil))))
    (unless value
      (pos-ledger--refuse 'encoding "Not a DAG-JSON block: %s" file))
    (car value)))

(defun pos-ledger--event-cid (bytes)
  "Return the CID a schema 3 event of BYTES is named by."
  (pos-cid-block pos-cid-dag-json bytes))

(defun pos-ledger--event-cid-p (value)
  "Return non-nil if VALUE has the form of a schema 3 event's CID."
  (and (stringp value)
       (string-match-p "\\`baguqeera[a-z2-7]\\{52\\}\\'" value)
       t))

(defun pos-ledger--link (cid)
  "Return a DAG-JSON link to CID."
  `((/ . ,cid)))

(defun pos-ledger--as-block (bytes)
  "Return BYTES, an event written with a final newline, as a block without it."
  (if (string-suffix-p "\n" bytes) (substring bytes 0 -1) bytes))

;;;; Files

(defun pos-ledger--read (file)
  "Return FILE's bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun pos-ledger--regular (file)
  "Return FILE's attributes, refusing all but a regular file of one link."
  (let ((attributes (file-attributes file)))
    (unless (and attributes (null (file-attribute-type attributes))
                 (= 1 (file-attribute-link-number attributes)))
      (pos-ledger--refuse 'link "Expected a regular file with one link: %s" file))
    attributes))

(defun pos-ledger--mode (file)
  "Return FILE's permission bits."
  (logand (file-modes file 'nofollow) #o7777))

(defun pos-ledger--writable-p (file)
  "Return non-nil if FILE has a write bit."
  (pos-ledger--regular file)
  (/= 0 (logand (pos-ledger--mode file) #o222)))

(defun pos-ledger-record (file)
  "Return FILE's entry: its SHA-256, size and mode less write bits."
  (let* ((before (pos-ledger--regular file))
         (bytes (pos-ledger--read file))
         (after (pos-ledger--regular file)))
    (unless (and (equal (file-attribute-inode-number before)
                        (file-attribute-inode-number after))
                 (equal (file-attribute-size before) (file-attribute-size after))
                 (equal (file-attribute-modification-time before)
                        (file-attribute-modification-time after)))
      (pos-ledger--refuse 'changed "File changed while reading: %s" file))
    (list (cons 'mode (logand (pos-ledger--mode file) (lognot #o222)))
          (cons 'sha256 (pos-ledger--sha bytes))
          (cons 'size (length bytes)))))

(defun pos-ledger--entries (dir)
  "Return DIR's entry names, sorted."
  (sort (directory-files dir nil directory-files-no-dot-files-regexp) #'string<))

(defun pos-ledger-inventory (archive)
  "Return ARCHIVE's inventory: an alist of relative path and entry.
An archive not yet made holds nothing."
  (let (result)
    (named-let walk ((dir archive) (rel nil))
      (dolist (name (unless (and (null rel) (not (file-exists-p dir))
                                 (not (file-symlink-p dir)))
                      (pos-ledger--entries dir)))
        (let ((path (expand-file-name name dir))
              (child (if rel (concat rel "/" name) name)))
          (cond
           ((and (null rel) (equal name pos-ledger-directory))
            (unless (and (file-directory-p path) (not (file-symlink-p path)))
              (pos-ledger--refuse 'ledger "Integrity metadata must be a directory")))
           ((file-symlink-p path)
            (pos-ledger--refuse 'link "Symlink in archive: %s" path))
           ((file-directory-p path) (walk path child))
           (t (push (cons child (pos-ledger-record path)) result))))))
    (sort result (lambda (a b) (string< (car a) (car b))))))

;;;; History

(defun pos-ledger--safe-p (path)
  "Return non-nil if PATH is a plain relative path."
  (and (not (string-empty-p path))
       (not (string-prefix-p "/" path))
       (not (string-match-p "\\\\" path))
       (not (seq-some (lambda (part) (member part '("" "." "..")))
                      (split-string path "/")))))

(defun pos-ledger--keys (object)
  "Return OBJECT's keys as sorted strings."
  (sort (mapcar (lambda (pair) (pos-ledger--key (car pair))) object) #'string<))

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
           (pos-ledger--refuse 'ledger "Two ledgers for one archive: %s" archive))
          ((or (file-exists-p beside) (file-symlink-p beside)) beside)
          (t inside))))

(defun pos-ledger--convert (entries event file)
  "Return ENTRIES after the conversion EVENT, in the event FILE.
Refuse unless it removes, renames or converts every legacy entry, each
from its fingerprint, and adds only new paths."
  (let* ((legacy (seq-filter (lambda (e) (not (assq 'cid (cdr e)))) entries))
         (remove (append (alist-get 'remove event) nil))
         (rename (mapcar (lambda (p) (cons (pos-ledger--key (car p)) (cdr p)))
                         (alist-get 'rename event)))
         (convert (mapcar (lambda (p) (cons (pos-ledger--key (car p)) (cdr p)))
                          (alist-get 'convert event)))
         (kept (seq-remove (lambda (e) (member (car e) remove)) entries))
         (moved (mapcar (lambda (e) (cons (or (cdr (assoc (car e) rename)) (car e)) (cdr e)))
                        kept))
         (bad (lambda () (pos-ledger--refuse 'entry "Invalid conversion: %s" file))))
    (unless (and (vectorp (alist-get 'remove event)) (listp (alist-get 'add event))
                 (listp (alist-get 'rename event)) (listp (alist-get 'convert event))
                 (seq-every-p (lambda (p) (assoc p legacy)) remove)
                 (seq-every-p (lambda (p) (and (assoc (car p) legacy) (not (member (car p) remove))
                                               (stringp (cdr p)) (pos-ledger--safe-p (cdr p))))
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
        (let ((name (pos-ledger--key (car pair))))
          (unless (and (pos-ledger--safe-p name) (not (assoc name result))
                       (pos-ledger--entry-p (cdr pair) 2))
            (funcall bad))
          (push (cons name (cdr pair)) result)))
      result)))

(defun pos-ledger--within-p (path item)
  "Return non-nil if PATH is ITEM or lies within it."
  (or (equal path item) (string-prefix-p (concat item "/") path)))

(defun pos-ledger--collections-p (value)
  "Return non-nil if VALUE is a sorted vector of distinct safe paths."
  (and (vectorp value)
       (seq-every-p (lambda (p) (and (stringp p) (pos-ledger--safe-p p))) value)
       (equal (append value nil)
              (seq-uniq (sort (append value nil) #'string<)))))

(defconst pos-ledger--event-name
  "\\`\\([0-9]\\{8\\}\\)-\\([0-9a-f]\\{64\\}\\|baguqeera[a-z2-7]\\{52\\}\\)\\.json\\'"
  "An event file's name: its number, and its hash or, in schema 3, its CID.")

(defun pos-ledger-history (archive)
  "Return ARCHIVE's ledger as a list of what it enrolled and recorded.
\(ENTRIES HEAD EVENTS FILES ROOT COLLECTIONS ITEMS EMPTY): ENTRIES is
an alist of path and entry; HEAD the last event's hash or, in
schema 3, its CID; EVENTS their number; FILES the event files; ROOT the
archive CID the last event recorded, if it records one; COLLECTIONS
every path sealed as a collection, sorted; ITEMS the path of each item
a schema 2 or 3 event sealed, sorted; EMPTY the empty directories the
schema 3 events enrolled, sorted.  No ledger is (nil nil 0 nil nil nil
nil nil)."
  (let ((folder (pos-ledger-folder archive))
        entries previous previous-cid files ledger-id (number 0) schema root
        collections items converted empty)
    (cond
     ((not (or (file-exists-p folder) (file-symlink-p folder)))
      (list nil nil 0 nil nil nil nil nil))
     ((or (file-symlink-p folder) (not (file-directory-p folder)))
      (pos-ledger--refuse 'ledger "Invalid ledger: %s" folder))
     (t
      (dolist (name (pos-ledger--entries folder))
        (let* ((path (expand-file-name name folder))
               (_ (pos-ledger--regular path))
               (bytes (pos-ledger--read path))
               (id (and (string-match pos-ledger--event-name name)
                        (= (1+ number) (string-to-number (match-string 1 name)))
                        (match-string 2 name)))
               (blocked (pos-ledger--event-cid-p id)))
          (setq number (1+ number))
          (unless (and id (equal id (if blocked (pos-ledger--event-cid bytes)
                                      (pos-ledger--sha bytes))))
            (pos-ledger--refuse 'sequence "Ledger sequence/hash failure: %s" path))
          (let* ((event (if blocked (pos-ledger--strict bytes path)
                          (pos-ledger--parse bytes)))
                 (keys (and (listp event) (pos-ledger--keys event)))
                 (version (and (listp event) (alist-get 'schema event)))
                 (legacy (seq-some (lambda (e) (not (assq 'cid (cdr e)))) entries))
                 (conversion
                  (and (eql version 2)
                       (equal keys '("add" "collections" "convert" "kind" "ledger_id"
                                     "previous" "remove" "rename" "root" "schema"))
                       (equal (alist-get 'kind event) "conversion")
                       (not converted) legacy))
                 (to-blocks
                  (and (eql version 3)
                       (equal keys '("empty" "from" "kind" "ledger_id" "previous"
                                     "root" "schema"))
                       (equal (alist-get 'kind event) "conversion")
                       (eql schema 2) (not legacy)
                       (equal (alist-get 'from event) previous)))
                 (follows (cond ((and (eql version 3) previous-cid)
                                 (pos-ledger--link previous-cid))
                                (previous)
                                (t :null))))
            (unless (and (or (and (eql version 1) (not (memq schema '(2 3)))
                                  (member keys '(("add" "previous" "schema")
                                                 ("add" "ledger_id" "previous"
                                                  "schema"))))
                             (and (eql version 2) (not (eql schema 3))
                                  (equal keys '("add" "collections" "item" "ledger_id"
                                                "previous" "root" "schema")))
                             conversion
                             (and (eql version 3) (memq schema '(nil 3))
                                  (equal keys '("add" "collections" "empty" "item"
                                                "ledger_id" "previous" "root" "schema")))
                             to-blocks)
                         (eq (eql version 3) blocked)
                         (equal (alist-get 'previous event) follows))
              (pos-ledger--refuse 'chain "Ledger chain failure: %s" path))
            (setq schema version)
            (let ((event-id (alist-get 'ledger_id event)))
              (cond
               (event-id
                (unless (and (pos-ledger--uuid-p event-id)
                             (or (null ledger-id) (equal event-id ledger-id)))
                  (pos-ledger--refuse 'identity "Ledger identity changed"))
                (setq ledger-id event-id))
               (ledger-id
                (pos-ledger--refuse 'identity "Ledger identity removed"))))
            (setq root (when (memq schema '(2 3)) (alist-get 'root event)))
            (when conversion
              (setq converted t
                    entries (pos-ledger--convert entries event path)))
            (when (and (memq schema '(2 3)) (not conversion) (not to-blocks))
              (let ((item (alist-get 'item event)))
                (unless (and (pos-ledger--cid-p root)
                             (stringp item) (pos-ledger--safe-p item)
                             (pos-ledger--collections-p (alist-get 'collections event))
                             (seq-every-p (lambda (p) (pos-ledger--within-p p item))
                                          (alist-get 'collections event))
                             (seq-every-p (lambda (pair)
                                            (pos-ledger--within-p
                                             (pos-ledger--key (car pair)) item))
                                          (alist-get 'add event)))
                  (pos-ledger--refuse 'entry "Invalid root, item or collections: %s" path))
                (when (eql schema 3)
                  (unless (and (pos-ledger--collections-p (alist-get 'empty event))
                               (seq-every-p (lambda (p) (pos-ledger--within-p p item))
                                            (alist-get 'empty event)))
                    (pos-ledger--refuse 'entry "Invalid empty directories: %s" path)))
                (push item items))
              (setq collections (append (alist-get 'collections event) collections)))
            (when conversion
              (unless (and (pos-ledger--cid-p root)
                           (pos-ledger--collections-p (alist-get 'collections event)))
                (pos-ledger--refuse 'entry "Invalid root or collections: %s" path))
              (setq collections (append (alist-get 'collections event) collections)))
            (when to-blocks
              (unless (and (pos-ledger--cid-p root)
                           (pos-ledger--collections-p (alist-get 'empty event)))
                (pos-ledger--refuse 'entry "Invalid root or empty directories: %s" path)))
            (when (eql schema 3)
              (setq empty (append (alist-get 'empty event) empty)))
            (let ((add (unless (or conversion to-blocks) (alist-get 'add event))))
              (unless (listp add)
                (pos-ledger--refuse 'entry "Invalid ledger additions"))
              (dolist (pair add)
                (let ((name (pos-ledger--key (car pair))))
                  (unless (pos-ledger--safe-p name)
                    (pos-ledger--refuse 'entry "Unsafe relative path: %s" name))
                  (when (or (equal (car (split-string name "/")) pos-ledger-directory)
                            (assoc name entries))
                    (pos-ledger--refuse
                     'entry "Ledger cannot replace an earlier entry or index itself"))
                  (unless (pos-ledger--entry-p (cdr pair) (if (eql schema 1) 1 2))
                    (pos-ledger--refuse 'entry "Invalid ledger entry"))
                  (push (cons name (cdr pair)) entries)))))
          (setq previous id
                previous-cid (if blocked id
                               (pos-ledger--event-cid (pos-ledger--as-block bytes))))
          (push path files)))
      (unless files
        (pos-ledger--refuse 'empty "Empty ledger needs investigation: %s" folder))
      (list (sort entries (lambda (a b) (string< (car a) (car b))))
            previous number (nreverse files) root
            (seq-uniq (sort collections #'string<))
            (sort items #'string<)
            (seq-uniq (sort empty #'string<)))))))

(defun pos-ledger-event (add previous number &optional ledger-id)
  "Return (NAME . BYTES), event NUMBER enrolling ADD after PREVIOUS.
ADD is an alist of path and entry; PREVIOUS a hash or nil."
  (let ((bytes (pos-ledger-json
                `((schema . 1) (previous . ,(or previous :null)) (add . ,add)
                  ,@(when ledger-id `((ledger_id . ,ledger-id)))))))
    (cons (format "%08d-%s.json" number (pos-ledger--sha bytes)) bytes)))

;;;; Discovery and checkpoints

(defun pos-ledger--checked (root)
  "Return ROOT absolute, refusing a symlink in it or its ancestors.
The system aliases /tmp and /var are allowed."
  (let ((path (directory-file-name (expand-file-name root))))
    (named-let up ((part path))
      (when (and (file-symlink-p part) (not (member part '("/tmp" "/var"))))
        (pos-ledger--refuse 'link "Symlink: %s" part))
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
  "Return the outermost archives under ROOT, in path order."
  (let ((root (file-truename (pos-ledger--checked root))) found)
    (unless (file-directory-p root)
      (pos-ledger--refuse 'root "Root must be an existing directory"))
    (if (equal (file-name-nondirectory root) "archives")
        (list root)
      (named-let walk ((dir root))
        (dolist (name (pos-ledger--entries dir))
          (let ((path (expand-file-name name dir)))
            (when (and (file-directory-p path)
                       (not (string-match-p "\\`[._]" name)))
              (when (file-symlink-p path)
                (pos-ledger--refuse 'link "Symlink in discovery: %s" path))
              (if (equal name "archives")
                  (push path found)
                (walk path))))))
      (sort found #'pos-ledger--path<))))

(defun pos-ledger--anchor-homes (root)
  "Return the checkpoint directories covering ROOT, legacy and current."
  (let* ((root (file-truename (pos-ledger--checked root)))
         (base (if (equal (file-name-nondirectory root) "archives")
                   (file-name-directory root)
                 root)))
    (list (expand-file-name pos-ledger-anchors base)
          (expand-file-name (concat pos-ledger-integrity "/checkpoints") base))))

(defun pos-ledger--checkpoint-files (root archives)
  "Return the checkpoint files covering ROOT and ARCHIVES."
  (let (files)
    (dolist (folder (sort (delete-dups (mapcan #'pos-ledger--anchor-homes
                                                (cons root archives)))
                          #'pos-ledger--path<))
      (cond
       ((not (or (file-exists-p folder) (file-symlink-p folder))))
       ((or (file-symlink-p folder) (not (file-directory-p folder)))
        (pos-ledger--refuse 'checkpoint "Invalid checkpoint directory"))
       (t (setq files (append files (mapcar (lambda (n) (expand-file-name n folder))
                                            (pos-ledger--entries folder)))))))
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
goes by one or the other."
  (let ((folder (pos-ledger-folder archive)))
    (mapcan (lambda (file)
              (pos-ledger--regular file)
              (let ((bytes (pos-ledger--read file)))
                (list (pos-ledger--sha bytes) (pos-ledger--event-cid bytes))))
            (append
             (when (file-directory-p folder)
               (directory-files folder t "\\.json\\'"))
             (seq-filter #'pos-ledger--ledger-file-p
                         (directory-files-recursively archive "\\.json\\'" nil nil nil))))))

(defun pos-ledger--check-anchors (root archives)
  "Refuse unless every head a checkpoint for ROOT names is in ARCHIVES."
  (let ((archive-root (equal (file-name-nondirectory (directory-file-name root))
                             "archives"))
        required present)
    (dolist (file (pos-ledger--checkpoint-files root archives))
      (pos-ledger--regular file)
      (let* ((bytes (pos-ledger--read file))
             (value (and (equal (file-name-nondirectory file)
                                (concat (pos-ledger--sha bytes) ".json"))
                         (pos-ledger--parse bytes))))
        (unless value
          (pos-ledger--refuse 'checkpoint "Checkpoint hash failure: %s" file))
        (unless (and (listp value)
                     (equal (pos-ledger--keys value) '("coverage" "heads" "schema"))
                     (eql 1 (alist-get 'schema value))
                     (member (alist-get 'coverage value) '("archive" "tree"))
                     (vectorp (alist-get 'heads value))
                     (seq-every-p (lambda (head) (or (pos-ledger--hex-p head)
                                                     (pos-ledger--event-cid-p head)))
                                  (alist-get 'heads value)))
          (pos-ledger--refuse 'checkpoint "Invalid checkpoint"))
        (when (or (not archive-root) (equal (alist-get 'coverage value) "archive"))
          (setq required (append (alist-get 'heads value) required)))))
    (dolist (archive archives)
      (setq present (append (pos-ledger--ledger-hashes archive) present)))
    (let ((absent (sort (seq-uniq (seq-difference required present)) #'string<)))
      (when absent
        (pos-ledger--refuse
         'anchor "Missing anchored ledger (archive removed or history truncated): %s"
         (string-join absent ", "))))))

;;;; Check

(defun pos-ledger--cids (archive)
  "Return the CIDs of ARCHIVE and everything in it, as `pos-cid-tree'.
Nil if IPFS would shard a directory in it, or ARCHIVE is not yet made."
  (condition-case nil
      (and (file-exists-p archive) (pos-cid-tree archive))
    (pos-cid-sharding-unsupported nil)))

(defun pos-ledger-fold (entries &optional empty)
  "Return the CIDs the enrolled ENTRIES and the EMPTY directories give.
As `pos-cid-tree' gives them from disk, the root as \".\".  A hidden
entry is left out, as IPFS leaves it out.  Refuses `entry' when an
entry records no CID, as a legacy ledger's do."
  (pos-cid-inventory
   (delq nil
         (mapcar (lambda (pair)
                   (let ((path (pos-ledger--key (car pair)))
                         (entry (cdr pair)))
                     (unless (assq 'cid entry)
                       (pos-ledger--refuse 'entry "No CID enrolled for %s" path))
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
    (pos-ledger-fold (nth 0 history) (nth 7 history))))

(defun pos-ledger--with-cids (known actual cids)
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
         (let ((value (ignore-errors (pos-ledger--parse (pos-ledger--read manifest)))))
           (and (listp value) value
                (eql 1 (alist-get 'schema_version value))
                (assq 'entries value) (assq 'entrypoint value))))))

(defun pos-ledger--declared-p (directory)
  "Return non-nil if DIRECTORY declares itself a collection.
Its README.org has the declaration line, or it is a capsule."
  (let ((readme (expand-file-name "README.org" directory)))
    (or (and (file-regular-p readme)
             (member (encode-coding-string pos-ledger-declaration 'utf-8)
                     (split-string (pos-ledger--read readme) "\n")))
        (pos-ledger-capsule-p directory))))

(defun pos-ledger--undeclared (archive collections)
  "Return those of COLLECTIONS in ARCHIVE that no longer declare themselves."
  (seq-remove (lambda (path) (pos-ledger--declared-p (expand-file-name path archive)))
              collections))

(defun pos-ledger--differences (known actual)
  "Return the missing, changed and new paths between KNOWN and ACTUAL."
  (let (missing changed new)
    (dolist (pair known)
      (let ((now (assoc (car pair) actual)))
        (cond ((null now) (push (car pair) missing))
              ((not (equal (sort (copy-sequence (cdr pair))
                                 (lambda (a b) (string< (car a) (car b))))
                           (sort (copy-sequence (cdr now))
                                 (lambda (a b) (string< (car a) (car b))))))
               (push (car pair) changed)))))
    (dolist (pair actual)
      (unless (assoc (car pair) known) (push (car pair) new)))
    (list (sort missing #'string<) (sort changed #'string<) (sort new #'string<))))

(defun pos-ledger-check (root)
  "Return the check report on every archive under ROOT.
A list of alists, one an archive: archive, head, events, files,
writable, checkpoint_writable, missing, changed, new, root,
recorded_root, hidden and undeclared; the lists as vectors, so the
report is a JSON value."
  (let* ((archives (pos-ledger-roots root))
         (_ (pos-ledger--check-anchors root archives))
         (writable-checkpoints
          (seq-filter #'pos-ledger--writable-p
                      (pos-ledger--checkpoint-files root archives)))
         reports)
    (dolist (archive archives)
      (pcase-let* ((`(,known ,head ,events ,files ,recorded ,collections)
                    (pos-ledger-history archive))
                   (actual (pos-ledger-inventory archive))
                   (cids (pos-ledger--cids archive))
                   (`(,missing ,changed ,new)
                    (pos-ledger--differences known (pos-ledger--with-cids known actual cids)))
                   (writable
                    (sort (append
                           (seq-filter (lambda (name)
                                         (pos-ledger--writable-p
                                          (expand-file-name name archive)))
                                       (mapcar #'car actual))
                           (mapcar (lambda (f) (file-relative-name f archive))
                                   (seq-filter #'pos-ledger--writable-p files)))
                          #'string<)))
        (push `((archive . ,archive) (head . ,(or head :null)) (events . ,events)
                (files . ,(length actual)) (writable . ,(vconcat writable))
                (root . ,(or (cdr (assoc "." cids)) :null))
                (recorded_root . ,(or recorded :null))
                (hidden . ,(vconcat (pos-ledger--hidden actual)))
                (undeclared . ,(vconcat (pos-ledger--undeclared archive collections)))
                (checkpoint_writable . ,(vconcat (unless reports writable-checkpoints)))
                (missing . ,(vconcat missing)) (changed . ,(vconcat changed))
                (new . ,(vconcat new)))
              reports)))
    (nreverse reports)))

(provide 'pos-ledger)
;;; pos-ledger.el ends here
