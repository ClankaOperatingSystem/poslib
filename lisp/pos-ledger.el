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
;; - `pos-ledger-json': canonical JSON bytes of a value.
;; - `pos-ledger-inventory': what an archive holds.
;; - `pos-ledger-history': what its ledger enrolled.
;; - `pos-ledger-event': a new event's name and bytes.
;; - `pos-ledger-check': the report on every archive under a root.
;;
;; JSON values are Lisp values: objects alists, arrays vectors, strings,
;; integers and :null.  Refusals signal `pos-ledger-refused' with a kind
;; symbol and a message.

;;; Code:

(require 'subr-x)

(defconst pos-ledger-directory ".archive-integrity"
  "The ledger's directory at the top of an archive.")

(defconst pos-ledger-anchors ".archive-integrity-anchors"
  "The checkpoints' directory beside the root they cover.")

(define-error 'pos-ledger-refused "Archive integrity refused")

(defun pos-ledger--refuse (kind format &rest args)
  "Signal a refusal of KIND, with a message from FORMAT and ARGS."
  (signal 'pos-ledger-refused (list kind (apply #'format format args))))

;;;; Canonical JSON

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
   ((integerp value) (number-to-string value))
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
  (encode-coding-string (concat (pos-ledger--encode value) "\n") 'utf-8 t))

(defun pos-ledger--parse (bytes)
  "Return the JSON value in BYTES."
  (json-parse-string (decode-coding-string bytes 'utf-8) :object-type 'alist
                     :null-object :null :false-object :false))

(defun pos-ledger--sha (bytes)
  "Return the lower-case hex SHA-256 of BYTES."
  (secure-hash 'sha256 bytes))

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
  "Return ARCHIVE's inventory: an alist of relative path and entry."
  (let (result)
    (named-let walk ((dir archive) (rel nil))
      (dolist (name (pos-ledger--entries dir))
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

(defun pos-ledger--entry-p (entry)
  "Return non-nil if ENTRY is a well-formed ledger entry."
  (and (listp entry)
       (equal (pos-ledger--keys entry) '("mode" "sha256" "size"))
       (pos-ledger--hex-p (alist-get 'sha256 entry))
       (natnump (alist-get 'size entry))
       (natnump (alist-get 'mode entry))
       (zerop (logand (alist-get 'mode entry) #o222))))

(defun pos-ledger-history (archive)
  "Return ARCHIVE's ledger as (ENTRIES HEAD EVENTS FILES).
ENTRIES is an alist of path and entry; HEAD the last event's hash;
EVENTS their number; FILES the event files.  No ledger is (nil nil 0 nil)."
  (let ((folder (expand-file-name pos-ledger-directory archive))
        entries previous files ledger-id (number 0))
    (cond
     ((not (or (file-exists-p folder) (file-symlink-p folder)))
      (list nil nil 0 nil))
     ((or (file-symlink-p folder) (not (file-directory-p folder)))
      (pos-ledger--refuse 'ledger "Invalid ledger: %s" folder))
     (t
      (dolist (name (pos-ledger--entries folder))
        (let* ((path (expand-file-name name folder))
               (_ (pos-ledger--regular path))
               (bytes (pos-ledger--read path)))
          (setq number (1+ number))
          (unless (and (string-match "\\`\\([0-9]\\{8\\}\\)-\\([0-9a-f]\\{64\\}\\)\\.json\\'" name)
                       (= number (string-to-number (match-string 1 name)))
                       (equal (pos-ledger--sha bytes) (match-string 2 name)))
            (pos-ledger--refuse 'sequence "Ledger sequence/hash failure: %s" path))
          (let* ((event (pos-ledger--parse bytes))
                 (keys (and (listp event) (pos-ledger--keys event)))
                 (event-previous (alist-get 'previous event)))
            (unless (and (member keys '(("add" "previous" "schema")
                                        ("add" "ledger_id" "previous" "schema")))
                         (eql 1 (alist-get 'schema event))
                         (equal event-previous (or previous :null)))
              (pos-ledger--refuse 'chain "Ledger chain failure: %s" path))
            (let ((event-id (alist-get 'ledger_id event)))
              (cond
               (event-id
                (unless (and (pos-ledger--uuid-p event-id)
                             (or (null ledger-id) (equal event-id ledger-id)))
                  (pos-ledger--refuse 'identity "Ledger identity changed"))
                (setq ledger-id event-id))
               (ledger-id
                (pos-ledger--refuse 'identity "Ledger identity removed"))))
            (let ((add (alist-get 'add event)))
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
                  (unless (pos-ledger--entry-p (cdr pair))
                    (pos-ledger--refuse 'entry "Invalid ledger entry"))
                  (push (cons name (cdr pair)) entries)))))
          (setq previous (pos-ledger--sha bytes))
          (push path files)))
      (unless files
        (pos-ledger--refuse 'empty "Empty ledger needs investigation: %s" folder))
      (list (sort entries (lambda (a b) (string< (car a) (car b))))
            previous number (nreverse files))))))

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

(defun pos-ledger--anchor-home (root)
  "Return the checkpoint directory covering ROOT."
  (let ((root (file-truename (pos-ledger--checked root))))
    (expand-file-name pos-ledger-anchors
                      (if (equal (file-name-nondirectory root) "archives")
                          (file-name-directory root)
                        root))))

(defun pos-ledger--checkpoint-files (root archives)
  "Return the checkpoint files covering ROOT and ARCHIVES."
  (let (files)
    (dolist (folder (sort (delete-dups (mapcar #'pos-ledger--anchor-home
                                                (cons root archives)))
                          #'pos-ledger--path<))
      (cond
       ((not (or (file-exists-p folder) (file-symlink-p folder))))
       ((or (file-symlink-p folder) (not (file-directory-p folder)))
        (pos-ledger--refuse 'checkpoint "Invalid checkpoint directory"))
       (t (setq files (append files (mapcar (lambda (n) (expand-file-name n folder))
                                            (pos-ledger--entries folder)))))))
    files))

(defun pos-ledger--ledger-hashes (archive)
  "Return the hashes of every ledger event file under ARCHIVE, nested ones too."
  (mapcar (lambda (file) (pos-ledger--regular file) (pos-ledger--sha (pos-ledger--read file)))
          (seq-filter (lambda (file)
                        (equal (file-name-nondirectory (directory-file-name
                                                        (file-name-directory file)))
                               pos-ledger-directory))
                      (directory-files-recursively archive "\\.json\\'" nil nil nil))))

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
                     (seq-every-p #'pos-ledger--hex-p (alist-get 'heads value)))
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
writable, checkpoint_writable, missing, changed and new; the lists
as vectors, so the report is a JSON value."
  (let* ((archives (pos-ledger-roots root))
         (_ (pos-ledger--check-anchors root archives))
         (writable-checkpoints
          (seq-filter #'pos-ledger--writable-p
                      (pos-ledger--checkpoint-files root archives)))
         reports)
    (dolist (archive archives)
      (pcase-let* ((`(,known ,head ,events ,files) (pos-ledger-history archive))
                   (actual (pos-ledger-inventory archive))
                   (`(,missing ,changed ,new) (pos-ledger--differences known actual))
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
                (checkpoint_writable . ,(vconcat (unless reports writable-checkpoints)))
                (missing . ,(vconcat missing)) (changed . ,(vconcat changed))
                (new . ,(vconcat new)))
              reports)))
    (nreverse reports)))

(provide 'pos-ledger)
;;; pos-ledger.el ends here
