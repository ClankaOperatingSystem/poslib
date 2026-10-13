;;; pos-node.el --- What a node declares, read and checked -*- lexical-binding: t; -*-

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

;; A node of a tree, a repository or a directory, declares in its
;; configuration what lies beneath it, as doc/pos-directory.txt
;; specifies: where its projects and methodologies are, the children
;; and worktrees mounted beneath it, where its archives are kept, the
;; server it is bound to, and the directories a walk does not enter.
;; A methodology declares in methodology.yaml the kinds of canon it
;; maintains and the checks it supplies, as doc/pos-methodology.txt
;; specifies.  This module reads both, checked, and says where a
;; node's configuration is.
;;
;; A reader warns of a key it does not know and reads the rest, so a
;; file written for a newer reader still works with this one, with
;; the newer key's default.  A file the documents do not allow is
;; refused, with the kind of refusal they name.  Which directories a
;; walk does not enter is the node's to declare, under exclude, and is
;; inherited by what lies beneath it until a node declares its own.
;;
;; - `pos-node-read-config': a config.yaml's text, checked, with its
;;   warnings.
;; - `pos-node-read-methodology', `pos-node-read-methodology-file': a
;;   methodology.yaml's text, or the file at a directory, checked.
;; - `pos-node-config-file', `pos-node-one-config': the configuration
;;   file a node has, by its directory or among paths found.
;; - `pos-node-exclusions', `pos-node-unwalked-p',
;;   `pos-node-default-exclude': what a walk of a node does not enter.
;; - `pos-node-methodologies': the methodologies a configuration declares.
;; - `pos-node-refuse', `pos-node-refused': a refusal, and its signal.
;; - `pos-node-uuid-regexp': a UUID as a ledger's ID is written.

;;; Code:

(require 'cl-lib)
(require 'pos-path)
(require 'seq)
(require 'subr-x)
(require 'yaml)

(define-error 'pos-node-refused "Configuration refused")

(defun pos-node-refuse (kind format &rest args)
  "Signal a refusal of KIND, with a message from FORMAT and ARGS."
  (signal 'pos-node-refused (list kind (apply #'format format args))))

;;;; The file

(defconst pos-node-uuid-regexp
  (concat "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-"
          "[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'")
  "Matches a UUID in canonical form, when letters' case is not folded.")

(defun pos-node--location (value what)
  "Return VALUE, the path of a WHAT, less a final slash; nil for nil.
Refuse a path that does not stay beneath its node."
  (when value
    (let ((path (if (and (> (length value) 1) (string-suffix-p "/" value))
                    (substring value 0 -1)
                  value)))
      (unless (pos-path-safe-p path)
        (pos-node-refuse 'bad-path "Not a path for %s: %s" what value))
      path)))

(defun pos-node--mapping-p (value)
  "Return non-nil if VALUE, as parsed from YAML, is a mapping."
  (and (listp value)
       (seq-every-p (lambda (pair) (and (consp pair) (stringp (car pair))))
                    value)))

(defun pos-node--typed (value type)
  "Return a list of VALUE read as TYPE, or nil if it is not of TYPE.
VALUE is as parsed from YAML, every scalar the text that was written.
TYPE is one of string, integer, sequence and mapping.  A string is
any text but the empty one and the two words for null; an integer is
a whole number in digits, returned as a number."
  (pcase type
    ('string (and (stringp value)
                  (not (member value '("" "null" "~")))
                  (list value)))
    ('integer (and (stringp value)
                   (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" value)
                   (list (string-to-number value))))
    ('sequence (and (vectorp value) (list value)))
    ('mapping (and (pos-node--mapping-p value) (list value)))))

(cl-defstruct (pos-node--reading (:constructor pos-node--reading-make)
                                 (:copier nil))
  "The reading of one file.
WARNINGS are those of the file so far, newest first."
  warnings)

(defun pos-node--mapping (reading value what keys &optional where)
  "Return the mapping VALUE as an alist of symbols, checked against KEYS.
WHAT names the mapping in a refusal.  KEYS is a list of (NAME TYPE
REQUIRED): a value not of its TYPE and a REQUIRED key that is absent
are each refused.  A key not among them is a warning, added to
READING, which names it after WHERE, the mapping's place in the file,
and the key is left unread: a file written for a newer reader is read
with that key's default."
  (unless (pos-node--mapping-p value)
    (pos-node-refuse 'not-a-mapping "%s is not a mapping" what))
  (dolist (pair value)
    (unless (assoc (car pair) keys)
      (push (format "unknown-key: %s%s" (if where (concat where ".") "") (car pair))
            (pos-node--reading-warnings reading))))
  (mapcan (lambda (key)
            (pcase-let* ((`(,name ,type ,required) key)
                         (pair (assoc name value))
                         (typed (and pair (pos-node--typed (cdr pair) type))))
              (cond
               (typed (list (cons (intern name) (car typed))))
               (pair (pos-node-refuse 'wrong-type "%s: %s is not a %s"
                                      what name type))
               (required (pos-node-refuse 'missing-key "%s lacks %s" what name)))))
          keys))

(defun pos-node--distinct (paths what)
  "Refuse if PATHS, each naming a WHAT, hold one path twice."
  (let ((seen nil))
    (dolist (path paths)
      (when (member path seen)
        (pos-node-refuse 'bad-path "%s is named twice: %s" what path))
      (push path seen))))

(defun pos-node--children (reading entries)
  "Return the children in ENTRIES, a sequence, checked, defaults filled in.
Warnings are added to READING."
  (let ((children
         (seq-map-indexed
          (lambda (entry index)
            (let-alist (pos-node--mapping reading
                        entry "A child"
                        '(("path" string t) ("remote" string nil)
                          ("branch" string nil))
                        (format "children[%d]" index))
              (unless (pos-path-safe-p .path)
                (pos-node-refuse 'bad-path "Not a child's path: %s" .path))
              ;; A branch is a repository's: a child with no remote is
              ;; a directory of this one.
              (when (and (not .remote) .branch)
                (pos-node-refuse 'bad-value
                                 "A child with no remote has no branch: %s"
                                 .path))
              `((path . ,.path) (remote . ,(or .remote :null))
                (branch . ,(if .remote (or .branch "master") :null)))))
          entries)))
    (dolist (a children)
      (dolist (b children)
        (when (and (not (eq a b))
                   (pos-path-within-p (alist-get 'path b) (alist-get 'path a)))
          (pos-node-refuse 'bad-path "A child's path is another's or beneath it: %s"
                           (alist-get 'path b)))))
    children))

(defun pos-node--check-methodologies (at children)
  "Refuse one of CHILDREN beneath AT, the methodologies path, not a methodology.
A methodology's path is AT and one more part, its name, which is not
clankos."
  (dolist (child children)
    (let ((path (alist-get 'path child)))
      (when (pos-path-within-p path at)
        (let ((name (and (> (length path) (length at))
                         (substring path (1+ (length at))))))
          (when (or (not name) (string-match-p "/" name))
            (pos-node-refuse 'bad-path "A methodology is one part beneath %s: %s"
                             at path))
          (when (equal name "clankos")
            (pos-node-refuse 'bad-value "The name clankos is reserved: %s" path)))))))

(defun pos-node-methodologies (config)
  "Return the methodologies CONFIG declares, each (NAME . PATH), by name.
A methodology is a child directly beneath the path CONFIG says its
methodologies belong at; its NAME is the last part of its PATH, which
is relative to the node.  None for a node that says no such path."
  (let ((at (alist-get 'methodologies config)))
    (when (stringp at)
      (seq-sort-by #'car #'string<
                   (delq nil
                         (mapcar (lambda (child)
                                   (let ((path (alist-get 'path child)))
                                     (when (pos-path-within-p path at)
                                       (cons (substring path (1+ (length at))) path))))
                                 (append (alist-get 'children config) nil)))))))

(defun pos-node--own-scope (path children what)
  "Refuse if PATH, a WHAT's scope, is one of CHILDREN or beneath one."
  (dolist (child children)
    (when (pos-path-within-p path (alist-get 'path child))
      (pos-node-refuse 'bad-path "%s is in a child, which declares its own: %s"
                       what path))))

(defun pos-node--worktrees (reading entries children)
  "Return the worktrees in ENTRIES, a sequence, checked, defaults filled in.
CHILDREN are the repository's children, already checked.  Warnings are
added to READING."
  (let ((worktrees
         (seq-map-indexed
          (lambda (entry index)
            (let-alist (pos-node--mapping reading
                        entry "A worktree"
                        '(("path" string t) ("of" string nil)
                          ("remote" string nil) ("branch" string nil))
                        (format "worktrees[%d]" index))
              (unless (and (pos-path-safe-p .path)
                           (string-match "\\`\\(?:\\(.+\\)/\\)?_worktrees/[^/]+\\'"
                                         .path))
                (pos-node-refuse 'bad-path "Not a worktree's path: %s" .path))
              (when-let* ((scope (match-string 1 .path)))
                (pos-node--own-scope scope children "A worktree"))
              (unless (eq (null .of) (not (null .remote)))
                (pos-node-refuse 'bad-value "A worktree has one of of and remote: %s"
                                 .path))
              (cond
               (.remote `((path . ,.path) (remote . ,.remote)
                          (branch . ,(or .branch "master"))))
               ((not (seq-some (lambda (child)
                                 (and (equal (alist-get 'path child) .of)
                                      (stringp (alist-get 'remote child))))
                               children))
                (pos-node-refuse 'bad-value
                                 "A worktree is of no declared child with a remote: %s"
                                 .of))
               ((not .branch)
                (pos-node-refuse 'missing-key "A worktree of a child lacks branch"))
               (t `((path . ,.path) (of . ,.of) (branch . ,.branch))))))
          entries)))
    (pos-node--distinct (mapcar (lambda (w) (alist-get 'path w)) worktrees)
                        "A worktree")
    worktrees))

(defun pos-node--archives (reading entries children)
  "Return the archive settings in ENTRIES, a sequence, checked.
CHILDREN are the repository's children, already checked.  Warnings are
added to READING."
  (let ((archives
         (seq-map-indexed
          (lambda (entry index)
            (let-alist (pos-node--mapping reading
                        entry "An archive"
                        '(("scope" string t) ("kept" string t)
                          ("ledger" string nil) ("url" string nil)
                          ("sweep" string nil) ("path" string nil))
                        (format "archives[%d]" index))
              (unless (equal .scope ".")
                (unless (pos-path-safe-p .scope)
                  (pos-node-refuse 'bad-path "Not a scope's path: %s" .scope))
                (pos-node--own-scope .scope children "An archive"))
              (unless (member .kept '("committed" "uncommitted" "remote"))
                (pos-node-refuse 'bad-value "An archive is not kept %s" .kept))
              (when (and .ledger
                         (not (let ((case-fold-search nil))
                                (string-match-p pos-node-uuid-regexp .ledger))))
                (pos-node-refuse 'bad-value "Not a ledger's id: %s" .ledger))
              (when (and .sweep (not (member .sweep '("weekly" "sealed"))))
                (pos-node-refuse 'bad-value "Done items are swept weekly or sealed, not %s"
                                 .sweep))
              (when (and .path (not (equal .sweep "weekly")))
                (pos-node-refuse 'bad-value "Only a weekly sweep has a path: %s" .scope))
              (let ((ledger (and .ledger `((ledger . ,.ledger))))
                    (sweep (append (and .sweep `((sweep . ,.sweep)))
                                   (and .path `((path . ,(pos-node--location .path "path")))))))
                (cond
                 ((not (equal .kept "remote"))
                  (when .url
                    (pos-node-refuse 'bad-value "Only a remote archive has a url: %s"
                                     .scope))
                  `((scope . ,.scope) (kept . ,.kept) ,@ledger ,@sweep))
                 (.url `((scope . ,.scope) (kept . ,.kept) ,@ledger (url . ,.url) ,@sweep))
                 (t (pos-node-refuse 'missing-key "A remote archive lacks url"))))))
          entries)))
    (pos-node--distinct (mapcar (lambda (a) (alist-get 'scope a)) archives)
                        "An archive's scope")
    (let ((seen nil))
      (dolist (archive archives)
        (when-let* ((ledger (alist-get 'ledger archive)))
          (when (member ledger seen)
            (pos-node-refuse 'bad-value "A ledger is named twice: %s" ledger))
          (push ledger seen))))
    archives))

(defconst pos-node-default-exclude '("archives" "attic" "node_modules" "_*" ".*")
  "The directories a walk does not enter when no node declares exclude.
Archives and attics, Node's modules, and names beginning with an
underscore or a dot, as doc/pos-directory.txt has it.")

(defun pos-node--exclusions (value)
  "Return VALUE, a configuration's exclude sequence, checked, as a vector.
Each entry is a directory's name or a glob over one, with * for any
text, or a path beneath the node when it holds a slash.  A path's
final slash is dropped unless the path is one name, which keeps it:
without it the entry would read as a name.  Nil for nil."
  (when value
    (vconcat
     (seq-map (lambda (entry)
                (unless (pos-node--typed entry 'string)
                  (pos-node-refuse 'wrong-type "exclude: an entry is not a string"))
                (if (string-match-p "/" entry)
                    (let ((path (pos-node--location entry "exclude")))
                      (if (string-match-p "/" path) path (concat path "/")))
                  entry))
              value))))

(defun pos-node-exclusions (config)
  "Return the exclusions in force at the node whose configuration is CONFIG.
A list: its exclude entries, or `pos-node-default-exclude' when it
declares none, or when CONFIG is nil."
  (let ((declared (alist-get 'exclude config)))
    (if (and declared (not (eq declared :null)))
        (append declared nil)
      pos-node-default-exclude)))

(defun pos-node-unwalked-p (path exclusions)
  "Return non-nil if PATH is a directory EXCLUSIONS keep a walk out of.
PATH is relative to the node whose EXCLUSIONS these are, a list as
`pos-node-exclusions' gives.  An entry with a slash names a path and
excludes it and what lies beneath it; any other names a directory,
with * for any text, wherever it lies beneath the node.  A name is
matched letter for letter: case counts, and no character but * stands
for another."
  (let ((name (file-name-nondirectory path))
        (case-fold-search nil))
    (seq-some (lambda (entry)
                (if (string-match-p "/" entry)
                    (pos-path-within-p path (string-remove-suffix "/" entry))
                  (string-match-p
                   (concat "\\`"
                           (mapconcat #'regexp-quote (split-string entry "\\*") ".*")
                           "\\'")
                   name)))
              exclusions)))

(defun pos-node--parsed (text what key version)
  "Return TEXT, a WHAT's YAML, parsed and checked to be a mapping at VERSION.
KEY is the key that names its version.  Refuse `not-yaml', then
`not-a-mapping', then `unknown-version'."
  (let ((parsed (condition-case nil
                    (yaml-parse-string text :object-type 'alist
                                       :object-key-type 'string
                                       :sequence-type 'array
                                       :string-values t)
                  (error (pos-node-refuse 'not-yaml "Not readable as YAML")))))
    (unless (pos-node--mapping-p parsed)
      (pos-node-refuse 'not-a-mapping "%s is not a mapping" what))
    (let ((found (car (pos-node--typed (cdr (assoc key parsed)) 'integer))))
      (when (and found (/= found version))
        (pos-node-refuse 'unknown-version "Not a version this reader knows: %s"
                         found)))
    parsed))

(defun pos-node-read-config (text)
  "Return the configuration in TEXT, a config.yaml's, checked.
An alist of projects, methodologies, image, bin, exclude, children,
worktrees, archives, server and warnings, with defaults filled in:
sequences are vectors and an absent value is :null.  warnings is a
vector of strings, one for each key this reader does not know, which
it leaves unread.  The file does not say what its node is: where the
node lies does, as doc/pos-directory.txt section 2 has it.  Every
scalar is read as the text written, and the keys' types decide what
it is.  Signal `pos-node-refused', with the refusal's kind and a
message, for a file doc/pos-directory.txt does not allow."
  (let ((reading (pos-node--reading-make))
        (parsed (pos-node--parsed text "The file" "pos" 2)))
    (let* ((top (pos-node--mapping reading
                 parsed "The file"
                 '(("pos" integer t) ("projects" string nil)
                   ("methodologies" string nil) ("image" string nil)
                   ("bin" string nil) ("exclude" sequence nil) ("children" sequence nil)
                   ("worktrees" sequence nil) ("archives" sequence nil)
                   ("server" mapping nil))))
           (projects (pos-node--location (alist-get 'projects top) "projects"))
           (methodologies (pos-node--location (alist-get 'methodologies top)
                                              "methodologies"))
           (image (alist-get 'image top))
           (bin (pos-node--location (alist-get 'bin top) "bin"))
           (children (pos-node--children reading (alist-get 'children top)))
           (server (assq 'server top)))
      (when (and projects methodologies
                 (or (pos-path-within-p projects methodologies)
                     (pos-path-within-p methodologies projects)))
        (pos-node-refuse 'bad-path
                         "Projects and methodologies belong at separate paths: %s"
                         methodologies))
      (when methodologies
        (pos-node--check-methodologies methodologies children))
      ;; A tool with no YAML reader finds the image by its line.
      (when (and image
                 (not (let ((case-fold-search nil))
                        (and (string-match-p "\\`[^ \t\n\"'#]+\\'" image)
                             (string-match-p
                              (concat "^image: " (regexp-quote image) "$")
                              text)))))
        (pos-node-refuse 'bad-value
                         "The image is not on one line, as image: NAME, unquoted"))
      `((projects . ,(or projects :null))
        (methodologies . ,(or methodologies :null))
        (image . ,(or image :null))
        (bin . ,(or bin :null))
        (exclude . ,(or (pos-node--exclusions (alist-get 'exclude top)) :null))
        (children . ,(vconcat children))
        (worktrees . ,(vconcat (pos-node--worktrees reading (alist-get 'worktrees top)
                                                    children)))
        (archives . ,(vconcat (pos-node--archives reading (alist-get 'archives top)
                                                  children)))
        (server . ,(if server
                       (pos-node--mapping reading
                        (cdr (assoc "server" parsed)) "The server"
                        '(("name" string t) ("mcp" string t)) "server")
                     :null))
        (warnings . ,(vconcat (reverse (pos-node--reading-warnings reading))))))))

;;;; The declaration

(defconst pos-node-methodology-file "methodology.yaml"
  "The declaration at a methodology's root, doc/pos-methodology.txt.")

(defun pos-node-url-p (value)
  "Return non-nil if VALUE is a URL: a scheme, then ://."
  (string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*://" value))

(defun pos-node--declared-path (value what)
  "Return VALUE, where a WHAT of a kind is, as written.
A path beneath the project, refused if it does not stay beneath it,
with a final slash allowed, which marks a directory of instances; or
a URL, for a kind kept outside the repository, which is taken as
written."
  (unless (or (pos-node-url-p value)
              (pos-path-safe-p (if (and (> (length value) 1) (string-suffix-p "/" value))
                                   (substring value 0 -1)
                                 value)))
    (pos-node-refuse 'bad-path "Not a path or a URL for %s: %s" what value))
  value)

(defun pos-node--kinds (reading entries)
  "Return the kinds of canon in ENTRIES, a sequence, checked, defaults filled in.
Warnings are added to READING."
  (let ((names nil))
    (seq-map-indexed
     (lambda (entry index)
       (let-alist (pos-node--mapping reading
                   entry "A kind"
                   '(("kind" string t) ("at" string t) ("format" string nil)
                     ("derived" string nil) ("entrance" string nil))
                   (format "canon[%d]" index))
         (when (member .kind names)
           (pos-node-refuse 'bad-value "A kind is declared twice: %s" .kind))
         (push .kind names)
         (unless (member .derived '(nil "true" "false"))
           (pos-node-refuse 'bad-value "derived is true or false: %s" .kind))
         `((kind . ,.kind)
           (at . ,(pos-node--declared-path .at "at"))
           (format . ,(or .format :null))
           (derived . ,(if (equal .derived "true") t :false))
           (entrance . ,(if .entrance
                            (pos-node--declared-path .entrance "entrance")
                          .at)))))
     entries)))

(defun pos-node--checks (names)
  "Return NAMES, a sequence of check names, checked."
  (seq-map (lambda (name)
             (let ((typed (pos-node--typed name 'string)))
               (unless typed
                 (pos-node-refuse 'wrong-type "A check is not a string"))
               (when (string-match-p "/" (car typed))
                 (pos-node-refuse 'bad-value
                                  "A check is a name in bin/, not a path: %s"
                                  (car typed)))
               (car typed)))
           names))

(defun pos-node-read-methodology (text)
  "Return the declaration in TEXT, a methodology.yaml's, checked.
An alist of methodology, the version; canon, a vector of kinds, each
an alist of kind, at, format, derived and entrance with defaults
filled in; checks, a vector of names; and warnings, a vector of
strings, one for each key this reader does not know.  Signal
`pos-node-refused' for a file doc/pos-methodology.txt does not allow."
  (let ((reading (pos-node--reading-make))
        (parsed (pos-node--parsed text "The declaration" "methodology" 1)))
    (let ((top (pos-node--mapping reading
                parsed "The declaration"
                '(("methodology" integer t) ("canon" sequence nil)
                  ("checks" sequence nil)))))
      `((methodology . 1)
        (canon . ,(vconcat (pos-node--kinds reading (alist-get 'canon top))))
        (checks . ,(vconcat (pos-node--checks (alist-get 'checks top))))
        (warnings . ,(vconcat (reverse (pos-node--reading-warnings reading))))))))

(defun pos-node-read-methodology-file (dir)
  "Return the declaration of the methodology at DIR, or nil if it has none.
Signal `pos-node-refused' for one this reader does not allow."
  (let ((file (expand-file-name pos-node-methodology-file dir)))
    (when (file-regular-p file)
      (pos-node-read-methodology
       (with-temp-buffer (insert-file-contents file) (buffer-string))))))

;;;; Where a node's configuration is

(defun pos-node-one-config (files)
  "Return the one of FILES, the configuration paths found in a node, or nil.
Signal `pos-node-refused' if there are two."
  (when (cdr files)
    (pos-node-refuse 'two-configurations "Two configurations: %s"
                     (string-join files ", ")))
  (car files))

(defun pos-node-config-file (dir)
  "Return the configuration file of the node at DIR, relative, or nil.
Signal `pos-node-refused' if it has two."
  (pos-node-one-config
   (pos-path-config-files dir)))

(provide 'pos-node)
;;; pos-node.el ends here
