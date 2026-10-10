;;; pos-person.el --- Record people and who they are in a scope -*- lexical-binding: t; -*-

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

;; A person is recorded once, as a person-entity: an Org file with an
;; ID in person-entities/ of the root's configuration directory.  In
;; each scope where the person is referred to they have a
;; person-identity, with an ID of its own.  In a scope that has a
;; configuration it is a file in person-identities/ of the scope's
;; configuration directory.  In a project that has none it is a
;; heading under the top-level heading People of the project's file.
;;
;; An identity is named PERSON@SCOPE: the person's name with a hyphen
;; for each space, and the scope's path with a dot for each slash.  The
;; root's identity is named for the root's directory.
;;
;; The entity links down to each of its identities, by ID.  An
;; identity links to nothing, so a scope's records do not refer to
;; what is above the scope.
;;
;; - `pos-person-add': command.
;; - `pos-person-identity': command.
;; - `pos-person-identity-rename': command.
;; - `pos-person-batch': shell entry; add NAME, identity PERSON SCOPE,
;;   or rename ID NAME.

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-corpus)
(require 'pos-tree)

(defconst pos-person-entities-directory "person-entities"
  "The directory of person-entities, in the root's configuration directory.")

(defconst pos-person-identities-directory "person-identities"
  "The directory of person-identities, in a scope's configuration directory.")

(defcustom pos-person-heading "People"
  "The top-level heading of a project's file that holds its person-identities."
  :type 'string
  :group 'pos)

;;;; Names

(defun pos-person--one-line-p (text)
  "Return non-nil if TEXT is a string of one line that is not blank."
  (and (stringp text)
       (not (string-empty-p (string-trim text)))
       (not (string-match-p "[[:cntrl:]]" text))))

(defun pos-person--slug (name)
  "Return the file name of the person NAME, without its extension.
Lower case, with one hyphen for each run of other characters than
letters and digits."
  (string-trim (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase name))
               "-+" "-+"))

(defun pos-person-identity-name (name root scope)
  "Return the name of the identity that the person NAME has in SCOPE.
ROOT is the tree's root.  The name is NAME with a hyphen for each
run of spaces, an at sign, and SCOPE's path with a dot for each
slash; for the root, the name of ROOT's directory."
  (concat (replace-regexp-in-string "[ \t]+" "-" (string-trim name))
          "@"
          (if (equal (pos-scope-path scope) ".")
              (file-name-nondirectory (directory-file-name root))
            (string-replace "/" "." (pos-scope-path scope)))))

;;;; Files

(defun pos-person--config-directory (dir)
  "Return the configuration directory of the node at DIR, or nil.
Absolute, as a directory name."
  (when-let* ((file (pos-tree-config-file dir)))
    (file-name-as-directory
     (expand-file-name (file-name-directory file) dir))))

(defun pos-person--entities-directory (root)
  "Return the directory of the person-entities of the tree at ROOT.
Refuse a root that has no configuration."
  (let ((config (pos-person--config-directory root)))
    (unless config
      (user-error "The root has no configuration to keep people in"))
    (file-name-as-directory
     (expand-file-name pos-person-entities-directory config))))

(defun pos-person--record-text (id title text)
  "Return the text of a record's file with ID, TITLE and the paragraph TEXT."
  (concat ":PROPERTIES:\n:ID:       " id "\n:END:\n#+TITLE: " title "\n\n"
          text "\n"))

(defun pos-person--file-text (file)
  "Return the text of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun pos-person--file-keyword (file regexp)
  "Return what group 1 of REGEXP matches before FILE's first heading, or nil."
  (let ((text (pos-person--file-text file))
        (case-fold-search t))
    (when (string-match "^\\*+ " text)
      (setq text (substring text 0 (match-beginning 0))))
    (and (string-match regexp text) (match-string 1 text))))

(defun pos-person--file-id (file)
  "Return the ID that FILE has before its first heading, or nil."
  (pos-person--file-keyword file "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$"))

(defun pos-person--file-title (file)
  "Return the title of FILE, or nil."
  (pos-person--file-keyword file "^#\\+TITLE:[ \t]*\\(.*?\\)[ \t]*$"))

(defun pos-person--org-files (dir)
  "Return the Org files in DIR, absolute, sorted; none if DIR is not there."
  (and (file-directory-p dir)
       (seq-filter #'file-regular-p
                   (directory-files dir t "\\`[^.#].*\\.org\\'"))))

(defun pos-person--entity (root person)
  "Return the file of the person-entity PERSON in the tree at ROOT.
PERSON is the person's name, or the name of their file without its
extension.  Refuse a person with no entity."
  (let ((file (expand-file-name (concat (pos-person--slug person) ".org")
                                (pos-person--entities-directory root))))
    (unless (and (not (string-empty-p (pos-person--slug person)))
                 (file-regular-p file))
      (user-error "No person-entity for %s; add the person first" person))
    file))

(defun pos-person--buffer (root file)
  "Return a buffer visiting FILE, in the tree at ROOT, to write it.
Refuse symlinks, unsaved or stale buffers and foreign locks."
  (let ((name (file-relative-name file root)))
    (when (file-symlink-p file)
      (user-error "The file is a symlink: %s" name))
    (when (stringp (file-locked-p file))
      (user-error "%s is locked by another editor; save it there first" name))
    (with-current-buffer (pos-visit file)
      (when (buffer-modified-p)
        (user-error "Save the modified buffer of %s first" name))
      (unless (verify-visited-file-modtime (current-buffer))
        (user-error "%s changed on disk; revert its buffer first" name))
      (current-buffer))))

(defmacro pos-person--writing (buffer &rest body)
  "Evaluate BODY in BUFFER as one change, then save it.
BUFFER is one `pos-person--buffer' returned.  The file is locked
while BODY runs.  Return what BODY returns."
  (declare (indent 1) (debug (form body)))
  `(let ((enable-local-variables nil)
         (enable-local-eval nil)
         (make-backup-files nil)
         (auto-save-default nil)
         (create-lockfiles t)
         (vc-handled-backends nil)
         (require-final-newline t))
     (with-current-buffer ,buffer
       (unwind-protect
           (progn
             (lock-buffer)
             (atomic-change-group
               (prog1 (org-with-wide-buffer ,@body)
                 (save-buffer))))
         (unlock-buffer)))))

(defun pos-person--write-new (file text)
  "Write TEXT to FILE, which is not there, making its directory."
  (let ((make-backup-files nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region text nil file nil 'silent nil 'excl)))

;;;; Adding a person

(defun pos-person-add (root name)
  "Record the person NAME in the tree at ROOT, as a person-entity.
NAME is one line.  The entity is a file with a new Org ID in the
person-entities directory of ROOT's configuration directory, named
for NAME in lower case with hyphens.  Return the file's name.
Refuse a root with no configuration, a NAME with no letter or
digit, and a person recorded already."
  (unless (pos-person--one-line-p name)
    (user-error "A person's name is one nonempty line"))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (name (string-trim name))
         (slug (pos-person--slug name))
         (file (expand-file-name (concat slug ".org")
                                 (pos-person--entities-directory root))))
    (when (string-empty-p slug)
      (user-error "A person's name has a letter or a digit: %s" name))
    (when (file-exists-p file)
      (user-error "There is already a person-entity %s" slug))
    (pos-person--write-new
     file
     (pos-person--record-text
      (org-id-new) name
      (concat "A person-entity.  Each item of the list below is one of this"
              " person's\nperson-identities: the person in one scope.")))
    file))

;;;; An identity in a scope

(defun pos-person--linked-ids (entity)
  "Return the IDs that the person-entity file ENTITY links to."
  (let ((text (pos-person--file-text entity))
        (start 0)
        ids)
    (while (string-match "\\[\\[id:\\([^]]+\\)\\]" text start)
      (push (match-string 1 text) ids)
      (setq start (match-end 0)))
    (nreverse ids)))

(defun pos-person--link (root entity id name where)
  "Add to the file ENTITY a link to the identity ID, named NAME, in WHERE.
ROOT is the tree's root and WHERE a path relative to it.  Do nothing
if ENTITY links to ID already."
  (unless (member id (pos-person--linked-ids entity))
    (pos-person--writing (pos-person--buffer root entity)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (save-excursion (forward-line -1) (looking-at-p "- \\[\\[id:"))
        (unless (save-excursion (forward-line -1) (looking-at-p "[ \t]*$"))
          (insert "\n")))
      (insert (format "- [[id:%s][%s]], in =%s=\n" id name where)))))

(defun pos-person--project-file (root corpus scope)
  "Return the file of SCOPE, a project of CORPUS with no configuration.
ROOT is the tree's root.  A one-file project is its file; a project
that is a directory has project.org.  Refuse a project with neither."
  (let* ((path (pos-scope-path scope))
         (file (seq-find
                (lambda (file) (member file (pos-corpus-files corpus)))
                (list (expand-file-name (concat path ".org") root)
                      (expand-file-name "project.org"
                                        (expand-file-name path root))))))
    (unless file
      (user-error "The project %s has no file to hold its people" path))
    file))

(defun pos-person--heading-identity (buffer ids name)
  "Return (ID . MADE) of the identity in BUFFER, a project's file, making it.
The identity is the heading under `pos-person-heading' whose ID is
one of IDS, or failing that whose title is NAME.  With neither, a
heading NAME with a new ID is added there, the heading
`pos-person-heading' being added at the file's end when it is not
there, and MADE is non-nil."
  (with-current-buffer buffer
    (let ((people (format "^\\* %s[ \t]*$" (regexp-quote pos-person-heading)))
          (case-fold-search nil)
          found)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward people nil t)
         (let ((end (save-excursion (org-end-of-subtree t t) (point))))
           (while (and (not found) (re-search-forward "^\\*\\* " end t))
             (let ((id (org-entry-get (point) "ID")))
               (when (and id
                          (or (member id ids)
                              (equal name (org-get-heading t t t t))))
                 (setq found id)))))))
      (if found
          (cons found nil)
        (let ((id (org-id-new)))
          (pos-person--writing buffer
            (goto-char (point-min))
            (if (re-search-forward people nil t)
                (org-end-of-subtree t t)
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "* " pos-person-heading "\n"))
            (unless (bolp) (insert "\n"))
            (insert "** " name "\n:PROPERTIES:\n:ID:       " id "\n:END:\n"))
          (cons id t))))))

(defun pos-person-identity (root person within)
  "Give PERSON a person-identity in the scope WITHIN of the tree at ROOT.
PERSON names a person-entity, by the person's name or its file's.
WITHIN is a scope's path, \".\" for the root.  In a scope with a
configuration the identity is a file in the person-identities
directory of its configuration directory; in a project with none, a
heading under `pos-person-heading' in the project's file.  It has a
new Org ID and the name `pos-person-identity-name' gives, and the
entity gains a link to it.  Return (FILE ID NAME MADE), MADE nil
when the identity was there already; the entity's link is added
then if it lacks one.  Refuse a person with no entity, a path that
is not a scope's, and a scope another repository holds."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (entity (pos-person--entity root person))
         (corpus (pos-corpus root))
         (scope (seq-find (lambda (scope) (equal within (pos-scope-path scope)))
                          (pos-corpus-scopes corpus))))
    (unless scope
      (user-error "Not a scope of the tree: %s" within))
    (unless (pos-corpus-scope-writable-p corpus scope)
      (user-error "Not a scope this root may write: %s" within))
    (let* ((name (pos-person-identity-name
                  (or (pos-person--file-title entity) person) root scope))
           (config (and (pos-scope-config scope)
                        (pos-person--config-directory (pos-scope-dir scope))))
           (file (if config
                     (expand-file-name
                      (concat (file-name-base entity) ".org")
                      (expand-file-name pos-person-identities-directory config))
                   (pos-person--project-file root corpus scope)))
           (identity
            (cond
             ((not config)
              (unless (pos-corpus-writable-p corpus file)
                (user-error "Not a file this root may write: %s"
                            (file-relative-name file root)))
              (pos-person--heading-identity
               (pos-person--buffer root file)
               (pos-person--linked-ids entity) name))
             ((file-exists-p file)
              (cons (or (pos-person--file-id file)
                        (user-error "The identity has no ID: %s"
                                    (file-relative-name file root)))
                    nil))
             (t
              (let ((id (org-id-new)))
                (pos-person--write-new
                 file
                 (pos-person--record-text
                  id name
                  (concat "A person-identity.  An item of this scope that"
                          " waits on this person links\nhere.")))
                (cons id t))))))
      (pos-person--link root entity (car identity) name
                        (file-relative-name file root))
      (list file (car identity) name (cdr identity)))))

;;;; Renaming an identity

(defun pos-person--identity-files (corpus)
  "Return the person-identity files of CORPUS.
Those of each scope that has a configuration and that the root may
write."
  (seq-mapcat
   (lambda (scope)
     (when-let* (((pos-scope-config scope))
                 ((pos-corpus-scope-writable-p corpus scope))
                 (config (pos-person--config-directory (pos-scope-dir scope))))
       (pos-person--org-files
        (expand-file-name pos-person-identities-directory config))))
   (pos-corpus-scopes corpus)))

(defun pos-person--relabel (id name)
  "Give NAME to each link to ID in the current buffer.
Return how many changed."
  (let ((link (format "\\[\\[id:%s\\]\\[\\([^]]*\\)\\]\\]" (regexp-quote id)))
        (count 0))
    (goto-char (point-min))
    (while (re-search-forward link nil t)
      (unless (equal name (match-string 1))
        (replace-match name t t nil 1)
        (setq count (1+ count))))
    count))

(defun pos-person-identity-rename (root id name)
  "Give the person-identity ID in the tree at ROOT the name NAME.
NAME is one line without square brackets.  The identity's title, or
its heading, becomes NAME, and so does the text of each link to it
by ID in the files the tree reads, the person-entities and the other
identities.  The ID stays.  Return (FILE . CHANGED): the identity's
file, and (FILE . COUNT) for each file whose links were renamed.
Refuse an ID no person-entity links to or no identity has, and a
file this root may not write, a symlink, an unsaved or stale buffer
or a foreign lock, before anything is written."
  (unless (and (pos-person--one-line-p name)
               (not (string-match-p "[][]" name)))
    (user-error "A name is one nonempty line without square brackets"))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (name (string-trim name))
         (corpus (pos-corpus root))
         (entities (pos-person--org-files (pos-person--entities-directory root)))
         (identities (pos-person--identity-files corpus))
         (property (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id)))
         (written (seq-filter (lambda (file) (pos-corpus-writable-p corpus file))
                              (pos-corpus-files corpus)))
         (holder (or (seq-find (lambda (file) (equal id (pos-person--file-id file)))
                               identities)
                     (seq-find (lambda (file)
                                 (string-match-p property
                                                 (pos-person--file-text file)))
                               written)))
         (mentions (seq-filter
                    (lambda (file)
                      (string-match-p (regexp-quote (concat "[[id:" id "]"))
                                      (pos-person--file-text file)))
                    (seq-uniq (append entities identities written))))
         changed)
    (unless (seq-some (lambda (entity)
                        (member id (pos-person--linked-ids entity)))
                      entities)
      (user-error "No person-entity links to the ID %s" id))
    (unless holder
      (user-error "No identity this root may write has the ID %s" id))
    ;; Every refusal comes before the first write.
    (let ((buffers (mapcar (lambda (file) (cons file (pos-person--buffer root file)))
                           (seq-uniq (cons holder mentions)))))
      (pos-person--writing (cdr (assoc holder buffers))
        (goto-char (point-min))
        (re-search-forward property)
        (if (org-before-first-heading-p)
            (progn
              (goto-char (point-min))
              (if (let ((case-fold-search t))
                    (re-search-forward "^#\\+TITLE:.*$" nil t))
                  (replace-match (concat "#+TITLE: " name) t t)
                (user-error "The identity's file has no title")))
          (org-back-to-heading t)
          (org-edit-headline name)))
      (dolist (file mentions)
        (let ((count (pos-person--writing (cdr (assoc file buffers))
                       (pos-person--relabel id name))))
          (when (> count 0)
            (push (cons file count) changed)))))
    (cons holder (nreverse changed))))

;;;; The shell entry

(defun pos-person-batch ()
  "Run a person command from `command-line-args-left' on `pos-directory'.
add NAME, as `pos-person-add' takes it, prints the file made,
relative to the root.  identity PERSON SCOPE, as
`pos-person-identity' takes them, prints \"FILE: NAME ID\", and
\"made\" or \"there already\" after it.  rename ID NAME, as
`pos-person-identity-rename' takes them, prints the identity's
file, then \"Links renamed: FILE: COUNT\" for each file changed.
Exit 2 on any other arguments."
  (let ((root (file-name-as-directory (expand-file-name pos-directory)))
        (arguments (prog1 command-line-args-left
                     (setq command-line-args-left nil))))
    (pcase arguments
      (`("add" ,name)
       (princ (format "%s\n"
                      (file-relative-name (pos-person-add root name) root))))
      (`("identity" ,person ,scope)
       (pcase-let ((`(,file ,id ,name ,made)
                    (pos-person-identity root person scope)))
         (princ (format "%s: %s %s, %s\n" (file-relative-name file root)
                        name id (if made "made" "there already")))))
      (`("rename" ,id ,name)
       (pcase-let ((`(,file . ,changed)
                    (pos-person-identity-rename root id name)))
         (princ (format "%s\n" (file-relative-name file root)))
         (pcase-dolist (`(,renamed . ,count) changed)
           (princ (format "Links renamed: %s: %d\n"
                          (file-relative-name renamed root) count)))))
      (_ (message "%s\n%s\n%s"
                  "Usage: add NAME"
                  "       identity PERSON SCOPE"
                  "       rename ID NAME")
         (kill-emacs 2)))))

(provide 'pos-person)
;;; pos-person.el ends here
