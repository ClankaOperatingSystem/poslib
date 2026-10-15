;;; pos-lock.el --- A node's skills-lock.json, compared with its sources -*- lexical-binding: t; -*-

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

;; A node declares the sources of its skills under methodologies, and
;; the image's install command installs them with the skills CLI,
;; which records what it installed in skills-lock.json beside them.
;; This module compares that file with the configuration, with no
;; network, as section 6 of doc/pos-directory.txt has it: lock-differs
;; for what the configuration declares and the file does not hold, and
;; lock-undeclared for what the file holds and the configuration does
;; not declare.  At a repository's root the image's skills are one
;; source more, which no configuration names.  Each entry's
;; computedHash is compared with the skill's directory too, so a skill
;; edited where it is installed is found.
;;
;; - `pos-lock-file': the name of the file, in the node's directory.
;; - `pos-lock-hash': a skill directory's hash, as the skills CLI
;;   computes it.
;; - `pos-lock-image': the image's skills, as a source, at a
;;   repository's root.
;; - `pos-lock-differences': the differences at a node.
;; - `pos-lock-installed': the skills at a node installed from its
;;   sources.

;;; Code:

(require 'cl-lib)
(require 'pos-node)
(require 'seq)
(require 'subr-x)

(defconst pos-lock-file "skills-lock.json"
  "The skills CLI's record of what it installed, in the node's directory.")

(defun pos-lock--entries (file)
  "Return the skills FILE records, each (NAME . ENTRY), by name.
ENTRY is an alist of the entry's keys, as symbols.  Return :invalid
for a file that is not a JSON object whose skills maps each name to
an object with a string source."
  (let ((parsed (condition-case nil
                    (with-temp-buffer
                      (insert-file-contents file)
                      (json-parse-buffer :object-type 'alist :null-object nil))
                  (error :invalid))))
    (let ((skills (and (consp parsed) (assq 'skills parsed))))
      (if (and skills
               (listp (cdr skills))
               (seq-every-p (lambda (pair)
                              (and (consp pair) (symbolp (car pair))
                                   (consp (cdr pair))
                                   (stringp (alist-get 'source (cdr pair)))))
                            (cdr skills)))
          (seq-sort-by #'car #'string<
                       (mapcar (lambda (pair) (cons (symbol-name (car pair)) (cdr pair)))
                               (cdr skills)))
        :invalid))))

;;;; A skill directory's hash

(defconst pos-lock--order
  " _-,;:!?.'\"()[]{}@*/\\&#%`^+<=>|~$0123456789abcdefghijklmnopqrstuvwxyz"
  "The ASCII characters in the order the Unicode collation gives them.
Each upper-case letter sorts with its lower-case one.")

(defun pos-lock--collation-key (string)
  "Return the sort key of STRING in the Unicode collation, as a list.
The letters of the key, then their case: a lower-case letter sorts
before its upper-case one only where the strings are otherwise equal.
A character outside ASCII sorts after them all, by its code."
  (let ((letters nil) (cases nil))
    (dolist (char (string-to-list string))
      (let ((at (seq-position pos-lock--order (downcase char))))
        (push (if (and at (< char 128)) at (+ 128 char)) letters)
        (push (if (and (>= char ?A) (<= char ?Z)) 1 0) cases)))
    (append (nreverse letters) (list -1) (nreverse cases))))

(defun pos-lock--key< (a b)
  "Return non-nil if the key A, a list of numbers, precedes the key B."
  (while (and a b (= (car a) (car b)))
    (setq a (cdr a) b (cdr b)))
  (if (and a b) (< (car a) (car b)) (and b t)))

(defun pos-lock--files (dir &optional prefix)
  "Return the files beneath DIR, as paths relative to it.
PREFIX is the directory being looked in, with its final slash.  A
symbolic link is no file, and a directory named .git or node_modules
is not looked in."
  (mapcan (lambda (name)
            (let ((path (concat prefix name))
                  (full (expand-file-name (concat prefix name) dir)))
              (cond
               ((file-symlink-p full) nil)
               ((file-directory-p full)
                (unless (member name '(".git" "node_modules"))
                  (pos-lock--files dir (concat path "/"))))
               ((file-regular-p full) (list path)))))
          (directory-files (expand-file-name (or prefix "") dir) nil
                           directory-files-no-dot-files-regexp)))

(defun pos-lock-hash (dir)
  "Return the hash of the skill directory DIR, as the skills CLI computes it.
That is computedHash of a lock entry: the lower-case hex SHA-256 of
each file's path relative to DIR, in UTF-8, followed by its bytes, the
files in the order of their paths in the Unicode collation."
  (let ((files (seq-sort-by #'pos-lock--collation-key #'pos-lock--key<
                            (pos-lock--files dir))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dolist (path files)
        (insert (encode-coding-string path 'utf-8 t))
        (goto-char (point-max))
        (insert-file-contents-literally (expand-file-name path dir))
        (goto-char (point-max)))
      (secure-hash 'sha256 (current-buffer)))))

;;;; The comparison

(defconst pos-lock-slots '("capture" "start-up" "wrap-up")
  "The names of the slots, as doc/pos-slots.txt has them.")

(defun pos-lock--line (dir name)
  "Return the first line of the file NAME in DIR, trimmed, or nil."
  (let ((file (expand-file-name name dir)))
    (when (file-regular-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((line (string-trim (buffer-substring (point-min) (line-end-position)))))
          (and (not (string-empty-p line)) line))))))

(defun pos-lock-auto (dir)
  "Return the auto directory of the repository at DIR, or nil.
It is in the configuration directory at DIR, which a repository whose
working tree has no configuration lacks."
  (when-let* ((file (pos-node-config-file dir)))
    (expand-file-name "auto" (expand-file-name (file-name-directory file) dir))))

(defun pos-lock-image (source dir)
  "Return the image's skills at DIR, a repository's root, as a source.
SOURCE is the directory the image installs from: it holds repository,
the git URL of the repository the image was built from; commit, the
full id of the commit; and skills/NAME/.  An alist of source, ref and
skills, a vector of the names, as a declared source's entry is.  A
slot's skill is left out where DIR fills the slot itself: something is
at .agents/skills/NAME that is not a symbolic link into DIR's auto/
and has no entry in DIR's skills-lock.json of the image's repository.
Nil where SOURCE names no repository or commit, or holds no skill."
  (let ((repository (pos-lock--line source "repository"))
        (commit (pos-lock--line source "commit"))
        (skills (expand-file-name "skills" source)))
    (when (and repository commit (file-directory-p skills))
      (let* ((file (expand-file-name pos-lock-file dir))
             (entries (and (file-exists-p file) (pos-lock--entries file)))
             (key (pos-node-source-key repository))
             (auto (pos-lock-auto dir))
             (names
              (seq-remove
               (lambda (name)
                 (let* ((path (expand-file-name (concat ".agents/skills/" name) dir))
                        (to (file-symlink-p path))
                        (entry (and (listp entries) (cdr (assoc name entries)))))
                   (and (member name pos-lock-slots)
                        (or to (file-exists-p path))
                        (not (and (stringp to) auto
                                  (string-prefix-p
                                   (file-name-as-directory auto)
                                   (expand-file-name to (file-name-directory path)))))
                        (not (and entry (equal (pos-lock--key entry) key))))))
               (seq-filter
                (lambda (name)
                  (file-exists-p (expand-file-name "SKILL.md" (expand-file-name name skills))))
                (directory-files skills nil directory-files-no-dot-files-regexp)))))
        (when names
          `((source . ,repository) (ref . ,commit) (skills . ,(vconcat names))))))))

(defun pos-lock--sources (config image)
  "Return the sources CONFIG declares, and IMAGE after them if given."
  (let ((declared (alist-get 'methodologies config)))
    (append (and (vectorp declared) (append declared nil))
            (and image (list image)))))

(defun pos-lock--key (entry)
  "Return the source of ENTRY, a lock entry, as it is compared.
Its sourceUrl where it has one, else its source."
  (let ((url (alist-get 'sourceUrl entry)))
    (pos-node-source-key (if (stringp url) url (alist-get 'source entry)))))

(defun pos-lock-differences (dir config &optional image)
  "Return how the skills-lock.json at DIR differs from CONFIG's sources.
DIR is a node's directory and CONFIG its configuration, as
`pos-node-read-config' gives it.  IMAGE, if given, is one source more,
as `pos-lock-image' gives it.  A list of alists, each of finding,
lock-differs or lock-undeclared; skill, the skill's name, or nil for
a finding of a whole source or file; and detail.  An entry of a
declared source at its ref differs where its skill's directory in
.agents/skills/ is not there, or has another hash than computedHash.
The entries come in the order of their names, then the sources in the
order declared.
None where CONFIG declares no sources and DIR has no skills-lock.json."
  (let* ((file (expand-file-name pos-lock-file dir))
         (sources (pos-lock--sources config image))
         (entries (and (file-exists-p file) (pos-lock--entries file)))
         (found nil))
    (cl-flet ((find (finding skill detail)
                (push `((finding . ,finding) (skill . ,skill) (detail . ,detail)) found))
              (source-of (key)
                (seq-find (lambda (source)
                            (equal (pos-node-source-key (alist-get 'source source)) key))
                          sources)))
      (cond
       ((and (not sources) (not (file-exists-p file))))
       ((eq entries :invalid)
        (find "lock-differs" nil "not a lock file"))
       (t
        (pcase-dolist (`(,name . ,entry) entries)
          (let ((source (source-of (pos-lock--key entry)))
                (locked (alist-get 'ref entry)))
            (let-alist source
              (cond
               ((not source)
                (find "lock-undeclared" name
                      (format "%s: from %s, which is not declared"
                              name (alist-get 'source entry))))
               ((and (vectorp .skills) (not (seq-contains-p .skills name)))
                (find "lock-undeclared" name
                      (format "%s: not among the skills of %s" name .source)))
               ((not (equal locked .ref))
                (find "lock-differs" name
                      (format "%s: %s installed, %s declared"
                              name (if (stringp locked) locked "no ref") .ref)))
               (t
                (let ((installed (expand-file-name (concat ".agents/skills/" name) dir))
                      (hash (alist-get 'computedHash entry)))
                  (cond
                   ((not (file-directory-p installed))
                    (find "lock-differs" name
                          (format "%s: not in .agents/skills/" name)))
                   ((and (stringp hash)
                         (not (equal hash (pos-lock-hash installed))))
                    (find "lock-differs" name
                          (format "%s: changed since it was installed"
                                  name))))))))))
        (dolist (source sources)
          (let-alist source
            (let* ((key (pos-node-source-key .source))
                   (names (mapcar #'car
                                  (seq-filter (lambda (pair)
                                                (equal (pos-lock--key (cdr pair)) key))
                                              entries))))
              (cond
               ((not names)
                (find "lock-differs" nil (format "%s at %s: not installed" .source .ref)))
               ((vectorp .skills)
                (seq-doseq (skill .skills)
                  (unless (member skill names)
                    (find "lock-differs" skill
                          (format "%s: not installed from %s" skill .source))))))))))))
    (nreverse found)))

(defun pos-lock-installed (dir config &optional image)
  "Return the names of the skills at DIR installed from CONFIG's sources.
IMAGE, if given, is one source more, as `pos-lock-image' gives it.
Each has an entry in DIR's skills-lock.json of a source CONFIG
declares, or of IMAGE, of a skill that source's entry names if it
names any.  By name."
  (let* ((file (expand-file-name pos-lock-file dir))
         (declared (pos-lock--sources config image))
         (entries (and declared (file-exists-p file)
                       (pos-lock--entries file))))
    (unless (eq entries :invalid)
      (mapcar #'car
              (seq-filter
               (lambda (pair)
                 (when-let* ((source (seq-find
                                      (lambda (source)
                                        (equal (pos-node-source-key
                                                (alist-get 'source source))
                                               (pos-lock--key (cdr pair))))
                                      declared)))
                   (let ((skills (alist-get 'skills source)))
                     (or (not (vectorp skills))
                         (seq-contains-p skills (car pair))))))
               entries)))))

(provide 'pos-lock)
;;; pos-lock.el ends here
