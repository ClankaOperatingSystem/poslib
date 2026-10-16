;;; pos-sources.el --- A node's sources, changed in its configuration -*- lexical-binding: t; -*-

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

;; A node declares the sources of its skills under methodologies, as
;; section 6 of doc/pos-directory.txt has it.  This module changes
;; them in the text of a configuration: it adds a source or skills of
;; one, removes them, and adopts a wild-caught skill, one whose entry
;; in skills-lock.json is of a source the configuration does not
;; declare.  Each returns the configuration's new text, with the
;; methodologies block written afresh and the rest of the text as it
;; was, and reads that text back before returning it.  Installing
;; what the text declares is the install command's.
;;
;; - `pos-sources-add': a source declared, or skills of one.
;; - `pos-sources-remove': a source, or skills of one, no longer
;;   declared, and the skills installed from it that go with them.
;; - `pos-sources-adopt': wild-caught skills declared from their lock
;;   entries.
;; - `pos-sources-write': a configuration's text with the sources given.

;;; Code:

(require 'cl-lib)
(require 'pos-lock)
(require 'pos-node)
(require 'seq)
(require 'subr-x)

;;;; The text

(defun pos-sources--scalar (value)
  "Return VALUE, a string, as a YAML scalar: plain where it can be."
  (if (and (string-match-p "\\`[A-Za-z0-9_./~][^][ \t\n#,{}\"']*\\'" value)
           (not (string-suffix-p ":" value)))
      value
    (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" value) "\"")))

(defun pos-sources--entry (source)
  "Return the lines of SOURCE, a source as the reader gives it."
  (let-alist source
    (concat "  - source: " (pos-sources--scalar .source) "\n"
            "    ref: " (pos-sources--scalar .ref) "\n"
            (if (vectorp .skills)
                (format "    skills: [%s]\n" (string-join (append .skills nil) ", "))
              "")
            (if (equal .kind "methodology")
                ""
              (format "    kind: %s\n" .kind)))))

(defun pos-sources--block ()
  "Return the bounds of the methodologies block in this buffer, or nil.
A cons of the beginning of its first line and the end of its last
line that is not blank or a comment, with that line's newline.  The
block is the key's line and the lines after it that are indented,
blank or comments, or begin a sequence's entry."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (when (re-search-forward "^methodologies:" nil t)
        (let ((start (line-beginning-position))
              (end (progn (forward-line 1) (point))))
          (while (and (not (eobp))
                      (looking-at "\\(?:[ \t]\\|-[ \t\n]\\|#\\|$\\)"))
            (let ((content (not (looking-at "[ \t]*\\(?:#.*\\)?$"))))
              (forward-line 1)
              (when content
                (setq end (point)))))
          (cons start end))))))

(defun pos-sources--same (a b)
  "Return non-nil if the sources A and B declare the same.
Each is a sequence of sources as the reader gives them."
  (cl-flet ((values (sources)
              (mapcar (lambda (source)
                        (mapcar (lambda (key) (alist-get key source))
                                '(source ref skills kind)))
                      (append sources nil))))
    (equal (values a) (values b))))

(defun pos-sources--read (text)
  "Return the configuration in TEXT, read; refuse methodologies as a path."
  (let ((config (pos-node-read-config text)))
    (when (stringp (alist-get 'methodologies config))
      (pos-node-refuse 'methodologies-path
                       "Methodologies are declared as a path, not as sources"))
    config))

(defun pos-sources--declared (config)
  "Return the sources CONFIG declares, as a list."
  (let ((declared (alist-get 'methodologies config)))
    (and (vectorp declared) (append declared nil))))

(defun pos-sources-write (text sources)
  "Return TEXT, a configuration, with methodologies declaring SOURCES.
SOURCES is a list of sources, each an alist of source, ref, skills, a
vector or :null, and kind.  The methodologies block is written afresh,
or added at the end, or removed where SOURCES is empty; a comment
within it is lost, and the rest of the text is as it was.  The text is
read back, and refused as the reader refuses it.  Signal
`pos-node-refused' too, of kind bad-value, where the text read back
does not declare SOURCES, or differs elsewhere from TEXT."
  (let* ((before (pos-sources--read text))
         (block (mapconcat #'pos-sources--entry sources ""))
         (written
          (with-temp-buffer
            (insert text)
            (let ((bounds (pos-sources--block))
                  (lines (if sources (concat "methodologies:\n" block) "")))
              (if bounds
                  (progn (delete-region (car bounds) (cdr bounds))
                         (goto-char (car bounds))
                         (insert lines))
                (goto-char (point-max))
                (unless (or (bobp) (eq (char-before) ?\n))
                  (insert "\n"))
                (insert lines)))
            (buffer-string)))
         (after (pos-sources--read written)))
    (unless (and (pos-sources--same (pos-sources--declared after) sources)
                 (equal (assq-delete-all 'methodologies (copy-alist after))
                        (assq-delete-all 'methodologies (copy-alist before))))
      (pos-node-refuse 'bad-value
                       "The configuration cannot be written to declare these sources"))
    written))

;;;; The changes

(defun pos-sources--find (sources source)
  "Return the source of SOURCES that is the same source as SOURCE, or nil."
  (let ((key (pos-node-source-key source)))
    (seq-find (lambda (each)
                (equal (pos-node-source-key (alist-get 'source each)) key))
              sources)))

(defun pos-sources--with (source key value)
  "Return SOURCE, an alist, with KEY's value VALUE."
  (cons (cons key value) (assq-delete-all key (copy-alist source))))

(defun pos-sources--installed (dir source)
  "Return the names of the skills at DIR whose lock entries are of SOURCE."
  (let ((file (expand-file-name pos-lock-file dir))
        (key (pos-node-source-key source)))
    (when (file-exists-p file)
      (let ((entries (pos-lock--entries file)))
        (unless (eq entries :invalid)
          (mapcar #'car (seq-filter (lambda (pair)
                                      (equal (pos-lock--key (cdr pair)) key))
                                    entries)))))))

(defun pos-sources-add (text source ref &optional skills kind)
  "Return TEXT, a configuration, declaring SOURCE at REF.
SOURCE is a git URL and REF a tag or a commit.  SKILLS, a list of
names, are the skills to install from it; all of its skills when nil.
KIND is methodology or skills-repository; methodology when nil.
Where SOURCE is declared already, its entry takes REF, and KIND if
given, and SKILLS are added to the skills it names, if it names any.
Signal `pos-node-refused' as
`pos-sources-write' does."
  (let* ((config (pos-sources--read text))
         (sources (pos-sources--declared config))
         (found (pos-sources--find sources source)))
    (pos-sources-write
     text
     (if found
         (mapcar
          (lambda (each)
            (if (not (eq each found))
                each
              (let* ((named (alist-get 'skills found))
                     (names (if (and skills (vectorp named))
                                (vconcat named (seq-difference skills (append named nil)))
                              named)))
                (thread-first found
                              (pos-sources--with 'ref ref)
                              (pos-sources--with 'skills names)
                              (pos-sources--with 'kind (or kind (alist-get 'kind found)))))))
          sources)
       (append sources
               (list `((source . ,source) (ref . ,ref)
                       (skills . ,(if skills (vconcat skills) :null))
                       (kind . ,(or kind "methodology")))))))))

(defun pos-sources-remove (text dir source &optional skills)
  "Return what removing SOURCE, or SKILLS of it, from TEXT comes to.
TEXT is the configuration of the node at DIR, and SOURCE a source it
declares.  Given no SKILLS, the source's entry is removed; given
SKILLS, a list of names, they are removed from the skills it names,
and the entry with them if it names no others.  Where the entry names
no skills, the skills it names are first those installed from it, by
DIR's skills-lock.json.  An alist: text, the new text; skills, the
names of the skills installed from SOURCE that it no longer declares;
and whole, non-nil where the entry was removed.  Signal
`pos-node-refused', of kind not-declared, for a source or a skill
that is not declared, and as `pos-sources-write' does."
  (let* ((config (pos-sources--read text))
         (sources (pos-sources--declared config))
         (found (or (pos-sources--find sources source)
                    (pos-node-refuse 'not-declared "Not a declared source: %s" source)))
         (installed (pos-sources--installed dir (alist-get 'source found)))
         (named (if (vectorp (alist-get 'skills found))
                    (append (alist-get 'skills found) nil)
                  installed)))
    (dolist (skill skills)
      (unless (member skill named)
        (pos-node-refuse 'not-declared "Not a skill declared from %s: %s"
                         (alist-get 'source found) skill)))
    (let* ((left (seq-difference named skills))
           (whole (or (not skills) (not left))))
      `((text . ,(pos-sources-write
                  text
                  (if whole
                      (remq found sources)
                    (mapcar (lambda (each)
                              (if (eq each found)
                                  (pos-sources--with each 'skills (vconcat left))
                                each))
                            sources))))
        (skills . ,(if whole installed (seq-intersection skills installed)))
        (whole . ,whole)))))

(defun pos-sources--url (entry)
  "Return the git URL of ENTRY, a lock entry.
Its sourceUrl where it has one, else its source; a source written
OWNER/REPOSITORY, as the skills CLI writes a GitHub repository's, is
that repository's URL on github.com."
  (let ((url (alist-get 'sourceUrl entry))
        (source (alist-get 'source entry)))
    (cond ((stringp url) url)
          ((string-match-p "\\`[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\\'" source)
           (concat "https://github.com/" (string-remove-suffix ".git" source) ".git"))
          (t source))))

(defun pos-sources-adopt (text dir names)
  "Return TEXT, the configuration of the node at DIR, declaring NAMES.
Each of NAMES is a wild-caught skill: its entry in DIR's
skills-lock.json is of a source TEXT does not declare.  Each source is
declared at the ref of its entries, naming the skills adopted from it.
Its kind is methodology where each of those skills is named with the
source's name and a hyphen, and skills-repository otherwise.  Signal
`pos-node-refused' of kind not-wild-caught for a name that is not a
wild-caught skill; no-ref for one whose entry has no ref, or whose
source's entries adopted have two; and as `pos-sources-write' does."
  (let* ((config (pos-sources--read text))
         (sources (pos-sources--declared config))
         (file (expand-file-name pos-lock-file dir))
         (entries (and (file-exists-p file) (pos-lock--entries file)))
         (groups nil))
    (when (eq entries :invalid)
      (pos-node-refuse 'not-wild-caught "%s is not a lock file" pos-lock-file))
    (dolist (name names)
      (let ((entry (cdr (assoc name entries))))
        (unless entry
          (pos-node-refuse 'not-wild-caught "Not installed: %s" name))
        (let ((url (pos-sources--url entry))
              (ref (alist-get 'ref entry)))
          (when (pos-sources--find sources url)
            (pos-node-refuse 'not-wild-caught "%s: installed from %s, which is declared"
                             name url))
          (unless (stringp ref)
            (pos-node-refuse 'no-ref "%s: installed at no ref; add its source at one"
                             name))
          (let ((group (assoc (pos-node-source-key url) groups)))
            (cond ((not group)
                   (push (list (pos-node-source-key url) url ref name) groups))
                  ((not (equal (nth 2 group) ref))
                   (pos-node-refuse 'no-ref "%s: installed at %s and at %s"
                                    url (nth 2 group) ref))
                  (t (setcdr (nthcdr 2 group)
                             (append (nthcdr 3 group) (list name)))))))))
    (pos-sources-write
     text
     (append sources
             (mapcar (lambda (group)
                       (pcase-let* ((`(,_ ,url ,ref . ,skills) group)
                                    (prefix (concat (pos-node-source-name url) "-")))
                         `((source . ,url) (ref . ,ref)
                           (skills . ,(vconcat (sort (copy-sequence skills) #'string<)))
                           (kind . ,(if (seq-every-p (lambda (skill)
                                                       (string-prefix-p prefix skill))
                                                     skills)
                                        "methodology"
                                      "skills-repository")))))
                     (nreverse groups))))))

(provide 'pos-sources)
;;; pos-sources.el ends here
