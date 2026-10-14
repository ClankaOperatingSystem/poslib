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
;; not declare.
;;
;; - `pos-lock-file': the name of the file, in the node's directory.
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

(defun pos-lock--key (entry)
  "Return the source of ENTRY, a lock entry, as it is compared.
Its sourceUrl where it has one, else its source."
  (let ((url (alist-get 'sourceUrl entry)))
    (pos-node-source-key (if (stringp url) url (alist-get 'source entry)))))

(defun pos-lock-differences (dir config)
  "Return how the skills-lock.json at DIR differs from CONFIG's sources.
DIR is a node's directory and CONFIG its configuration, as
`pos-node-read-config' gives it.  A list of alists, each of finding,
lock-differs or lock-undeclared; skill, the skill's name, or nil for
a finding of a whole source or file; and detail.  The entries come in
the order of their names, then the sources in the order declared.
None where CONFIG declares no sources and DIR has no skills-lock.json."
  (let* ((file (expand-file-name pos-lock-file dir))
         (declared (alist-get 'methodologies config))
         (sources (and (vectorp declared) (append declared nil)))
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
                              name (if (stringp locked) locked "no ref") .ref)))))))
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

(defun pos-lock-installed (dir config)
  "Return the names of the skills at DIR installed from CONFIG's sources.
Each has an entry in DIR's skills-lock.json of a source CONFIG
declares, of a skill that source's entry names if it names any.  By
name."
  (let* ((file (expand-file-name pos-lock-file dir))
         (declared (alist-get 'methodologies config))
         (entries (and (vectorp declared) (file-exists-p file)
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
