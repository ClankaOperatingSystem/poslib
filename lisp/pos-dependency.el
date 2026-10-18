;;; pos-dependency.el --- What a scope's canon may refer to -*- lexical-binding: t; -*-

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

;; The dependency rule: a scope's canon refers to its own canon and
;; archive, and to its children's canon, and to nothing else in the
;; tree.  The scopes are the corpus's, pos-corpus.el.  A scope's
;; archive is its archives/ and its attic is its attic/; the rest of
;; what it holds is its canon.
;;
;; A reference is an Org link by file or by ID, in the text or in a
;; property's value, an Org #+INCLUDE or #+SETUPFILE, a Markdown link,
;; or a path into a home directory written in any text file.  In a
;; file that is neither Org nor Markdown, which is code or data, a
;; relative path that climbs and Python's Path.parents[N] are
;; references too.
;;
;; Each finding names the rule it breaks:
;;
;;   D-canon-links          the target is in the tree, outside the scope
;;   D-attic-links          the target is in an attic
;;   D-child-archive-links  the target is in a child's archive
;;   D-sibling-links        the target is beside the root, or a relative
;;                          path climbs out of it
;;   D-outside-links        the target is elsewhere on disk
;;   D-links-resolve        nothing is at the target, or two files hold
;;                          the ID
;;
;; A line is exempt from a rule where `doctrine: allow RULE' is
;; written on it, on the line above, or on the line above the code
;; block that holds it; a file is, where `doctrine: allow-file RULE' is
;; written in its first five lines.  Either names one rule or more.
;;
;; - `pos-dependency-findings': the references of the root's own files
;;   that break the rule, each a `pos-finding'.
;; - `pos-dependency-place': the scope that holds a path, and whether
;;   it is in that scope's canon, attic or archive.
;; - `pos-dependency-classify': whether a reference from one file to a
;;   path is allowed, and why.
;; - `pos-dependency-references': the references in a file.
;; - `pos-dependency-allowed-p': non-nil if a pragma in FILE exempts
;;   LINE from RULE.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-bytes)
(require 'pos-corpus)
(require 'pos-links)
(require 'pos-person)
(require 'pos-referrers)

;;;; Where a path lies

(defun pos-dependency--scope-file (corpus scope)
  "Return the Org file that SCOPE is, if it is a project of one file.
SCOPE is a scope of CORPUS; nil for any other scope."
  (when (eq 'project (pos-scope-kind scope))
    (let ((file (concat (expand-file-name (pos-scope-path scope) (pos-corpus-root corpus))
                        ".org")))
      (and (eq scope (pos-corpus-owner corpus file)) file))))

(defun pos-dependency-place (corpus path)
  "Return where PATH lies in CORPUS's tree: (SCOPE . KIND), or nil.
Nil for a path outside the tree.  SCOPE is the innermost scope that
holds PATH, a `pos-scope', and KIND is canon, attic or archive.  A
project of one Org file holds that file alone."
  (let ((path (expand-file-name path)) (best nil) (extent 0))
    (when (string-prefix-p (pos-corpus-root corpus) (file-name-as-directory path))
      (dolist (scope (pos-corpus-scopes corpus))
        (let* ((file (pos-dependency--scope-file corpus scope))
               (holds (or file (pos-scope-dir scope))))
          (when (and (if file
                         (equal path file)
                       (string-prefix-p holds (file-name-as-directory path)))
                     (> (length holds) extent))
            (setq best scope extent (length holds)))))
      (let ((rest (if (pos-dependency--scope-file corpus best)
                      ""
                    (substring (file-name-as-directory path) extent))))
        (cons best
              (cond ((string-prefix-p "archives/" rest) 'archive)
                    ((and (string-prefix-p "attic/" rest) (not (equal rest "attic/")))
                     'attic)
                    (t 'canon)))))))

(defun pos-dependency--within-p (scope other)
  "Return non-nil if scope OTHER is SCOPE or lies within it."
  (or (eq scope other)
      (string-prefix-p (pos-scope-dir scope) (pos-scope-dir other))))

(defun pos-dependency-classify (corpus source target &optional absolute)
  "Return whether SOURCE, a file of CORPUS, may refer to TARGET, and why.
TARGET is an absolute path.  ABSOLUTE is non-nil if SOURCE wrote it as
an absolute path, not a relative one.  The value is (VERDICT .
DETAIL): VERDICT is ok, poor-form, inadvisable or forbidden, and
DETAIL says where TARGET lies.  A relative path that climbs out of
the root depends on where the root is, and a path beside the root is
a sibling's: both are forbidden.  Any other path outside the root,
such as a tool's configuration, is inadvisable."
  (let* ((root (pos-corpus-root corpus))
         (from (pos-dependency-place corpus source))
         (to (pos-dependency-place corpus target)))
    (cond ((and (null to) (not absolute)) '(forbidden . "climbs out of the root"))
          ((and (null to)
                (string-prefix-p (file-name-directory (directory-file-name root))
                                 (expand-file-name target)))
           '(forbidden . "a sibling on disk"))
          ((null to) '(inadvisable . "outside the root on disk"))
          ((and (pos-dependency--scope-file corpus (car from))
                (not (eq (car to) (car from))))
           '(forbidden . "outside a single-file scope"))
          ((not (pos-dependency--within-p (car from) (car to)))
           '(forbidden . "outside the scope"))
          ((eq (cdr to) 'attic) '(forbidden . "into an attic"))
          ((eq (car to) (car from))
           (if (eq (cdr to) 'archive) '(ok . "own archive") '(ok . "own canon")))
          ((eq (cdr to) 'archive) '(poor-form . "into a child's archive"))
          (t '(ok . "child canon")))))

(defun pos-dependency--archived (corpus target)
  "Return the same path as TARGET in its scope's archive, if a file is there.
TARGET is a path of CORPUS's tree.  An attic keeps canon's paths, so
attic/X is sought at archives/X."
  (let ((place (pos-dependency-place corpus target)))
    (when (and place (memq (cdr place) '(canon attic))
               (not (pos-dependency--scope-file corpus (car place))))
      (let* ((dir (pos-scope-dir (car place)))
             (base (if (eq (cdr place) 'attic) (concat dir "attic/") dir))
             (candidate (expand-file-name
                         (concat "archives/"
                                 (string-remove-prefix base (expand-file-name target)))
                         dir)))
        (when (file-exists-p candidate) candidate)))))

;;;; References

(defun pos-dependency--text-file-p (file)
  "Return non-nil if FILE is text: no NUL in its first 8192 bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 8192)
    (not (search-forward "\0" nil t))))

(defun pos-dependency--file-target (base link)
  "Return the absolute path that LINK leads to from directory BASE, or nil.
Nil for a LINK that names no file, a search option alone."
  (let ((link (car (split-string link "::"))))
    (unless (string-empty-p link)
      (expand-file-name (substitute-in-file-name link) base))))

(defun pos-dependency--org-references (file)
  "Return the references in Org FILE, each (LINE KIND RAW TARGET).
KIND is link for a link by file, id for a link by ID, whose TARGET is
the ID, and include for #+INCLUDE and #+SETUPFILE.  A link in a
property's value is among them."
  (let ((base (file-name-directory file)) refs)
    (with-temp-buffer
      (insert (decode-coding-string (pos-bytes-read file) 'utf-8))
      ;; Read this file alone: no #+SETUPFILE and no mode hooks.
      (cl-letf (((symbol-function 'org-file-contents) (lambda (&rest _) "")))
        (let ((org-mode-hook nil)) (org-mode)))
      (let ((tree (org-element-parse-buffer)))
        (org-element-map tree 'link
          (lambda (link)
            (let ((type (org-element-property :type link))
                  (line (line-number-at-pos (org-element-property :begin link)))
                  (raw (org-element-property :raw-link link))
                  (path (org-element-property :path link)))
              (cond
               ((equal type "id") (push (list line 'id raw path) refs))
               ((member type '("file" "file+sys" "file+emacs"))
                (push (list line 'link raw (pos-dependency--file-target base path))
                      refs))))))
        (org-element-map tree 'node-property
          (lambda (property)
            (let ((value (or (org-element-property :value property) ""))
                  (line (line-number-at-pos (org-element-property :begin property)))
                  (start 0))
              (while (string-match org-link-bracket-re value start)
                (let ((raw (match-string 1 value)))
                  (setq start (match-end 0))
                  (cond
                   ((string-prefix-p "id:" raw)
                    (push (list line 'id raw
                                (org-link-unescape (string-remove-prefix "id:" raw)))
                          refs))
                   ((string-prefix-p "file:" raw)
                    (push (list line 'link raw
                                (pos-dependency--file-target
                                 base (string-remove-prefix "file:" raw)))
                          refs))))))))
        (org-element-map tree 'keyword
          (lambda (keyword)
            (when (member (org-element-property :key keyword) '("INCLUDE" "SETUPFILE"))
              (let ((value (string-trim (org-element-property :value keyword))))
                (when (string-match "\\`\"?\\([^\" ]+\\)" value)
                  (let ((raw (match-string 1 value)))
                    (push (list (line-number-at-pos (org-element-property :begin keyword))
                                'include raw
                                (pos-dependency--file-target
                                 base (string-remove-prefix "file:" raw)))
                          refs)))))))))
    (nreverse refs)))

(defun pos-dependency--markdown-references (file)
  "Return the links in Markdown FILE, each (LINE link RAW TARGET).
They are the path links `pos-links-in-file' finds."
  (let ((base (file-name-directory file))
        (bytes (pos-bytes-read file)))
    (delq nil
          (mapcar (lambda (link)
                    (unless (string-empty-p (pos-link-path link))
                      (list (pos-referrers-line bytes (pos-link-offset link)) 'link
                            (pos-link-text link)
                            (expand-file-name (pos-link-path link) base))))
                  (pos-links-in-file file)))))

;; doctrine: allow D-outside-links (the pattern itself)
(defconst pos-dependency--home-regexp
  (concat "\\(?:\\`\\|[^[:alnum:]_.~/-]\\)"
          "\\(\\(?:/Users/\\|/home/\\|~/\\)[^][[:space:]\"'`(){}<>,;*]+\\)")
  "Matches an absolute path into a home directory, where checkouts are.")

(defconst pos-dependency--climb-regexp
  "\\(?:\\`\\|[^[:alnum:]_.]\\)\\(\\(?:\\.\\./\\)+[^][[:space:]\"'`(){}<>,;*]*\\)"
  "Matches a relative path that climbs out of its directory.")

(defconst pos-dependency--parents-regexp "\\.parents\\[\\([0-9]+\\)\\]"
  "Matches Python's Path.parents[N], which climbs N+1 directories from a file.")

(defun pos-dependency--path-references (file code)
  "Return the paths written in FILE, each (LINE KIND RAW TARGET).
A path into a home directory always, of KIND path.  With CODE
non-nil, a relative path that climbs, of KIND path too, and
Path.parents[N], of KIND walk-up, whose TARGET is the directory it
reaches."
  (let ((base (file-name-directory file)) refs)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position)))
              (number (line-number-at-pos)))
          (dolist (regexp (if code
                              (list pos-dependency--home-regexp pos-dependency--climb-regexp)
                            (list pos-dependency--home-regexp)))
            (let ((start 0))
              (while (string-match regexp line start)
                (setq start (match-end 1))
                (let ((raw (string-trim-right (match-string 1 line) "[.:=]+")))
                  (push (list number 'path raw (expand-file-name raw base)) refs)))))
          (when code
            (let ((start 0))
              (while (string-match pos-dependency--parents-regexp line start)
                (setq start (match-end 0))
                (let ((dir (directory-file-name base)))
                  (dotimes (_ (string-to-number (match-string 1 line)))
                    (setq dir (directory-file-name (file-name-directory dir))))
                  (push (list number 'walk-up (match-string 0 line) dir) refs))))))
        (forward-line 1)))
    (nreverse refs)))

(defun pos-dependency-references (file)
  "Return the references in FILE, each (LINE KIND RAW TARGET).
RAW is the reference as written and TARGET the absolute path it
leads to, or for KIND id, the ID.  One is given for each line and
target."
  (seq-uniq
   (cond ((string-suffix-p ".org" file)
          (append (pos-dependency--org-references file)
                  (pos-dependency--path-references file nil)))
         ((string-suffix-p ".md" file)
          (append (pos-dependency--markdown-references file)
                  (pos-dependency--path-references file nil)))
         ((pos-dependency--text-file-p file)
          (pos-dependency--path-references file t)))
   (lambda (a b) (and (equal (car a) (car b))
                      (eq (eq (nth 1 a) 'id) (eq (nth 1 b) 'id))
                      (equal (nth 3 a) (nth 3 b))))))

;;;; Pragmas

(defun pos-dependency--pragma-p (text rule kind)
  "Return non-nil for a pragma in TEXT, of KIND, naming RULE.
KIND is allow or allow-file."
  (let ((start 0) found)
    (while (and (not found)
                (string-match (concat "doctrine: " kind "\\(\\(?: D-[[:alnum:]-]+\\)+\\)")
                              text start))
      (setq start (match-end 0)
            found (member rule (split-string (match-string 1 text)))))
    found))

(defun pos-dependency--above-block ()
  "Return the line above the code block around point's line, or nil.
A block is a Markdown fence or an Org #+begin_ block."
  (let ((target (line-number-at-pos)) above previous)
    (save-excursion
      (goto-char (point-min))
      (while (< (line-number-at-pos) target)
        (let ((text (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position)))
              (case-fold-search t))
          (cond ((string-match-p "\\`[ \t]*\\(```\\|~~~\\)" text)
                 (setq above (unless above (or previous ""))))
                ((string-match-p "\\`[ \t]*#\\+begin_" text) (setq above (or previous "")))
                ((string-match-p "\\`[ \t]*#\\+end_" text) (setq above nil)))
          (setq previous text))
        (forward-line 1)))
    above))

(defun pos-dependency-allowed-p (file line rule)
  "Return non-nil if a pragma in FILE exempts LINE from RULE.
That is `doctrine: allow RULE' on LINE, on the line above, or on the
line above the code block that holds LINE, or `doctrine: allow-file
RULE' in FILE's first five lines."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (or (pos-dependency--pragma-p
         (buffer-substring-no-properties (point) (progn (forward-line 5) (point)))
         rule "allow-file")
        (progn
          (goto-char (point-min))
          (forward-line (max 0 (- line 2)))
          (pos-dependency--pragma-p
           (buffer-substring-no-properties
            (point) (progn (forward-line (if (> line 1) 2 1)) (point)))
           rule "allow"))
        (progn
          (goto-char (point-min))
          (forward-line (1- line))
          (let ((above (pos-dependency--above-block)))
            (and above (pos-dependency--pragma-p above rule "allow")))))))

;;;; Findings

(defun pos-dependency--verdict (verdict)
  "Return VERDICT, a symbol, as the text of a finding."
  (if (eq verdict 'poor-form) "poor form" (symbol-name verdict)))

(defun pos-dependency--id-finding (corpus file ref ids)
  "Return (RULE . MESSAGE) for ID reference REF in FILE, or nil if it is allowed.
IDS is as `pos-links-ids' gives it for CORPUS's files and its
people's, `pos-person-files'."
  (pcase-let* ((`(,_ ,_ ,raw ,path) ref)
               ;; Org tries the part before ::, then the whole, for an
               ;; ID that holds :: itself.
               (places (or (gethash (car (split-string path "::")) ids)
                           (gethash path ids)))
               (root (pos-corpus-root corpus)))
    (cond
     ((null places)
      (cons "D-links-resolve" (format "no file holds the ID: %s" raw)))
     ((cdr places)
      (cons "D-links-resolve"
            (format "the ID is in %s: %s"
                    (string-join (sort (mapcar (lambda (place)
                                                 (concat (file-relative-name (car place) root)
                                                         (cdr place)))
                                               places)
                                       #'string<)
                                 ", ")
                    raw)))
     (t
      (let ((verdict (pos-dependency-classify corpus file (caar places))))
        (unless (eq (car verdict) 'ok)
          (cons "D-canon-links"
                (format "%s: %s: %s (%s)" (pos-dependency--verdict (car verdict))
                        (cdr verdict) raw (file-relative-name (caar places) root)))))))))

(defun pos-dependency--path-findings (corpus file ref)
  "Return (RULE . MESSAGE) for each break of the rule by REF, a path in FILE.
FILE is a file of CORPUS.  A link into a child's archive may also
lead nowhere; both are given, so that a pragma that allows the one
does not hide the other."
  (pcase-let* ((`(,_ ,kind ,raw ,target) ref)
               (root (pos-corpus-root corpus))
               (exists (or (file-exists-p target) (file-symlink-p target)))
               (verdict (pos-dependency-classify
                         corpus file target
                         (string-match-p "\\`\\(?:file:\\(?://\\)?\\)?[/~]" raw))))
    (delq nil
          (list
           (unless (eq (car verdict) 'ok)
             (cons (cond ((eq (car verdict) 'poor-form) "D-child-archive-links")
                         ((equal (cdr verdict) "into an attic") "D-attic-links")
                         ((pos-dependency-place corpus target) "D-canon-links")
                         ((eq (car verdict) 'forbidden) "D-sibling-links")
                         (t "D-outside-links"))
                   (format "%s: %s: %s" (pos-dependency--verdict (car verdict))
                           (cdr verdict) raw)))
           (when (and (memq (car verdict) '(ok poor-form)) (not exists)
                      (not (eq kind 'path)))
             (let ((archived (pos-dependency--archived corpus target)))
               (cons "D-links-resolve"
                     (if archived
                         (format "moved to %s: %s" (file-relative-name archived root) raw)
                       (format "does not resolve: %s" raw)))))))))

(defun pos-dependency-findings (corpus)
  "Return the references by the root's own files that break the dependency rule.
CORPUS is the tree's corpus.  The root's own files are those a
command of the root may write, the files of other repositories and of
products left out.  Each finding is a `pos-finding' whose message
begins with the rule it breaks."
  (let ((ids (pos-links-ids (append (pos-corpus-files corpus) (pos-person-files corpus))))
        findings)
    (dolist (file (mapcar #'car (append (pos-corpus-entries corpus)
                                        (pos-corpus-others corpus))))
      (when (pos-corpus-path-writable-p corpus file)
        (pcase-dolist ((and ref `(,line ,kind ,_ ,target))
                       (pos-dependency-references file))
          (dolist (found (cond ((eq kind 'id)
                                (let ((one (pos-dependency--id-finding corpus file ref ids)))
                                  (and one (list one))))
                               (target (pos-dependency--path-findings corpus file ref))))
            (unless (pos-dependency-allowed-p file line (car found))
              (push (pos-finding-make :file file :line line
                                      :message (concat (car found) ": " (cdr found)))
                    findings))))))
    (sort findings #'pos-finding<)))

(provide 'pos-dependency)
;;; pos-dependency.el ends here
