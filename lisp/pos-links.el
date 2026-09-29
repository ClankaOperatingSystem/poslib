;;; pos-links.el --- Find and resolve links in items being sealed -*- lexical-binding: t; -*-

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

;; Links in Org and Markdown files, found as Org and markdown-mode find
;; them, and their resolution when an item is sealed, as
;; doc/formats.org specifies.
;;
;; - `pos-links-in-file': the path links in a file, with byte offsets.
;; - `pos-links-resolve': where one leads, from where it was written.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'markdown-mode)
(require 'url-util)
(require 'pos-cid)
(require 'pos-ledger)

;;;; Finding

(defun pos-links--byte (position)
  "Return the byte offset, from 0, of POSITION in the current buffer."
  (1- (position-bytes position)))

(defun pos-links--org ()
  "Return the file links in the current Org buffer.
Each is (OFFSET TEXT PATH SUFFIX OFFSET TEXT): TEXT the raw link at byte
OFFSET, PATH its file, SUFFIX its search option with its :: or empty;
the raw link is also the whole that an annotation replaces."
  (let (links)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (equal (org-element-property :type link) "file")
          (let ((raw (org-element-property :raw-link link))
                (search (org-element-property :search-option link)))
            (save-excursion
              (goto-char (org-element-property :begin link))
              (when (search-forward raw (org-element-property :end link) t)
                (let ((offset (pos-links--byte (match-beginning 0))))
                  (push (list offset raw (org-element-property :path link)
                              (if search (concat "::" search) "")
                              offset raw)
                        links))))))))
    (nreverse links)))

(defun pos-links--markdown-path (url)
  "Return (PATH . SUFFIX) of the Markdown URL, or nil if it is not a path."
  (let ((url (string-trim url "<" ">")))
    (unless (or (string-empty-p url) (string-prefix-p "#" url)
                (string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*:" url))
      (let ((hash (string-search "#" url)))
        (cons (url-unhex-string (if hash (substring url 0 hash) url))
              (if hash (substring url hash) ""))))))

(defun pos-links--markdown ()
  "Return the path links in the current Markdown buffer, as `pos-links--org'.
The whole of an inline link is all of it; of a reference definition, its URL."
  (syntax-propertize (point-max))
  (let (links)
    (dolist (spec (list (cons markdown-regex-link-inline 6)
                        (cons markdown-regex-reference-definition 5)))
      (goto-char (point-min))
      (while (re-search-forward (car spec) nil t)
        (let ((beg (match-beginning (cdr spec))) (url (match-string-no-properties (cdr spec))))
          (unless (or (null beg)
                      (markdown-code-block-at-pos (match-beginning 0))
                      (markdown-inline-code-at-pos-p (match-beginning 0)))
            (let ((path (pos-links--markdown-path url))
                  (inline (eq (car spec) markdown-regex-link-inline)))
              (when path
                (push (list (pos-links--byte beg) url (car path) (cdr path)
                            (pos-links--byte (if inline (match-beginning 0) beg))
                            (if inline (match-string-no-properties 0) url)
                            (and inline (match-string-no-properties 3)))
                      links)))))))
    (sort links (lambda (a b) (< (car a) (car b))))))

(defun pos-links-in-file (file)
  "Return the path links in FILE, an Org or Markdown file, else nil.
Each is (OFFSET TEXT PATH SUFFIX WHOLE-OFFSET WHOLE DESCRIPTION), offsets
in bytes: TEXT the target, WHOLE what an annotation replaces."
  (let ((org (string-suffix-p ".org" file)) (md (string-suffix-p ".md" file)))
    (when (or org md)
      (with-temp-buffer
        (insert (decode-coding-string (pos-ledger--read file) 'utf-8))
        ;; Read this file alone: no #+SETUPFILE, which may name another
        ;; file or a URL, and no mode hooks.
        (cl-letf (((symbol-function 'org-file-contents) (lambda (&rest _) "")))
          (let ((org-mode-hook nil) (markdown-mode-hook nil))
            (if org (org-mode) (markdown-mode))))
        (if org (pos-links--org) (pos-links--markdown))))))

(defun pos-links-annotate (file link label)
  "Return the annotation replacing LINK's whole in FILE, marked LABEL.
LABEL is broken or later.  In Org, the link's type becomes LABEL; in
Markdown, the link becomes its text and a bracketed note."
  (pcase-let ((`(,_ ,text ,_ ,_ ,_ ,whole ,description) link))
    (cond
     ((string-suffix-p ".org" file)
      (concat label ":" (if (string-prefix-p "file:" text) (substring text 5) text)))
     (description
      (format "%s [%s: %s]" description
              (if (equal label "broken") "broken link" "later record") text))
     (t (concat label ":" whole)))))

(defun pos-links-rewrite (bytes rewrites)
  "Return BYTES with each (OFFSET FROM TO) of REWRITES applied.
FROM must be at OFFSET."
  (let ((out bytes))
    (dolist (r (sort (copy-sequence rewrites) (lambda (a b) (> (car a) (car b)))))
      (pcase-let* ((`(,offset ,from ,to) r)
                   (from (encode-coding-string from 'utf-8))
                   (to (encode-coding-string to 'utf-8)))
        (unless (equal from (substring out offset (+ offset (length from))))
          (pos-ledger--refuse 'plan "Link not where it was planned, at byte %d" offset))
        (setq out (concat (substring out 0 offset) to
                          (substring out (+ offset (length from)))))))
    out))

;;;; Resolving

(defun pos-links--archive-of (file)
  "Return the outermost archive above FILE, or nil."
  (let (found (dir (file-name-directory (directory-file-name file))))
    (while (and dir (not (equal dir (file-name-directory (directory-file-name dir)))))
      (when (equal (file-name-nondirectory (directory-file-name dir)) "archives")
        (setq found (directory-file-name dir)))
      (setq dir (file-name-directory (directory-file-name dir))))
    found))

(defun pos-links--sealed (target)
  "Return the ipfs:// link to archived TARGET, as sealed, or refuse."
  (let* ((archive (pos-links--archive-of target))
         (rel (file-relative-name target archive)))
    (pcase-let ((`(,entries ,_ ,_ ,_ ,_ ,collections ,items) (pos-ledger-history archive)))
      (let ((item (or (seq-find (lambda (i) (pos-ledger--within-p rel i)) items)
                      (seq-find (lambda (c) (pos-ledger--within-p rel c)) collections)
                      (and (assoc rel entries) rel))))
        (unless item
          (pos-ledger--refuse 'unsealed "Link to an archived path not sealed: %s" target))
        (let ((path (expand-file-name item archive)))
          (concat "ipfs://" (if (file-directory-p path) (pos-cid-directory path)
                              (pos-cid-file path))
                  (unless (equal rel item) (concat "/" (substring rel (1+ (length item)))))))))))

(defun pos-links-garden (scope)
  "Return the root of the garden holding SCOPE.
Its repository's, else the outermost directory above it with an archive."
  (file-truename
   (or (locate-dominating-file scope ".git")
       (let ((dir (file-name-as-directory (expand-file-name scope))) found)
         (while dir
           (when (file-directory-p (expand-file-name "archives" dir)) (setq found dir))
           (let ((parent (file-name-directory (directory-file-name dir))))
             (setq dir (unless (equal parent dir) parent))))
         found)
       scope)))

(defun pos-links-outside-garden-p (target scope)
  "Return non-nil if TARGET lies outside the garden holding SCOPE."
  (not (string-prefix-p (file-name-as-directory (pos-links-garden scope))
                        (file-truename target))))

(defun pos-links-description (target scope)
  "Return the description of TARGET a rumour gives, from SCOPE's garden.
Nothing outside the garden is read: only that it lies there is said."
  (cond
   ((pos-links-outside-garden-p target scope) "outside the garden, and was not read")
   ((file-directory-p target) "a directory")
   (t (let ((bytes (pos-ledger--read target))
            (title (pos-links--title target)))
        (format "a file of %d bytes, SHA-256 =%s=%s" (length bytes) (pos-ledger--sha bytes)
                (if title (format ", titled \"%s\"" title) ""))))))

(defun pos-links--title (file)
  "Return FILE's title, from an Org #+TITLE or a Markdown heading, or nil."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert (decode-coding-string (pos-ledger--read file) 'utf-8))
      (goto-char (point-min))
      (when (re-search-forward (if (string-suffix-p ".md" file) "^# +\\(.+\\)$"
                                 "^#\\+TITLE: *\\(.+\\)$")
                               nil t)
        (string-trim (match-string 1))))))

(defun pos-links-within-scope-p (archive scope)
  "Return non-nil if ARCHIVE is SCOPE's own, or a scope's within it."
  (string-prefix-p (file-name-as-directory (file-truename scope))
                   (file-name-as-directory (file-truename archive))))

(defun pos-links-resolve (target &optional scope)
  "Return how a link to TARGET, an absolute path outside the item, resolves.
\(cid . LINK) for material archived in SCOPE or a scope within it,
\(rumour . TARGET) for anything else that exists, a container's archive
included, and (broken . TARGET) for nothing.  Without SCOPE, any archive
is cited."
  (let ((archive (pos-links--archive-of target)))
    (cond
     ((not (or (file-exists-p target) (file-symlink-p target)))
      (cons 'broken target))
     ((and archive (or (null scope) (pos-links-within-scope-p archive scope)))
      (cons 'cid (pos-links--sealed target)))
     (t (cons 'rumour target)))))

(provide 'pos-links)
;;; pos-links.el ends here
