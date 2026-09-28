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
Each is (OFFSET TEXT PATH SUFFIX): TEXT the raw link at byte OFFSET,
PATH its file, SUFFIX its search option with its :: or empty."
  (let (links)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (equal (org-element-property :type link) "file")
          (let ((raw (org-element-property :raw-link link))
                (search (org-element-property :search-option link)))
            (save-excursion
              (goto-char (org-element-property :begin link))
              (when (search-forward raw (org-element-property :end link) t)
                (push (list (pos-links--byte (match-beginning 0)) raw
                            (org-element-property :path link)
                            (if search (concat "::" search) ""))
                      links)))))))
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
  "Return the path links in the current Markdown buffer, as `pos-links--org'."
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
            (let ((path (pos-links--markdown-path url)))
              (when path
                (push (list (pos-links--byte beg) url (car path) (cdr path)) links)))))))
    (sort links (lambda (a b) (< (car a) (car b))))))

(defun pos-links-in-file (file)
  "Return the path links in FILE, an Org or Markdown file, else nil.
Each is (OFFSET TEXT PATH SUFFIX), OFFSET in bytes."
  (let ((org (string-suffix-p ".org" file)) (md (string-suffix-p ".md" file)))
    (when (or org md)
      (with-temp-buffer
        (insert (decode-coding-string (pos-ledger--read file) 'utf-8))
        (let ((org-mode-hook nil) (markdown-mode-hook nil))
          (if org (org-mode) (markdown-mode)))
        (if org (pos-links--org) (pos-links--markdown))))))

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

(defun pos-links-resolve (target)
  "Return how a link to TARGET, an absolute path outside the item, resolves.
\(cid . LINK) for archived material, (rumour . TARGET) for anything else
that exists; refuse a broken link."
  (cond
   ((not (or (file-exists-p target) (file-symlink-p target)))
    (pos-ledger--refuse 'broken "Broken link: %s" target))
   ((pos-links--archive-of target) (cons 'cid (pos-links--sealed target)))
   (t (cons 'rumour target))))

(provide 'pos-links)
;;; pos-links.el ends here
