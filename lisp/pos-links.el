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
;; - `pos-links-in-file': the path links in a file, each a `pos-link'.
;; - `pos-links-ids': each Org ID in some files, and where it is held.
;; - `pos-links-resolve': where one leads, from where it was written.
;; - `pos-resolution': what becomes of one when its item is sealed.
;; - `pos-links-link': the ipfs:// link to a sealed path in an archive.
;; - `pos-links-archive-of': the outermost archive above a path.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'markdown-mode)
(require 'url-util)
(require 'pos-bytes)
(require 'pos-path)
(require 'pos-cid)
(require 'pos-ledger)

;;;; Finding

(cl-defstruct (pos-link (:constructor pos-link-create) (:copier nil))
  "A path link found in an Org or Markdown file.
TEXT is the target as written, at byte OFFSET; PATH its file and SUFFIX
its search option or fragment, with its :: or #, or empty.  WHOLE, at
byte WHOLE-OFFSET, is what an annotation replaces: in Org the raw
link, in Markdown all of an inline link, or a reference definition's
URL.  DESCRIPTION is a Markdown inline link's text, else nil."
  offset text path suffix whole-offset whole description)

(defun pos-links--byte (position)
  "Return the byte offset, from 0, of POSITION in the current buffer."
  (1- (position-bytes position)))

(defconst pos-links-no-file "/no-file-holds-the-id/"
  "The directory an id link leads into when no file holds its ID.
Nothing is there, so the link resolves as one to nothing.")

(defun pos-links--id-place (id ids)
  "Return (PATH . SUFFIX) that a link to ID leads to, by IDS.
IDS is as `pos-links-ids' gives it.  PATH is the file that holds ID,
absolute, and SUFFIX the search option of the place in it.  An ID no
file holds leads into `pos-links-no-file'.  Refuse one that two
files hold (unresolved)."
  (let ((places (gethash id ids)))
    (cond
     ((null places) (cons (concat pos-links-no-file id) ""))
     ((cdr places)
      (pos-ledger-refuse 'unresolved "Two files hold the ID %s: %s" id
                         (mapconcat #'car (reverse places) ", ")))
     (t (car places)))))

(defun pos-links--org (&optional ids)
  "Return the file links in the current Org buffer, each a `pos-link'.
The raw link is both the text and the whole that an annotation
replaces.  With IDS, as `pos-links-ids' gives it, an id link is one
too: its path and suffix are where `pos-links--id-place' says its ID is
held."
  (let (links)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let* ((type (org-element-property :type link))
               (raw (org-element-property :raw-link link))
               (search (org-element-property :search-option link))
               (place (cond
                       ((equal type "file")
                        (cons (org-element-property :path link)
                              (if search (concat "::" search) "")))
                       ((and ids (equal type "id"))
                        (pos-links--id-place (org-element-property :path link)
                                             ids)))))
          (when place
            (save-excursion
              (goto-char (org-element-property :begin link))
              (when (search-forward raw (org-element-property :end link) t)
                (let ((offset (pos-links--byte (match-beginning 0))))
                  (push (pos-link-create :offset offset :text raw
                                         :path (car place) :suffix (cdr place)
                                         :whole-offset offset :whole raw)
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
            ;; Decoding the URL searches strings, which would take the
            ;; match data the fields below are read from.
            (let ((path (save-match-data (pos-links--markdown-path url)))
                  (inline (eq (car spec) markdown-regex-link-inline)))
              (when path
                (push (pos-link-create
                       :offset (pos-links--byte beg) :text url
                       :path (car path) :suffix (cdr path)
                       :whole-offset (pos-links--byte (if inline (match-beginning 0) beg))
                       :whole (if inline (match-string-no-properties 0) url)
                       :description (and inline (match-string-no-properties 3)))
                      links)))))))
    (seq-sort-by #'pos-link-offset #'< links)))

(defun pos-links-in-file (file &optional ids)
  "Return the path links in FILE, an Org or Markdown file, else nil.
Each is a `pos-link'.  With IDS, as `pos-links-ids' gives it, an Org
file's id links are among them, each leading to the file that holds
its ID."
  (let ((org (string-suffix-p ".org" file)) (md (string-suffix-p ".md" file)))
    (when (or org md)
      (with-temp-buffer
        (insert (decode-coding-string (pos-bytes-read file) 'utf-8))
        ;; Read this file alone: no #+SETUPFILE, which may name another
        ;; file or a URL, and no mode hooks.
        (cl-letf (((symbol-function 'org-file-contents) (lambda (&rest _) "")))
          (let ((org-mode-hook nil) (markdown-mode-hook nil))
            (if org (org-mode) (markdown-mode))))
        (if org (pos-links--org ids) (pos-links--markdown))))))

;;;; IDs

(defun pos-links--anchor ()
  "Return the search option that names where point's ID property is held.
\"\" for a file's own ID, before its first heading.  For a heading's,
:: and # with its CUSTOM_ID if it has one, else :: and * with its
title."
  (if (org-before-first-heading-p)
      ""
    (let ((custom (org-entry-get nil "CUSTOM_ID")))
      (if custom
          (concat "::#" custom)
        (concat "::*" (substring-no-properties (org-get-heading t t t t)))))))

(defun pos-links-ids (files)
  "Return a hash table from each Org ID in FILES to where it is held.
An ID is the value of an ID property, of a file or of a heading.  Each
place is (FILE . ANCHOR), ANCHOR as `pos-links--anchor' gives it."
  (let ((ids (make-hash-table :test #'equal))
        (property "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$"))
    (dolist (file files)
      (let ((text (decode-coding-string (pos-bytes-read file) 'utf-8)))
        (when (string-match-p property text)
          (with-temp-buffer
            (insert text)
            ;; This file alone: no #+SETUPFILE and no mode hooks.
            (cl-letf (((symbol-function 'org-file-contents) (lambda (&rest _) "")))
              (let ((org-mode-hook nil)) (org-mode)))
            (goto-char (point-min))
            (while (re-search-forward property nil t)
              (let ((id (match-string-no-properties 1)))
                (cl-pushnew (cons file (save-excursion (pos-links--anchor)))
                            (gethash id ids) :test #'equal)))))))
    ids))

(defun pos-links-org-files (dir)
  "Return the Org files beneath DIR that a link by ID may lead to.
Archives, attics, node_modules, and hidden and underscore directories
are not looked in, nor is a symbolic link followed.  A hidden file and
a lock file are not among them."
  (directory-files-recursively
   dir "\\`[^.#].*\\.org\\'" nil
   (lambda (sub)
     (let ((name (file-name-nondirectory sub)))
       (not (or (member name '("archives" "attic" "node_modules"))
                (string-match-p "\\`[._]" name)
                (file-symlink-p sub)))))))

(defun pos-links-annotate (file link label)
  "Return the annotation replacing LINK's whole in FILE, marked LABEL.
LABEL is broken or later.  In Org, the link's type becomes LABEL; in
Markdown, the link becomes its text and a bracketed note."
  (pcase-let (((cl-struct pos-link text whole description) link))
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
          (pos-ledger-refuse 'plan "Link not where it was planned, at byte %d" offset))
        (setq out (concat (substring out 0 offset) to
                          (substring out (+ offset (length from)))))))
    out))

;;;; Resolving

(cl-defstruct (pos-resolution (:constructor pos-resolution-create) (:copier nil))
  "What becomes of LINK, a `pos-link' in FILE, when its item is sealed.
KIND is internal, cid, rumour, broken or later, and TO the text written
in the link's place.  TO is nil while the CID it cites is not known:
then TARGET is the sealed path the link leads to, or RUMOUR the
destination of the rumour that stands for it, and SUFFIX what follows
the CID."
  file link kind to target rumour suffix)

(defun pos-resolution--whole-p (resolution)
  "Return non-nil if the whole of RESOLUTION's link is replaced.
An annotation, broken or later, does; any other replaces the target."
  (member (pos-resolution-kind resolution) '("broken" "later")))

(defun pos-resolution-offset (resolution)
  "Return the byte offset of the text replaced by RESOLUTION."
  (let ((link (pos-resolution-link resolution)))
    (if (pos-resolution--whole-p resolution)
        (pos-link-whole-offset link)
      (pos-link-offset link))))

(defun pos-resolution-from (resolution)
  "Return the text replaced by RESOLUTION."
  (let ((link (pos-resolution-link resolution)))
    (if (pos-resolution--whole-p resolution)
        (pos-link-whole link)
      (pos-link-text link))))

(defun pos-resolution-rewrite (resolution)
  "Return RESOLUTION as (OFFSET FROM TO), for `pos-links-rewrite'."
  (list (pos-resolution-offset resolution) (pos-resolution-from resolution)
        (pos-resolution-to resolution)))

(defun pos-resolution-alist (resolution)
  "Return RESOLUTION as a plan records it, an alist."
  `((file . ,(pos-resolution-file resolution))
    (offset . ,(pos-resolution-offset resolution))
    (from . ,(pos-resolution-from resolution))
    (kind . ,(pos-resolution-kind resolution))
    (to . ,(pos-resolution-to resolution))))

(defun pos-links-archive-of (file)
  "Return the outermost archive above FILE, or nil."
  (let (found (dir (file-name-directory (directory-file-name file))))
    (while (and dir (not (equal dir (file-name-directory (directory-file-name dir)))))
      (when (equal (file-name-nondirectory (directory-file-name dir)) "archives")
        (setq found (directory-file-name dir)))
      (setq dir (file-name-directory (directory-file-name dir))))
    found))

(defun pos-links--sealed (target)
  "Return the ipfs:// link to archived TARGET, as sealed, or refuse."
  (let* ((archive (pos-links-archive-of target))
         (rel (file-relative-name target archive)))
    (pcase-let (((cl-struct pos-history entries collections items)
                 (pos-ledger-history archive)))
      (let ((item (or (seq-find (lambda (i) (pos-path-within-p rel i)) items)
                      (seq-find (lambda (c) (pos-path-within-p rel c)) collections)
                      (and (assoc rel entries) rel))))
        (unless item
          (pos-ledger-refuse 'unsealed "Link to an archived path not sealed: %s" target))
        (let ((path (expand-file-name item archive)))
          (concat "ipfs://"
                  (cond ((pos-ledger-kept archive)
                         (cdr (assoc item (pos-ledger-fold-cids archive))))
                        ((file-directory-p path) (pos-cid-directory path))
                        (t (pos-cid-file path)))
                  (unless (equal rel item) (concat "/" (substring rel (1+ (length item)))))))))))

(defun pos-links--kept-p (target)
  "Return non-nil if TARGET is enrolled in an archive kept by a keeper.
Nothing is on disk there, and TARGET is a path its ledger folds to."
  (let ((archive (pos-links-archive-of target)))
    (and archive (pos-ledger-kept archive)
         (assoc (file-relative-name target archive) (pos-ledger-fold-cids archive))
         t)))

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
   (t (let ((bytes (pos-bytes-read target))
            (title (pos-links--title target)))
        (format "a file of %d bytes, SHA-256 =%s=%s" (length bytes) (pos-bytes-sha bytes)
                (if title (format ", titled \"%s\"" title) ""))))))

(defun pos-links--title (file)
  "Return FILE's title, from an Org #+TITLE or a Markdown heading, or nil."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert (decode-coding-string (pos-bytes-read file) 'utf-8))
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
  (let ((archive (pos-links-archive-of target)))
    (cond
     ((not (or (file-exists-p target) (file-symlink-p target)
               (pos-links--kept-p target)))
      (cons 'broken target))
     ((and archive (or (null scope) (pos-links-within-scope-p archive scope)))
      (cons 'cid (pos-links--sealed target)))
     (t (cons 'rumour target)))))

(defun pos-links-link (path)
  "Return the ipfs:// link to PATH, a sealed path in an archive, or refuse.
The link is the one a sealed item's link to PATH is rewritten to."
  (let ((target (directory-file-name (expand-file-name path))))
    (unless (pos-links-archive-of target)
      (pos-ledger-refuse 'unsealed "Not a path in an archive: %s" target))
    (pos-links--sealed target)))

(provide 'pos-links)
;;; pos-links.el ends here
