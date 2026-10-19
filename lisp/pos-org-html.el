;;; pos-org-html.el --- Org files as HTML for a viewer of the tree -*- lexical-binding: t; -*-

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

;; Org files under a root as HTML, for a viewer of the tree that gives
;; each Org file a page: the body of each, as ox-html exports it, and
;; its title.  The links are the viewer's addresses:
;;
;; - a file: link to an Org file under the root is the address of that
;;   file's page, `pos-org-html-file-url', with ::*HEADING or
;;   ::#CUSTOM-ID as the anchor of that heading;
;; - an id: link is the address `pos-org-html-id-url' gives the ID,
;;   which the viewer finds in the tree data, pos-tree-data.el;
;; - a link to a heading or target in the same file is its anchor;
;; - a link with a scheme a browser follows is itself;
;; - any other link is its description, or its path, as text.
;;
;; Each heading's anchor is its CUSTOM_ID, else a slug of its title,
;; `pos-org-html-slug', so that a link from another file can name it;
;; a heading with an ID has the ID as a second anchor.  A file's HTML depends on no other file, so a viewer need
;; export only the files that change.  Nothing is evaluated, no file is
;; written and no network is used.
;;
;; - `pos-org-html': the HTML and title of each of some files.
;; - `pos-org-html-batch': shell entry; prints them as JSON.
;; - `pos-org-html-slug': the anchor of a heading with no ID.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox-html)
(require 'subr-x)
(require 'url-util)
(require 'pos)

(defvar pos-org-html-file-url "/file/%s"
  "The address of an Org file's page: a format of its path from the root.")

(defvar pos-org-html-id-url "/id/%s"
  "The address of an Org ID: a format of the ID.")

(defconst pos-org-html-schemes '("http" "https" "mailto" "ftp" "ipfs" "ipns")
  "The schemes of the links a browser follows, kept as they are.")

(defvar pos-org-html--root nil
  "The root of the files being exported, a directory name.")

(defvar pos-org-html--file nil
  "The file being exported, absolute.")

;;;; Anchors

(defun pos-org-html-slug (title)
  "Return the anchor of a heading with TITLE and no ID.
TITLE downcased, with each run of characters other than letters and
digits made one hyphen, and none at either end."
  (string-trim (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title))
               "-" "-"))

(defun pos-org-html--anchor (search)
  "Return the anchor a file: link's SEARCH option names, or nil.
*HEADING names the heading with that title, #CUSTOM-ID the heading
with that CUSTOM_ID."
  (cond ((null search) nil)
        ((string-prefix-p "*" search) (pos-org-html-slug (substring search 1)))
        ((string-prefix-p "#" search) (substring search 1))))

(defun pos-org-html--set-anchors ()
  "Give each heading of this buffer with no CUSTOM_ID one, its anchor.
The slug of its title; a slug already given has -2, -3 and so on
added."
  (let ((given (make-hash-table :test #'equal)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-heading-regexp nil t)
       (unless (org-entry-get nil "CUSTOM_ID")
         (let* ((base (pos-org-html-slug (or (org-get-heading t t t t) "")))
                (anchor base)
                (n 1))
           (while (gethash anchor given)
             (setq n (1+ n) anchor (format "%s-%d" base n)))
           (unless (string-empty-p anchor)
             (puthash anchor t given)
             (org-entry-put nil "CUSTOM_ID" anchor))))))))

(defun pos-org-html--headline (headline contents info)
  "Transcode HEADLINE, with CONTENTS, as ox-html does, with its ID's anchor.
INFO is the export's."
  (let ((html (org-html-headline headline contents info))
        (id (org-element-property :ID headline)))
    (if (and id (string-match "<h[0-9]+ id=\"[^\"]*\">" html))
        (concat (substring html 0 (match-end 0))
                (format "<a id=\"%s\"></a>" (org-html-encode-plain-text id))
                (substring html (match-end 0)))
      html)))

;;;; Links

(defun pos-org-html--target (path)
  "Return PATH, a file: link's, from the root, or nil.
Nil unless it names an Org file under the root.  PATH is relative to
the directory of the file being exported."
  (let ((file (expand-file-name path (file-name-directory pos-org-html--file))))
    (and (string-suffix-p ".org" file)
         (string-prefix-p pos-org-html--root file)
         (file-relative-name file pos-org-html--root))))

(defun pos-org-html--href (href text)
  "Return a link to HREF with TEXT, which is HTML already."
  (format "<a href=\"%s\">%s</a>" (org-html-encode-plain-text href) text))

(defun pos-org-html--internal (link info)
  "Return the anchor of the heading or target LINK resolves to, or nil.
LINK is a fuzzy or custom-id link; INFO is the export's."
  (when-let* ((target (ignore-errors
                        (if (equal (org-element-property :type link) "custom-id")
                            (org-export-resolve-id-link link info)
                          (org-export-resolve-fuzzy-link link info)))))
    (pcase (org-element-type target)
      ('headline (org-element-property :CUSTOM_ID target))
      ((or 'target 'radio-target) (org-html--reference target info)))))

(defun pos-org-html--link (link contents info)
  "Transcode LINK, with CONTENTS, to HTML for the viewer.
INFO is the export's."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw (org-element-property :raw-link link))
         (text (or contents (org-html-encode-plain-text raw))))
    ;; A link of a type no library has defined is parsed as fuzzy, so
    ;; the scheme is read from the link as written.
    (when (string-match "\\`\\([[:alpha:]][[:alnum:]+.-]*\\):" raw)
      (when (member (match-string 1 raw) pos-org-html-schemes)
        (setq type "scheme")))
    (pcase type
      ("file"
       (if-let* ((target (pos-org-html--target path)))
           (let ((anchor (pos-org-html--anchor
                          (org-element-property :search-option link))))
             (pos-org-html--href
              (concat (format pos-org-html-file-url (url-hexify-string target url-path-allowed-chars))
                      (if anchor (concat "#" (url-hexify-string anchor)) ""))
              (or contents (org-html-encode-plain-text path))))
         (format "<code>%s</code>" (or contents (org-html-encode-plain-text path)))))
      ("id" (pos-org-html--href (format pos-org-html-id-url (url-hexify-string path))
                                text))
      ((or "fuzzy" "custom-id")
       (if-let* ((anchor (pos-org-html--internal link info)))
           (pos-org-html--href (concat "#" anchor) text)
         text))
      ("scheme" (pos-org-html--href raw text))
      (_ text))))

(org-export-define-derived-backend 'pos-org-html 'html
  :translate-alist '((headline . pos-org-html--headline)
                     (link . pos-org-html--link)))

;;;; Export

(defun pos-org-html--title ()
  "Return this buffer's #+TITLE, or nil."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search t))
     (and (re-search-forward "^#\\+TITLE:[ \t]*\\(.*?\\)[ \t]*$" nil t)
          (match-string-no-properties 1)))))

(defun pos-org-html--file (file)
  "Return FILE's title and the HTML of its body, as (TITLE . HTML).
FILE is absolute.  The file is read into a buffer of its own, whose
headings are given their anchors there, and exported from it."
  (with-temp-buffer
    (insert-file-contents file)
    (setq default-directory (file-name-directory file))
    (let ((org-inhibit-startup t)
          (enable-local-variables nil))
      (delay-mode-hooks (org-mode)))
    (pos-org-html--set-anchors)
    (let ((pos-org-html--file file)
          (title (pos-org-html--title)))
      ;; The export looks up the file of each id: link, through the
      ;; user's index of IDs, which it would read or build.  The links
      ;; lead to the viewer, which finds the IDs itself.
      (cl-letf (((symbol-function 'org-id-find) #'ignore))
        (cons title
              (org-export-as 'pos-org-html nil nil t
                             '(:with-toc nil :section-numbers nil
                               :with-planning t :with-broken-links mark)))))))

(defun pos-org-html (root files)
  "Return the HTML of each of FILES under ROOT, for `json-serialize'.
FILES are paths from ROOT.  A vector, in the order of FILES, of
alists of file, the path from ROOT; title, the file's #+TITLE or
:null; and html, the body as ox-html exports it, with the links
`pos-org-html--link' gives.  A file that cannot be read or exported
has error, a message, in place of title and html."
  (let ((pos-org-html--root (file-name-as-directory (expand-file-name root)))
        (org-export-use-babel nil)
        (org-html-htmlize-output-type nil)
        (vc-handled-backends nil))
    (vconcat
     (mapcar (lambda (path)
               (condition-case err
                   (pcase-let ((`(,title . ,html)
                                (pos-org-html--file
                                 (expand-file-name path pos-org-html--root))))
                     `((file . ,path) (title . ,(or title :null)) (html . ,html)))
                 (error `((file . ,path) (error . ,(error-message-string err))))))
             files))))

(defun pos-org-html-batch ()
  "Print the HTML of the files `command-line-args-left' names, as JSON.
They are paths from `pos-directory'.  --file-url FORMAT and --id-url
FORMAT set `pos-org-html-file-url' and `pos-org-html-id-url'.  Exit 2
with no file."
  (let (files)
    (while command-line-args-left
      (let ((argument (pop command-line-args-left)))
        (pcase argument
          ((and "--file-url" (guard command-line-args-left))
           (setq pos-org-html-file-url (pop command-line-args-left)))
          ((and "--id-url" (guard command-line-args-left))
           (setq pos-org-html-id-url (pop command-line-args-left)))
          (_ (push argument files)))))
    (unless files
      (message "Usage: [--file-url FORMAT] [--id-url FORMAT] FILE ...")
      (kill-emacs 2))
    ;; `json-serialize' gives UTF-8 bytes, which `princ' would print in
    ;; Emacs's internal form, each byte above 127 as two.
    (princ (concat (decode-coding-string
                    (json-serialize (pos-org-html pos-directory (nreverse files)))
                    'utf-8)
                   "\n"))))

(provide 'pos-org-html)
;;; pos-org-html.el ends here
