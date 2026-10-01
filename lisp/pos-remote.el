;;; pos-remote.el --- An archive kept away from its ledger -*- lexical-binding: t; -*-

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

;; The client's side of doc/remote-archive-protocol.txt, in lockstep
;; with pyposlib.  A ledger stays with its scope; the archive it enrols
;; may be kept by a keeper, reached at a URL.  Four operations pass
;; between them, and they are generic functions, so that what keeps an
;; archive can be HTTP, a test's tape or another transport:
;;
;; - `pos-remote-describe': what the keeper holds of the ledger.
;; - `pos-remote-event': an event's bytes, by number.
;; - `pos-remote-append': an event, with files and the client's claims.
;; - `pos-remote-read': the bytes under a CID, or at a path beneath it.
;;
;; `pos-remote-http' is the protocol's wire.  A refusal signals
;; `pos-ledger-refused' with the kind the keeper names.  Nothing here
;; seals: `pos-seal' does not yet call it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'pos-ledger)
(require 'pos-signin)

(defvar url-http-end-of-headers)

;;;; The port

(cl-defgeneric pos-remote-describe (archive)
  "Return what the keeper of ARCHIVE has of its ledger.
An alist: protocol, ledger_id, head, events, root and erased.")

(cl-defgeneric pos-remote-event (archive number)
  "Return the bytes of event NUMBER of ARCHIVE's ledger, as its file has them.")

(cl-defgeneric pos-remote-append (archive name event files claims)
  "Append to ARCHIVE the event of bytes EVENT, which the ledger file NAME is.
FILES is an alist of CID and bytes: the files it or earlier events
enrol.  CLAIMS is an alist of what the client says of itself, strings
by name.  Return the description after it, as `pos-remote-describe'.
The same event again at the same number changes nothing.")

(cl-defgeneric pos-remote-read (archive cid &optional path)
  "Return the bytes ARCHIVE's ledger enrols under CID.
A file or an event; or with PATH, the file at that path beneath a
directory.")

;;;; The wire

(cl-defstruct (pos-remote-http (:constructor pos-remote-http-create) (:copier nil))
  "A keeper reached over HTTP: the ledger's base URL, and a bearer token or nil."
  url token)

(defconst pos-remote--kinds
  '((401 . access) (403 . access) (404 . absent) (409 . chain) (410 . erased)
    (413 . size))
  "The kind of refusal a status is, when its body does not say.")

(defvar pos-remote-timeout 300
  "Seconds an exchange with a keeper may take before it is given up.
An exchange given up is refused as `remote'.  Nil waits without end.")

(defun pos-remote--send (method url headers body)
  "Make one HTTP exchange: METHOD on URL with HEADERS, an alist, and BODY.
BODY is a unibyte string or nil.  Return (STATUS . BYTES).  A keeper
that has not answered in `pos-remote-timeout' seconds is refused."
  (let* ((url-request-method method)
         ;; url.el joins these to the body, and refuses the request if
         ;; that makes multibyte text of the body's bytes.
         (url (encode-coding-string url 'utf-8))
         (url-request-extra-headers
          (mapcar (lambda (header)
                    (cons (car header) (encode-coding-string (cdr header) 'utf-8)))
                  headers))
         (url-request-data body)
         (url-mime-encoding-string "identity")
         (url-show-status nil)
         (buffer (condition-case err
                     ;; Left to itself url.el answers a 401's challenge:
                     ;; it asks at the terminal for a name and password,
                     ;; and for a scheme it does not know, Bearer among
                     ;; them, it never finishes.  The refusal is the
                     ;; caller's to read.  url-http is required above
                     ;; because loading it here would undo this.
                     (cl-letf (((symbol-function 'url-http-handle-authentication)
                                (lambda (_proxy) t)))
                       (url-retrieve-synchronously url t t pos-remote-timeout))
                   (error (pos-ledger--refuse 'remote "%s: %s" url
                                              (error-message-string err))))))
    (unless buffer
      (pos-ledger--refuse 'remote "%s: no answer" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                       (bound-and-true-p url-http-end-of-headers))
            (pos-ledger--refuse 'remote "%s: no answer" url))
          (let ((status (string-to-number (match-string 1))))
            (goto-char url-http-end-of-headers)
            (forward-line 1)
            (cons status (encode-coding-string
                          (buffer-substring-no-properties (point) (point-max))
                          'binary))))
      (kill-buffer buffer))))

(defvar pos-remote-keeper-function
  (lambda (url) (pos-remote-http-create :url url :token (getenv "POS_ARCHIVE_TOKEN")))
  "The function a keeper's URL is made a keeper with.
What it returns answers `pos-remote-describe', `pos-remote-event',
`pos-remote-append' and `pos-remote-read'.  By default the keeper is
reached over HTTP, with the bearer token in the environment's
POS_ARCHIVE_TOKEN, else the one kept for it by signing in.")

(defvar pos-remote-send-function #'pos-remote--send
  "The function an exchange of `pos-remote-http' is made with.
Called with a method, a URL, an alist of headers and a body, a unibyte
string or nil; it gives back (STATUS . BYTES).")

(defun pos-remote--multipart (parts)
  "Return (BODY . CONTENT-TYPE) for PARTS, each (NAME . BYTES), written one way.
The boundary is taken from the bytes, so equal parts are equal bodies."
  (let* ((parts (mapcar (lambda (part)
                          (cons (encode-coding-string (car part) 'utf-8)
                                (string-to-unibyte (cdr part))))
                        parts))
         (boundary (concat "pos-" (pos-ledger--sha (mapconcat #'cdr parts "")))))
    (cons (string-to-unibyte
           (concat (mapconcat
                    (lambda (part)
                      (concat "--" boundary "\r\nContent-Disposition: form-data; name=\""
                              (car part) "\"\r\n\r\n" (cdr part) "\r\n"))
                    parts "")
                   "--" boundary "--\r\n"))
          (concat "multipart/form-data; boundary=" boundary))))

(defun pos-remote--call (archive method path &optional body content-type)
  "Return the body ARCHIVE's keeper answers METHOD on PATH with.
BODY, with its CONTENT-TYPE, is sent if given.  Any answer but 200 or
201 is refused, by the kind its body names or its status means.
Given no token, the request carries the one kept for the keeper by
signing in, if there is one; and a keeper not seen before, refusing it,
is asked once more with a token already kept that it takes."
  (let* ((url (string-remove-suffix "/" (pos-remote-http-url archive)))
         (given (let ((token (pos-remote-http-token archive)))
                  (unless (member token '(nil "")) token)))
         (exchange
          (lambda (token)
            (funcall pos-remote-send-function method (concat url path)
                     (append (when token
                               `(("Authorization" . ,(concat "Bearer " token))))
                             (when content-type
                               `(("Content-Type" . ,content-type))))
                     body)))
         (answer (funcall exchange (or given (pos-signin-token url)))))
    (when (and (eql (car answer) 401) (null given))
      (let ((adopted (pos-signin-adopt url)))
        (when adopted
          (setq answer (funcall exchange adopted)))))
    (let ((status (car answer)))
      (if (memq status '(200 201))
          (cdr answer)
        (let ((named (ignore-errors
                       (alist-get 'refused (pos-ledger--parse (cdr answer))))))
          (if (and (eql status 401) (null given))
              (pos-ledger--refuse
               'access "Not signed in to this keeper; sign in with: sign-in %s" url)
            (pos-ledger--refuse (cond ((stringp named) (intern named))
                                      ((alist-get status pos-remote--kinds))
                                      (t 'remote))
                                "%s %s answered %d" method path status)))))))

(cl-defmethod pos-remote-describe ((archive pos-remote-http))
  "Return what the keeper of ARCHIVE has of its ledger, over HTTP."
  (pos-ledger--parse (pos-remote--call archive "GET" "/")))

(cl-defmethod pos-remote-event ((archive pos-remote-http) number)
  "Return the bytes of event NUMBER of ARCHIVE's ledger, over HTTP."
  (pos-remote--call archive "GET" (format "/events/%d" number)))

(cl-defmethod pos-remote-append ((archive pos-remote-http) name event files claims)
  "Append to ARCHIVE the event EVENT named NAME, with FILES and CLAIMS, over HTTP."
  (let ((sent (pos-remote--multipart
               (append (list (cons "name" (encode-coding-string name 'utf-8))
                             (cons "event" event)
                             (cons "claims" (pos-ledger-json claims)))
                       (sort (copy-sequence files)
                             (lambda (a b) (string< (car a) (car b))))))))
    (pos-ledger--parse
     (pos-remote--call archive "POST" "/events" (car sent) (cdr sent)))))

(cl-defmethod pos-remote-read ((archive pos-remote-http) cid &optional path)
  "Return the bytes ARCHIVE's ledger enrols under CID, or at PATH, over HTTP."
  (pos-remote--call
   archive "GET"
   (concat "/ipfs/" cid
           (unless (member path '(nil ""))
             (concat "/" (mapconcat #'url-hexify-string (split-string path "/") "/"))))))

(provide 'pos-remote)
;;; pos-remote.el ends here
