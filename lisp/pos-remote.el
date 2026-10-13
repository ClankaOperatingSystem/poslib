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
;; may be kept by a keeper, reached at a URL.  Seven operations pass
;; between them, and they are generic functions, so that what keeps an
;; archive can be HTTP, a test's tape or another transport:
;;
;; - `pos-remote-describe': what the keeper holds of the ledger, and the
;;   versions of the protocol it serves.
;; - `pos-remote-event': an event's bytes, by number.
;; - `pos-remote-held': which of some blocks the keeper lacks (version 2).
;; - `pos-remote-put': one block, by its CID (version 2).
;; - `pos-remote-append': an event and the client's claims; under version
;;   1, the files it enrols travel with it.
;; - `pos-remote-read': the bytes under a CID, or at a path beneath it.
;; - `pos-remote-search': where a query is found, as references with a
;;   range and the passage there, from a keeper that searches.
;;
;; `pos-remote-version' is the version a client speaks to a keeper: the
;; highest it lists that the client knows.  `pos-remote-following' is
;; what a client sends after an event that names no ledger, so that a
;; keeper can tell whose it is.
;;
;; `pos-remote-http' is a keeper reached over HTTP, each exchange made
;; by pos-http.el.  A refusal signals `pos-ledger-refused' with the kind
;; the keeper names.  Nothing here seals: `pos-seal' calls it for an
;; archive a keeper keeps.  Loading this module gives pos-ledger.el
;; `pos-remote-ask', so that a check of a kept archive asks its keeper.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'pos-bytes)
(require 'pos-http)
(require 'pos-ledger)
(require 'pos-signin)

;;;; The port

(cl-defgeneric pos-remote-describe (archive)
  "Return what the keeper of ARCHIVE has of its ledger.
An alist: protocol, protocols and any retiring, ledger_id, head, events,
root, erased, and search for a keeper that searches.")

(cl-defgeneric pos-remote-held (archive cids)
  "Return the CIDS, a list, that the keeper of ARCHIVE has no block for.
In their order.  Version 2.")

(cl-defgeneric pos-remote-put (archive cid bytes)
  "Have the keeper of ARCHIVE hold the block CID names, BYTES.
Return non-nil if it was not held before.  Version 2.")

(cl-defgeneric pos-remote-event (archive number)
  "Return the bytes of event NUMBER of ARCHIVE's ledger, as its file has them.")

(cl-defgeneric pos-remote-append (archive name event files claims &optional following)
  "Append to ARCHIVE the event of bytes EVENT, which the ledger file NAME is.
FILES is an alist of CID and bytes: the files it or earlier events
enrol, which travel with the event under version 1 and are nil under
version 2, their blocks put first.  CLAIMS is an alist of what the
client says of itself, strings by name.  FOLLOWING is the events after
it, an alist of name and bytes in order, by which an event that names
no ledger is known for this ledger's (`pos-remote-following').  Return
the description after it, as `pos-remote-describe'.  The same event
again at the same number changes nothing.")

(cl-defgeneric pos-remote-read (archive cid &optional path)
  "Return the bytes ARCHIVE's ledger enrols under CID.
A file or an event; or with PATH, the file at that path beneath a
directory.")

(cl-defgeneric pos-remote-search (archive query &optional mode limit within)
  "Return where QUERY is found in what ARCHIVE's ledger enrols.
MODE is one the keeper lists under search in `pos-remote-describe',
literal where nil.  LIMIT is the most hits wanted, or nil for the
keeper's cap.  WITHIN is a CID the ledger enrols, to search beneath it
alone, or nil for everything.  Return an alist whose hits is a vector
of hits, each an alist: ref, an ipfs:// link; range; passage, the text
there; and in a ranked mode score.  A keeper that does not search
refuses as `absent'; a mode it does not serve, as `mode'.")

;;;; Which version

(defconst pos-remote-versions '(1 2)
  "The versions of the protocol this client speaks, ascending.")

(defun pos-remote-version (described)
  "Return the version to speak to the keeper DESCRIBED.
DESCRIBED is as `pos-remote-describe' gives it.  The highest of
`pos-remote-versions' it lists under protocols, or 1 where it lists
none, as a keeper of version 1 alone does.  Refused as
`version' where none is shared."
  (let* ((offered (let ((listed (alist-get 'protocols described)))
                    (if (and listed (not (eq listed :null)))
                        (append listed nil)
                      (list (or (alist-get 'protocol described) 1)))))
         (shared (seq-filter (lambda (v) (memq v offered)) pos-remote-versions)))
    (unless shared
      (pos-ledger-refuse 'version "The keeper serves protocol versions %S; this client speaks %S"
                         offered pos-remote-versions))
    (apply #'max shared)))

;;;; Whose an event is

(defun pos-remote--named (events)
  "Return the ledger_id EVENTS name, or nil.
EVENTS is an alist of name and bytes, in order.  It is the ledger_id of
the last of them that has one: in a valid chain every event that has
one has the same, and none follows one that has."
  (seq-some (lambda (event)
              (let ((id (alist-get 'ledger_id (pos-bytes-parse (cdr event)))))
                (and (stringp id) id)))
            (reverse events)))

(defun pos-remote-following (events number)
  "Return what a client sends after event NUMBER of EVENTS.
EVENTS is a ledger's events, an alist of name and bytes in order, and
what is returned is some of them, by which a keeper can tell whose the
event is.

An event's ledger is named by the event or by one before it.  A ledger
begun before events carried a ledger_id has first events that name
none, and for one of those the client sends the events after it, up to
and including the first that does name the ledger: each event's
previous is the hash of the one before, so that event vouches for all
before it.  For any other event this is nil; and it is nil where no
event names a ledger, since nothing then vouches."
  (unless (pos-remote--named (seq-take events number))
    (let ((rest (seq-drop events number))
          found)
      (while (and rest (not found))
        (when (pos-remote--named (list (car rest)))
          (setq found t))
        (setq rest (cdr rest)))
      (when found
        (seq-subseq events number (- (length events) (length rest)))))))

;;;; The wire

(cl-defstruct (pos-remote-http (:constructor pos-remote-http-create) (:copier nil))
  "A keeper reached over HTTP: the ledger's base URL, and a bearer token or nil."
  url token)

(defconst pos-remote--kinds
  '((401 . access) (403 . access) (404 . absent) (409 . chain) (410 . erased)
    (413 . size))
  "The kind of refusal a status is, when its body does not say.")

(defvar pos-remote-keeper-function
  (lambda (url) (pos-remote-http-create :url url :token (getenv "POS_ARCHIVE_TOKEN")))
  "The function a keeper's URL is made a keeper with.
What it returns answers `pos-remote-describe', `pos-remote-event',
`pos-remote-append' and `pos-remote-read'.  By default the keeper is
reached over HTTP, with the bearer token in the environment's
POS_ARCHIVE_TOKEN, else the one kept for it by signing in.")

(defun pos-remote--multipart (parts)
  "Return (BODY . CONTENT-TYPE) for PARTS, each (NAME . BYTES), written one way.
The boundary is taken from the bytes, so equal parts are equal bodies."
  (let* ((parts (mapcar (lambda (part)
                          (cons (encode-coding-string (car part) 'utf-8)
                                (string-to-unibyte (cdr part))))
                        parts))
         (boundary (concat "pos-" (pos-bytes-sha (mapconcat #'cdr parts "")))))
    (cons (string-to-unibyte
           (concat (mapconcat
                    (lambda (part)
                      (concat "--" boundary "\r\nContent-Disposition: form-data; name=\""
                              (car part) "\"\r\n\r\n" (cdr part) "\r\n"))
                    parts "")
                   "--" boundary "--\r\n"))
          (concat "multipart/form-data; boundary=" boundary))))

(defun pos-remote--call-status (archive method path &optional body content-type)
  "Return (STATUS . BODY) that ARCHIVE's keeper answers METHOD on PATH with.
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
            (funcall pos-http-send-function method (concat url path)
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
          answer
        (let ((named (ignore-errors
                       (alist-get 'refused (pos-bytes-parse (cdr answer))))))
          (if (and (eql status 401) (null given))
              (pos-ledger-refuse
               'access "Not signed in to this keeper; sign in with: sign-in %s" url)
            (pos-ledger-refuse (cond ((stringp named) (intern named))
                                     ((alist-get status pos-remote--kinds))
                                     (t 'remote))
                               "%s %s answered %d" method path status)))))))

(defun pos-remote--call (archive method path &optional body content-type)
  "Return the body ARCHIVE's keeper answers METHOD on PATH with.
BODY and CONTENT-TYPE are as `pos-remote--call-status' takes them; this
is that, without the status."
  (cdr (pos-remote--call-status archive method path body content-type)))

(cl-defmethod pos-remote-describe ((archive pos-remote-http))
  "Return what the keeper of ARCHIVE has of its ledger, over HTTP."
  (pos-bytes-parse (pos-remote--call archive "GET" "/")))

(cl-defmethod pos-remote-held ((archive pos-remote-http) cids)
  "Return which of CIDS the keeper of ARCHIVE lacks, over HTTP."
  (append (alist-get 'missing
                     (pos-bytes-parse
                      (pos-remote--call archive "POST" "/blocks"
                                        (pos-bytes-json (vconcat cids))
                                        "application/json")))
          nil))

(cl-defmethod pos-remote-put ((archive pos-remote-http) cid bytes)
  "Have the keeper of ARCHIVE hold the block CID names, BYTES, over HTTP.
Non-nil if the keeper answered 201: it was not held before."
  (eql 201 (car (pos-remote--call-status archive "PUT" (concat "/blocks/" cid)
                                         (string-to-unibyte bytes) "application/octet-stream"))))

(cl-defmethod pos-remote-event ((archive pos-remote-http) number)
  "Return the bytes of event NUMBER of ARCHIVE's ledger, over HTTP."
  (pos-remote--call archive "GET" (format "/events/%d" number)))

(cl-defmethod pos-remote-append ((archive pos-remote-http) name event files claims
                                 &optional following)
  "Append to ARCHIVE the event EVENT named NAME, with FILES and CLAIMS, over HTTP.
FOLLOWING, the events after it, goes after the claims and before the
files; under version 2 FILES is nil and nothing follows the later events."
  (let ((sent (pos-remote--multipart
               (append (list (cons "name" (encode-coding-string name 'utf-8))
                             (cons "event" event)
                             (cons "claims" (pos-bytes-json claims)))
                       following
                       (seq-sort-by #'car #'string< files)))))
    (pos-bytes-parse
     (pos-remote--call archive "POST" "/events" (car sent) (cdr sent)))))

(cl-defmethod pos-remote-read ((archive pos-remote-http) cid &optional path)
  "Return the bytes ARCHIVE's ledger enrols under CID, or at PATH, over HTTP."
  (pos-remote--call
   archive "GET"
   (concat "/ipfs/" cid
           (unless (member path '(nil ""))
             (concat "/" (mapconcat #'url-hexify-string (split-string path "/") "/"))))))

(cl-defmethod pos-remote-search ((archive pos-remote-http) query &optional mode limit within)
  "Return where QUERY is found in ARCHIVE, over HTTP.
MODE, LIMIT and WITHIN go as query parameters after q, in that order,
each left out where nil."
  (pos-bytes-parse
   (pos-remote--call
    archive "GET"
    (concat "/search?"
            (mapconcat (lambda (parameter)
                         (concat (car parameter) "=" (url-hexify-string (cdr parameter))))
                       (seq-filter #'cdr
                                   `(("q" . ,query) ("mode" . ,mode)
                                     ("limit" . ,(and limit (number-to-string limit)))
                                     ("within" . ,within)))
                       "&")))))

(defun pos-remote-ask (url)
  "Return what the keeper at URL has of its ledger, as `pos-remote-describe' does.
The keeper is the one `pos-remote-keeper-function' makes of URL."
  (pos-remote-describe (funcall pos-remote-keeper-function url)))

;; A ledger asks no keeper itself; this is the client it asks with.
(setq pos-ledger-ask-function #'pos-remote-ask)

(provide 'pos-remote)
;;; pos-remote.el ends here
