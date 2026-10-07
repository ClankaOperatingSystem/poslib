;;; pos-signin.el --- Signing in to a keeper -*- lexical-binding: t; -*-

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

;; Signing in to a keeper, and the tokens kept, as section 8 of
;; doc/remote-archive-protocol.txt specifies.
;;
;; A keeper that takes tokens from an OAuth 2.0 authorization server
;; says how a person gets one: the issuer, the scopes to ask for and the
;; public client to sign in as.  Signing in is the authorization code
;; flow with PKCE to a loopback address, and happens when a person asks
;; for it and at no other time.  What it yields is kept in one file,
;; which pyposlib reads too.
;;
;; - `pos-signin': sign a person in to the keeper at a URL.
;; - `pos-signin-token': the token a request to a keeper carries.
;; - `pos-signin-adopt': after a refusal, find that a keeper not seen
;;   before takes a token already kept.

;;; Code:

(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'pos-ledger)

(defvar pos-remote-send-function)

(defconst pos-signin-well-known "/.well-known/oauth-protected-resource"
  "The path, at a keeper's origin, of how to sign in to it (RFC 9728).")

(defconst pos-signin-soon 60
  "A token expiring within this many seconds is refreshed first.")

(defvar pos-signin-browse-function #'browse-url
  "The function a person's browser is sent to the issuer with.
Called with the URL to open.")

(defvar pos-signin-fetch-function #'pos-signin--fetch
  "The function an exchange with a keeper or an issuer is made with.
Called with a URL and, for a POST, an alist of form fields; it gives
back (STATUS . BYTES).")

(defun pos-signin--fetch (url &optional form)
  "Return (STATUS . BYTES) for URL: a GET, or with FORM a POST of it."
  (require 'pos-remote)
  (funcall pos-remote-send-function (if form "POST" "GET") url
           (append '(("Accept" . "application/json"))
                   (when form
                     '(("Content-Type" . "application/x-www-form-urlencoded"))))
           (when form
             (encode-coding-string
              (mapconcat (lambda (pair)
                           (concat (url-hexify-string (car pair)) "="
                                   (url-hexify-string (cdr pair))))
                         form "&")
              'utf-8))))

;;;; What is kept

(defun pos-signin-file ()
  "Return where the tokens are kept.
The file pos/tokens.json under $XDG_CONFIG_HOME, or under ~/.config."
  (expand-file-name
   "pos/tokens.json"
   (let ((config (getenv "XDG_CONFIG_HOME")))
     (if (member config '(nil "")) (expand-file-name "~/.config") config))))

(defun pos-signin--object (value)
  "Return VALUE, a JSON object, as an alist with string keys, or nil."
  (and (listp value)
       (mapcar (lambda (pair) (cons (pos-ledger--key (car pair)) (cdr pair))) value)))

(defun pos-signin--load ()
  "Return what is kept, as (TOKENS . KEEPERS).
Each an alist by string: TOKENS by \"ISSUER CLIENT\", KEEPERS by origin."
  (let ((kept (ignore-errors
                (pos-ledger-parse (pos-ledger-read (pos-signin-file))))))
    (cons (pos-signin--object (and (listp kept) (alist-get 'tokens kept)))
          (pos-signin--object (and (listp kept) (alist-get 'keepers kept))))))

(defun pos-signin--save (kept)
  "Replace the file whole with KEPT, readable by its owner alone."
  (let* ((file (pos-signin-file))
         (dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (set-file-modes dir #o700))
    (let ((temp (make-temp-file (expand-file-name "tokens-" dir)))
          (coding-system-for-write 'binary))
      (set-file-modes temp #o600)
      (with-temp-file temp
        (set-buffer-multibyte nil)
        (insert (pos-ledger-json `((tokens . ,(car kept)) (keepers . ,(cdr kept))))))
      (set-file-modes temp #o600)
      (rename-file temp file t))))

(defun pos-signin--origin (url)
  "Return the origin of URL: its scheme and host, with a port it names."
  (let* ((parts (url-generic-parse-url url))
         (port (url-portspec parts)))
    (concat (url-type parts) "://" (url-host parts)
            (if port (format ":%d" port) ""))))

;;;; What a keeper and an issuer say

(defun pos-signin--json (answer)
  "Return the JSON object in ANSWER, a (STATUS . BYTES), if it is a 200."
  (and (eql (car answer) 200)
       (ignore-errors (pos-ledger-parse (cdr answer)))))

(defun pos-signin--told (url)
  "Return how to sign in to the keeper at URL, or refuse `access'.
As (ISSUER CLIENT SCOPES), SCOPES a list."
  (let* ((origin (pos-signin--origin url))
         (told (pos-signin--json
                (funcall pos-signin-fetch-function
                         (concat origin pos-signin-well-known))))
         (servers (and (listp told) (alist-get 'authorization_servers told)))
         (issuer (and (vectorp servers) (> (length servers) 0) (aref servers 0)))
         (client (and (listp told) (alist-get 'client_id told)))
         (scopes (and (listp told) (alist-get 'scopes_supported told))))
    (unless (and (stringp issuer) (stringp client) (vectorp scopes)
                 (seq-every-p #'stringp scopes))
      (pos-ledger--refuse 'access "This keeper does not say how to sign in: %s" origin))
    (list issuer client (append scopes nil))))

(defun pos-signin--endpoints (issuer)
  "Return (AUTHORIZATION . TOKEN), ISSUER's endpoints, from its discovery."
  (let* ((told (pos-signin--json
                (funcall pos-signin-fetch-function
                         (concat (string-remove-suffix "/" issuer)
                                 "/.well-known/openid-configuration"))))
         (authorization (and (listp told) (alist-get 'authorization_endpoint told)))
         (token (and (listp told) (alist-get 'token_endpoint told))))
    (unless (and (stringp authorization) (stringp token))
      (pos-ledger--refuse 'remote "The issuer does not say where to sign in: %s" issuer))
    (cons authorization token)))

;;;; Tokens

(defun pos-signin--granted (answer scopes &optional previous)
  "Return a kept token from ANSWER, a token endpoint's, or nil.
SCOPES are what was asked for, and PREVIOUS a refresh token to keep
when the answer gives none."
  (let ((told (pos-signin--json answer)))
    (when (and (listp told) (stringp (alist-get 'access_token told)))
      `((access_token . ,(alist-get 'access_token told))
        (expires_at . ,(+ (truncate (float-time))
                          (let ((seconds (alist-get 'expires_in told)))
                            (if (integerp seconds) seconds 0))))
        (refresh_token . ,(let ((refresh (alist-get 'refresh_token told)))
                            (cond ((stringp refresh) refresh)
                                  (previous)
                                  (t :null))))
        (scopes . ,(vconcat scopes))))))

(defun pos-signin--usable (kept issuer client)
  "Return the access token in KEPT for ISSUER and CLIENT, or nil.
One about to expire is refreshed first, and what is kept rewritten; nil
if none is kept or it cannot be refreshed.  KEPT is changed in place."
  (let* ((key (concat issuer " " client))
         (token (cdr (assoc key (car kept))))
         (refresh (and (listp token) (alist-get 'refresh_token token))))
    (cond
     ((not (and (listp token) (stringp (alist-get 'access_token token)))) nil)
     ((> (- (or (alist-get 'expires_at token) 0) (float-time)) pos-signin-soon)
      (alist-get 'access_token token))
     ((not (stringp refresh)) nil)
     (t
      (let ((renewed
             (condition-case nil
                 (pos-signin--granted
                  (funcall pos-signin-fetch-function
                           (cdr (pos-signin--endpoints issuer))
                           `(("grant_type" . "refresh_token") ("client_id" . ,client)
                             ("refresh_token" . ,refresh)))
                  (append (alist-get 'scopes token) nil) refresh)
               (pos-ledger-refused nil))))
        (when renewed
          (setcar kept (cons (cons key renewed)
                             (seq-remove (lambda (pair) (equal (car pair) key))
                                         (car kept))))
          (pos-signin--save kept)
          (alist-get 'access_token renewed)))))))

(defun pos-signin-token (url)
  "Return the token a request to the keeper at URL carries, or nil.
The one kept for its origin, refreshed if it is about to expire."
  (let* ((kept (pos-signin--load))
         (entry (cdr (assoc (pos-signin--origin url) (cdr kept)))))
    (when (listp entry)
      (let ((issuer (alist-get 'issuer entry)) (client (alist-get 'client_id entry)))
        (and (stringp issuer) (stringp client)
             (pos-signin--usable kept issuer client))))))

(defun pos-signin--record (kept url issuer client)
  "Record in KEPT that the keeper at URL takes ISSUER's token for CLIENT."
  (let ((origin (pos-signin--origin url)))
    (setcdr kept (cons (cons origin `((issuer . ,issuer) (client_id . ,client)))
                       (seq-remove (lambda (pair) (equal (car pair) origin))
                                   (cdr kept))))))

(defun pos-signin-adopt (url)
  "Return a token for the keeper at URL, not seen before, if one is kept.
That is, if one is kept for the issuer and client the keeper names:
signing in to one of a keeper's ledgers serves its others.  The origin
is then recorded.  Nil otherwise."
  (let ((kept (pos-signin--load)))
    (unless (assoc (pos-signin--origin url) (cdr kept))
      (pcase (condition-case nil (pos-signin--told url) (pos-ledger-refused nil))
        (`(,issuer ,client ,_)
         (let ((token (pos-signin--usable kept issuer client)))
           (when token
             (pos-signin--record kept url issuer client)
             (pos-signin--save kept)
             token)))))))

;;;; Signing in

(defun pos-signin--random (bytes)
  "Return BYTES random bytes from the system, as a URL-safe string."
  (base64url-encode-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     ;; Not `insert-file-contents': it reads no part of a device.
     (let ((coding-system-for-read 'binary))
       (unless (eq 0 (call-process "head" nil t nil "-c" (number-to-string bytes)
                                   "/dev/urandom"))
         (error "No random bytes from the system")))
     (unless (= (buffer-size) bytes)
       (error "No random bytes from the system"))
     (buffer-string))
   t))

(defun pos-signin--wait (state timeout browse)
  "Listen on a loopback port and return (CODE . REDIRECT) once it arrives.
BROWSE is called with the redirect address the listener answers at, and
sends the person's browser away.  The code must come back with STATE,
within TIMEOUT seconds; refuse `access' otherwise."
  (let* ((found nil)
         (server
          (make-network-process
           :name "pos-signin" :server t :host "127.0.0.1" :service t
           :family 'ipv4 :coding 'binary :noquery t
           :filter
           (lambda (connection data)
             (let ((seen (concat (process-get connection 'seen) data)))
               (process-put connection 'seen seen)
               (when (string-match "\\`GET \\([^ ?]*\\)\\(?:\\?\\([^ ]*\\)\\)? HTTP/[0-9.]+\r\n"
                                   seen)
                 (let* ((callback (equal (match-string 1 seen) "/callback"))
                        (query (and callback (match-string 2 seen)
                                    (url-parse-query-string (match-string 2 seen))))
                        (body (cond ((not callback) "Nothing here.\n")
                                    ((assoc "code" query)
                                     "Signed in. This window can be closed.\n")
                                    (t "Not signed in. This window can be closed.\n"))))
                   (when callback
                     (setq found (or query '(("error" "no answer")))))
                   (process-send-string
                    connection
                    (format "HTTP/1.1 %s\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                            (if callback "200 OK" "404 Not Found") (length body) body))
                   (delete-process connection)))))))
         (redirect (format "http://localhost:%d/callback"
                           (process-contact server :service)))
         (deadline (+ (float-time) timeout)))
    (unwind-protect
        (progn
          (funcall browse redirect)
          (while (and (not found) (< (float-time) deadline))
            (accept-process-output nil 0.2)))
      (delete-process server))
    (unless found
      (pos-ledger--refuse 'access "Nobody signed in before the wait ran out"))
    (unless (and (equal (cadr (assoc "state" found)) state) (assoc "code" found))
      (pos-ledger--refuse 'access "Signing in was refused: %s"
                          (or (cadr (assoc "error" found))
                              "the answer was not the one asked for")))
    (cons (cadr (assoc "code" found)) redirect)))

(defun pos-signin (url &optional timeout)
  "Sign a person in to the keeper at URL, and keep what that yields.
If a token is already kept for the issuer and client the keeper names,
nothing is opened and the keeper is recorded as taking it.  Otherwise
the person's browser is sent to the issuer and the code it brings back
to a loopback address, within TIMEOUT seconds, 300 by default, is
exchanged for a token.  Return an alist: keeper, issuer, client_id,
expires_at and opened, whether a browser was."
  (pcase-let* ((`(,issuer ,client ,scopes) (pos-signin--told url))
               (kept (pos-signin--load))
               (key (concat issuer " " client))
               (opened nil))
    (unless (pos-signin--usable kept issuer client)
      (pcase-let* ((`(,authorization . ,token-endpoint) (pos-signin--endpoints issuer))
                   (asked (append scopes (unless (member "offline_access" scopes)
                                           '("offline_access"))))
                   (verifier (pos-signin--random 48))
                   (state (pos-signin--random 16))
                   (`(,code . ,redirect)
                    (pos-signin--wait
                     state (or timeout 300)
                     (lambda (redirect)
                       (setq opened t)
                       (funcall
                        pos-signin-browse-function
                        (concat authorization
                                (if (string-search "?" authorization) "&" "?")
                                (url-build-query-string
                                 `(("response_type" "code") ("client_id" ,client)
                                   ("redirect_uri" ,redirect)
                                   ("scope" ,(string-join asked " "))
                                   ("state" ,state)
                                   ("code_challenge"
                                    ,(base64url-encode-string
                                      (secure-hash 'sha256 verifier nil nil t) t))
                                   ("code_challenge_method" "S256"))))))))
                   (granted (pos-signin--granted
                             (funcall pos-signin-fetch-function token-endpoint
                                      `(("grant_type" . "authorization_code")
                                        ("code" . ,code) ("redirect_uri" . ,redirect)
                                        ("client_id" . ,client)
                                        ("code_verifier" . ,verifier)))
                             asked)))
        (unless granted
          (pos-ledger--refuse 'access "The issuer gave no token for the code: %s" issuer))
        (setq kept (pos-signin--load))
        (setcar kept (cons (cons key granted)
                           (seq-remove (lambda (pair) (equal (car pair) key))
                                       (car kept))))))
    (pos-signin--record kept url issuer client)
    (pos-signin--save kept)
    `((keeper . ,(pos-signin--origin url)) (issuer . ,issuer) (client_id . ,client)
      (expires_at . ,(alist-get 'expires_at (cdr (assoc key (car kept)))))
      (opened . ,(if opened t :false)))))

(provide 'pos-signin)
;;; pos-signin.el ends here
