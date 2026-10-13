;;; pos-http.el --- One HTTP exchange, as a client makes it -*- lexical-binding: t; -*-

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

;; One exchange over HTTP: a method on a URL, with headers and a body,
;; answered by a status and bytes.  pos-remote.el makes a keeper's
;; exchanges with it and pos-signin.el an issuer's.  Nothing of the
;; protocol is here: what is sent and what an answer means are the
;; callers'.  A keeper that does not answer in time, or at all, is
;; refused as `remote'.
;;
;; - `pos-http-send': the exchange.
;; - `pos-http-send-function': the function the exchange is made with,
;;   which a test rebinds to a recording.
;; - `pos-http-timeout': how long an exchange may take.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-http)
(require 'pos-ledger)

(defvar url-http-end-of-headers)

(defvar pos-http-timeout 300
  "Seconds an exchange with a keeper may take before it is given up.
An exchange given up is refused as `remote'.  Nil waits without end.")

(defun pos-http-send (method url headers body)
  "Make one HTTP exchange: METHOD on URL with HEADERS, an alist, and BODY.
BODY is a unibyte string or nil.  Return (STATUS . BYTES).  A keeper
that has not answered in `pos-http-timeout' seconds is refused."
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
                       (url-retrieve-synchronously url t t pos-http-timeout))
                   (error (pos-ledger-refuse 'remote "%s: %s" url
                                             (error-message-string err))))))
    (unless buffer
      (pos-ledger-refuse 'remote "%s: no answer" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                       (bound-and-true-p url-http-end-of-headers))
            (pos-ledger-refuse 'remote "%s: no answer" url))
          (let ((status (string-to-number (match-string 1))))
            (goto-char url-http-end-of-headers)
            (forward-line 1)
            (cons status (encode-coding-string
                          (buffer-substring-no-properties (point) (point-max))
                          'binary))))
      (kill-buffer buffer))))

(defvar pos-http-send-function #'pos-http-send
  "The function an exchange with a keeper or an issuer is made with.
Called with a method, a URL, an alist of headers and a body, a unibyte
string or nil; it gives back (STATUS . BYTES).")

(provide 'pos-http)
;;; pos-http.el ends here
