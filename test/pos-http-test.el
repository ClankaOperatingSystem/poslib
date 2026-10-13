;;; pos-http-test.el --- Tests for pos-http.el  -*- lexical-binding: t -*-

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

;; Run: make test.  Each test is named for the rule it pins, against a
;; server of its own on the loopback address.

;;; Code:

(require 'ert)
(require 'pos-http)

(defun pos-http-test-refusing (challenge)
  "Start a server on a loopback port that answers every request 401.
CHALLENGE, if a string, is its WWW-Authenticate header.  Return the
server process."
  (make-network-process
   :name "pos-http-test" :server t :host "127.0.0.1" :service t
   :family 'ipv4 :coding 'binary :noquery t
   :filter
   (lambda (connection data)
     (let ((seen (concat (process-get connection 'seen) data)))
       (process-put connection 'seen seen)
       (when (string-search "\r\n\r\n" seen)
         (process-send-string
          connection
          (concat "HTTP/1.1 401 Unauthorized\r\n"
                  (and challenge (format "WWW-Authenticate: %s\r\n" challenge))
                  "Content-Type: application/json\r\nContent-Length: 2\r\n"
                  "Connection: close\r\n\r\n{}"))
         (delete-process connection))))))

(ert-deftest pos-http/a-refusal-for-want-of-a-token-is-handed-back ()
  "A 401 to a request with no token is the caller's, whatever it challenges.
Emacs would otherwise ask at the terminal, or wait without end."
  (dolist (challenge '(nil "Bearer"
                           "Bearer resource_metadata=\"http://127.0.0.1/told\""
                           "Basic realm=\"keeper\""))
    (let ((server (pos-http-test-refusing challenge)))
      (unwind-protect
          (should (equal (with-timeout (10 'waited)
                           (pos-http-send
                            "GET" (format "http://127.0.0.1:%d/"
                                          (process-contact server :service))
                            nil nil))
                         '(401 . "{}")))
        (delete-process server)))))

(ert-deftest pos-http/a-keeper-that-says-nothing-is-given-up-on ()
  "A socket that takes a request and never answers it is not waited on.
The exchange is refused once `pos-http-timeout' has passed."
  (let ((server (make-network-process
                 :name "pos-http-test" :server t :host "127.0.0.1"
                 :service t :family 'ipv4 :coding 'binary :noquery t
                 :filter #'ignore))
        (pos-http-timeout 1))
    (unwind-protect
        (should (equal (with-timeout (20 'waited)
                         (condition-case err
                             (pos-http-send
                              "GET" (format "http://127.0.0.1:%d/"
                                            (process-contact server :service))
                              nil nil)
                           (pos-ledger-refused (cadr err))))
                       'remote))
      (delete-process server))))

(provide 'pos-http-test)
;;; pos-http-test.el ends here
