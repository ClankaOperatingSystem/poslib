;;; pos-remote-test.el --- Tests for pos-remote.el  -*- lexical-binding: t -*-

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

;; Run: make test.  The tapes in fixtures/remote/ are the protocol's
;; exchanges, shared with pyposlib: the client must make each request as
;; recorded and make of each response the result or refusal recorded.

;;; Code:

(require 'ert)
(require 'pos-remote)
(require 'pos-ledger-test
         (expand-file-name "pos-ledger-test"
                           (file-name-directory (or load-file-name
                                                    buffer-file-name))))

(defun pos-remote-test-call (archive call)
  "Make the CALL a tape's exchange describes of ARCHIVE; return its result.
As the tape writes it: an object, or for bytes an object holding them."
  (let-alist call
    (pcase .operation
      ("describe" (pos-remote-describe archive))
      ("event"
       `((bytes . ,(decode-coding-string (pos-remote-event archive .number) 'utf-8))))
      ("read"
       `((bytes . ,(decode-coding-string (pos-remote-read archive .cid .path) 'utf-8))))
      ("append"
       (pos-remote-append
        archive .name (encode-coding-string .event 'utf-8 t)
        (mapcar (lambda (file)
                  (cons (symbol-name (car file))
                        (encode-coding-string (cdr file) 'utf-8 t)))
                .files)
        .claims)))))

(ert-deftest pos-remote/the-client-makes-each-tape-s-requests ()
  "Every tape in fixtures/remote/: for each call the request recorded is
sent, byte for byte, and the response gives the result or the refusal
recorded."
  (dolist (named (pos-fixtures "remote"))
    (let* ((tape (cdr named))
           (base (alist-get 'url tape))
           (archive (pos-remote-http-create :url base :token (alist-get 'token tape)))
           (index 0))
      (seq-doseq (exchange (alist-get 'exchanges tape))
        (ert-info ((format "%s %d" (car named) index))
          (let* (sent
                 (pos-remote-send-function
                  (lambda (method url headers body)
                    (setq sent
                          `((method . ,method)
                            (path . ,(substring url (length base)))
                            (authorization . ,(cdr (assoc "Authorization" headers)))
                            ,@(when body
                                `((content_type . ,(cdr (assoc "Content-Type" headers)))
                                  (body_sha256 . ,(pos-ledger--sha body))))))
                    (let-alist (alist-get 'response exchange)
                      (cons .status (encode-coding-string .body 'utf-8 t)))))
                 (got (condition-case err
                          `((result . ,(pos-remote-test-call
                                        archive (alist-get 'call exchange))))
                        (pos-ledger-refused `((refused . ,(symbol-name (cadr err))))))))
            (pos-ledger-test-same (alist-get 'request exchange) sent)
            (pos-ledger-test-same
             (if (assq 'refused exchange)
                 `((refused . ,(alist-get 'refused exchange)))
               `((result . ,(alist-get 'result exchange))))
             got)))
        (setq index (1+ index))))))

(ert-deftest pos-remote/a-status-alone-names-the-refusal ()
  "An answer with no body to say why is refused by what its status means,
and one that means nothing here, as remote."
  (let ((archive (pos-remote-http-create :url "https://keeper.example/ledger")))
    (pcase-dolist (`(,status . ,kind) '((401 . "access") (403 . "access") (404 . "absent")
                                        (409 . "chain") (410 . "erased") (413 . "size")
                                        (500 . "remote")))
      (let ((pos-remote-send-function (lambda (&rest _) (cons status "<html>"))))
        (pos-ledger-test-refused kind (pos-remote-describe archive))))))

(cl-defstruct pos-remote-test-memory
  "A keeper that is not HTTP: its events, in a list."
  events)

(cl-defmethod pos-remote-event ((archive pos-remote-test-memory) number)
  "Return event NUMBER of ARCHIVE, from its list."
  (nth (1- number) (pos-remote-test-memory-events archive)))

(ert-deftest pos-remote/a-port-takes-another-transport ()
  "The operations are generic: a keeper that is not HTTP answers them too."
  (should (equal "second"
                 (pos-remote-event (make-pos-remote-test-memory :events '("first" "second"))
                                   2))))

(provide 'pos-remote-test)
;;; pos-remote-test.el ends here
