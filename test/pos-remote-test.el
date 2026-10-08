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
(require 'pos-fixtures)

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
      ("held" `((missing . ,(vconcat (pos-remote-held archive (append .cids nil))))))
      ("search" (pos-remote-search archive .q .mode .limit .within))
      ("put" `((held . ,(if (pos-remote-put archive .cid (encode-coding-string .block 'utf-8 t))
                            "new" "already"))))
      ("append"
       (pos-remote-append
        archive .name (encode-coding-string .event 'utf-8 t)
        (mapcar (lambda (file)
                  (cons (symbol-name (car file))
                        (encode-coding-string (cdr file) 'utf-8 t)))
                .files)
        .claims
        (seq-map (lambda (later)
                   (let-alist later
                     (cons .name (encode-coding-string .event 'utf-8 t))))
                 .following))))))

(ert-deftest pos-remote/the-client-makes-each-tape-s-requests ()
  "Every tape in fixtures/remote/ plays as recorded.
For each call the request recorded is sent, byte for byte, and the
response gives the result or the refusal recorded."
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
                                  (body_sha256 . ,(pos-ledger-sha body))))))
                    (let-alist (alist-get 'response exchange)
                      (cons .status (encode-coding-string .body 'utf-8 t)))))
                 (got (condition-case err
                          `((result . ,(pos-remote-test-call
                                        archive (alist-get 'call exchange))))
                        (pos-ledger-refused `((refused . ,(symbol-name (cadr err))))))))
            (pos-test-same-json (alist-get 'request exchange) sent)
            (pos-test-same-json
             (if (assq 'refused exchange)
                 `((refused . ,(alist-get 'refused exchange)))
               `((result . ,(alist-get 'result exchange))))
             got)))
        (setq index (1+ index))))))

(ert-deftest pos-remote/a-status-alone-names-the-refusal ()
  "An answer with no body to say why is refused by its status.
By what the status means; one that means nothing here, as remote."
  (let ((archive (pos-remote-http-create :url "https://keeper.example/ledger")))
    (pcase-dolist (`(,status . ,kind) '((401 . "access") (403 . "access") (404 . "absent")
                                        (409 . "chain") (410 . "erased") (413 . "size")
                                        (500 . "remote")))
      (let ((pos-remote-send-function (lambda (&rest _) (cons status "<html>"))))
        (pos-test-refused pos-ledger-refused kind (pos-remote-describe archive))))))

(defun pos-remote-test-begun (unnamed named)
  "Return the first events of a ledger begun before events had a ledger_id.
An alist of name and bytes: UNNAMED schema 1 events that name no
ledger, then NAMED that name one."
  (let (events previous)
    (dotimes (i (+ unnamed named))
      (let* ((event (pos-ledger-json
                     `((schema . 1) (previous . ,(or previous :null)) (add . ((,(format "%d.md" (1+ i)) . ((mode . 292) (size . 0)))))
                       ,@(when (>= i unnamed)
                           '((ledger_id . "0f1e2d3c-4b5a-4968-8778-a6b5c4d3e2f1"))))))
             (hash (pos-ledger-sha event)))
        (push (cons (format "%08d-%s.json" (1+ i) hash) event) events)
        (setq previous hash)))
    (nreverse events)))

(ert-deftest pos-remote/an-event-that-names-no-ledger-is-followed-to-the-first-that-does ()
  "An event is followed by what must be sent with it, and no more.
Each of the first events that name none is followed by the rest of
them and the first that names the ledger, and by no more; an event that
names the ledger, or comes after one, by nothing."
  (let ((events (pos-remote-test-begun 3 2)))
    (should (equal (list (seq-subseq events 1 4) (seq-subseq events 2 4)
                         (seq-subseq events 3 4) nil nil)
                   (mapcar (lambda (number) (pos-remote-following events number))
                           '(1 2 3 4 5))))))

(ert-deftest pos-remote/a-ledger-no-event-names-is-followed-by-nothing ()
  "Where no event names a ledger, nothing vouches for the first events."
  (let ((events (pos-remote-test-begun 3 0)))
    (should (equal '(nil nil nil)
                   (mapcar (lambda (number) (pos-remote-following events number))
                           '(1 2 3))))))

(ert-deftest pos-remote/later-events-travel-after-the-claims-and-before-the-files ()
  "An append is sent in parts, in order.
Name, event, claims, the events after it in order, then the files by
CID."
  (let* ((events (pos-remote-test-begun 2 1))
         sent
         (pos-remote-send-function
          (lambda (_method _url _headers body)
            (let ((start 0) names)
              (while (string-match "name=\"\\([^\"]*\\)\"" body start)
                (push (match-string 1 body) names)
                (setq start (match-end 0)))
              (setq sent (nreverse names)))
            (cons 201 "{}"))))
    (pos-remote-append (pos-remote-http-create :url "https://keeper.example/ledger" :token "t")
                       (car (car events)) (cdr (car events))
                       '(("bafkb" . "b") ("bafka" . "a")) nil (cdr events))
    (should (equal (list "name" "event" "claims" (car (nth 1 events)) (car (nth 2 events))
                         "bafka" "bafkb")
                   sent))))

(cl-defstruct pos-remote-test-memory
  "A keeper that is not HTTP: its events, in a list."
  events)

(cl-defmethod pos-remote-event ((archive pos-remote-test-memory) number)
  "Return event NUMBER of ARCHIVE, from its list."
  (nth (1- number) (pos-remote-test-memory-events archive)))

(defun pos-remote-test-refusing (challenge)
  "Start a server on a loopback port that answers every request 401.
CHALLENGE, if a string, is its WWW-Authenticate header.  Return the
server process."
  (make-network-process
   :name "pos-remote-test" :server t :host "127.0.0.1" :service t
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

(ert-deftest pos-remote/a-refusal-for-want-of-a-token-is-handed-back ()
  "A 401 to a request with no token is the caller's, whatever it challenges.
Emacs would otherwise ask at the terminal, or wait without end."
  (dolist (challenge '(nil "Bearer"
                           "Bearer resource_metadata=\"http://127.0.0.1/told\""
                           "Basic realm=\"keeper\""))
    (let ((server (pos-remote-test-refusing challenge)))
      (unwind-protect
          (should (equal (with-timeout (10 'waited)
                           (pos-remote--send
                            "GET" (format "http://127.0.0.1:%d/"
                                          (process-contact server :service))
                            nil nil))
                         '(401 . "{}")))
        (delete-process server)))))

(ert-deftest pos-remote/a-keeper-that-says-nothing-is-given-up-on ()
  "A socket that takes a request and never answers it is not waited on.
The exchange is refused once `pos-remote-timeout' has passed."
  (let ((server (make-network-process
                 :name "pos-remote-test" :server t :host "127.0.0.1"
                 :service t :family 'ipv4 :coding 'binary :noquery t
                 :filter #'ignore))
        (pos-remote-timeout 1))
    (unwind-protect
        (should (equal (with-timeout (20 'waited)
                         (condition-case err
                             (pos-remote--send
                              "GET" (format "http://127.0.0.1:%d/"
                                            (process-contact server :service))
                              nil nil)
                           (pos-ledger-refused (cadr err))))
                       'remote))
      (delete-process server))))

(ert-deftest pos-remote/a-port-takes-another-transport ()
  "The operations are generic: a keeper that is not HTTP answers them too."
  (should (equal "second"
                 (pos-remote-event (make-pos-remote-test-memory :events '("first" "second"))
                                   2))))

(provide 'pos-remote-test)
;;; pos-remote-test.el ends here
