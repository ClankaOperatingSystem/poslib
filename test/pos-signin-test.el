;;; pos-signin-test.el --- Tests for pos-signin  -*- lexical-binding: t; -*-

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

;; Run: make test.  The issuer is a stand-in: it says where its
;; endpoints are, and gives a token for a code only to the client that
;; holds the verifier its challenge was made from.  The browser is a
;; stand-in too: it takes the code straight back to the loopback
;; address, which is all a person's browser does once they have signed
;; in.

;;; Code:

(require 'ert)
(require 'pos-signin)
(require 'pos-remote)
(require 'pos-fixtures)

(defconst pos-signin-test-issuer "https://issuer.example")

(defvar pos-signin-test-asked nil "The queries the browser was sent with.")
(defvar pos-signin-test-codes nil "Codes handed out, each with its query.")
(defvar pos-signin-test-refreshes nil "Refresh tokens that are still good.")
(defvar pos-signin-test-granted 0 "How many tokens the issuer has given.")
(defvar pos-signin-test-lifetime 3600 "How long a token it gives lasts.")
(defvar pos-signin-test-state nil "A state to send back in place of the one sent.")

(defun pos-signin-test-json (value)
  "Return (200 . BYTES) answering VALUE."
  (cons 200 (pos-ledger-json value)))

(defun pos-signin-test-fetch (url &optional form)
  "Answer URL, and FORM if it is a POST, as the issuer and its keepers."
  (cond
   ((string-suffix-p pos-signin-well-known url)
    (pos-signin-test-json
     `((resource . ,(substring url 0 (- (length pos-signin-well-known))))
       (authorization_servers . [,pos-signin-test-issuer])
       (scopes_supported . ["openid" "a-scope"])
       (client_id . "a-public-client"))))
   ((equal url (concat pos-signin-test-issuer "/.well-known/openid-configuration"))
    (pos-signin-test-json
     `((authorization_endpoint . ,(concat pos-signin-test-issuer "/authorize"))
       (token_endpoint . ,(concat pos-signin-test-issuer "/token")))))
   ((and form (equal url (concat pos-signin-test-issuer "/token")))
    (let* ((field (lambda (name) (cdr (assoc name form))))
           (good
            (pcase (funcall field "grant_type")
              ("authorization_code"
               (let ((asked (cdr (assoc (funcall field "code") pos-signin-test-codes))))
                 (setq pos-signin-test-codes
                       (assoc-delete-all (funcall field "code") pos-signin-test-codes))
                 (and asked
                      (equal (cadr (assoc "code_challenge" asked))
                             (base64url-encode-string
                              (secure-hash 'sha256 (funcall field "code_verifier")
                                           nil nil t)
                              t))
                      (equal (cadr (assoc "code_challenge_method" asked)) "S256")
                      (equal (cadr (assoc "redirect_uri" asked))
                             (funcall field "redirect_uri"))
                      (equal (cadr (assoc "client_id" asked))
                             (funcall field "client_id")))))
              ("refresh_token"
               (prog1 (member (funcall field "refresh_token") pos-signin-test-refreshes)
                 (setq pos-signin-test-refreshes
                       (delete (funcall field "refresh_token")
                               pos-signin-test-refreshes)))))))
      (if (not good)
          (cons 400 "{\"error\":\"invalid_grant\"}")
        (setq pos-signin-test-granted (1+ pos-signin-test-granted))
        (let ((refresh (format "refresh-%d" pos-signin-test-granted)))
          (push refresh pos-signin-test-refreshes)
          (pos-signin-test-json
           `((access_token . ,(format "access-%d" pos-signin-test-granted))
             (token_type . "Bearer") (expires_in . ,pos-signin-test-lifetime)
             (refresh_token . ,refresh)))))))
   (t (cons 404 "{}"))))

(defun pos-signin-test-browse (url)
  "Stand in for a person who signs in at once at URL."
  (let* ((query (url-parse-query-string (cadr (split-string url "?"))))
         (code (format "code-%d" (1+ (length pos-signin-test-asked))))
         (redirect (url-generic-parse-url (cadr (assoc "redirect_uri" query)))))
    (push query pos-signin-test-asked)
    (push (cons code query) pos-signin-test-codes)
    (run-at-time
     0.05 nil
     (lambda ()
       (let ((connection (open-network-stream "pos-signin-test" nil "127.0.0.1"
                                              (url-port redirect))))
         (set-process-query-on-exit-flag connection nil)
         (process-send-string
          connection
          (format "GET /callback?code=%s&state=%s HTTP/1.1\r\nHost: localhost\r\n\r\n"
                  code (or pos-signin-test-state (cadr (assoc "state" query))))))))))

(defmacro pos-signin-test-with-issuer (&rest body)
  "Evaluate BODY with the stand-in issuer and browser, and nothing kept."
  (declare (indent 0))
  `(let ((process-environment
          (cons (concat "XDG_CONFIG_HOME=" (make-temp-file "pos-signin" t))
                process-environment))
         (pos-signin-fetch-function #'pos-signin-test-fetch)
         (pos-signin-browse-function #'pos-signin-test-browse)
         (pos-signin-test-asked nil) (pos-signin-test-codes nil)
         (pos-signin-test-refreshes nil) (pos-signin-test-granted 0)
         (pos-signin-test-lifetime 3600) (pos-signin-test-state nil))
     ,@body))

(ert-deftest pos-signin/signing-in-keeps-a-token-only-its-owner-can-read ()
  "The browser is sent to the issuer with the keeper's client, the scopes
it names and one for a refresh token, and a challenge; the code that
comes back is exchanged with the verifier; what is kept is mode 0600."
  (pos-signin-test-with-issuer
    (let ((told (pos-signin "https://one.keeper.example/ledgers/a" 10))
          (asked (lambda (name) (cadr (assoc name (car pos-signin-test-asked))))))
      (should (equal '(t "https://issuer.example" "a-public-client")
                     (list (alist-get 'opened told) (alist-get 'issuer told)
                           (alist-get 'client_id told))))
      (should (equal '("code" "a-public-client" "openid a-scope offline_access" "S256")
                     (mapcar asked '("response_type" "client_id" "scope"
                                     "code_challenge_method"))))
      (should (string-match-p "\\`http://localhost:[0-9]+/callback\\'"
                              (funcall asked "redirect_uri")))
      (should (equal "access-1" (pos-signin-token "https://one.keeper.example/ledgers/a")))
      (should (equal #o600 (logand #o777 (file-modes (pos-signin-file))))))))

(ert-deftest pos-signin/one-sign-in-serves-every-ledger-of-a-keeper ()
  "Asked to sign in to a second origin that names the same issuer and
client, nothing is opened; and a request to an origin never seen adopts
the token after its first refusal."
  (pos-signin-test-with-issuer
    (pos-signin "https://one.keeper.example" 10)
    (let ((again (pos-signin "https://two.keeper.example" 10)))
      (should (equal 1 (length pos-signin-test-asked)))
      (should (eq :false (alist-get 'opened again))))
    (let* (sent
           (pos-remote-send-function
            (lambda (_method _url headers _body)
              (let ((bearer (cdr (assoc "Authorization" headers))))
                (push bearer sent)
                (if (equal bearer "Bearer access-1")
                    (cons 200 "{\"events\":0}")
                  (cons 401 ""))))))
      (should (equal '((events . 0))
                     (pos-remote-describe
                      (pos-remote-http-create :url "https://three.keeper.example/x"))))
      (should (equal '("Bearer access-1" nil) sent))
      (should (equal "access-1" (pos-signin-token "https://three.keeper.example"))))))

(ert-deftest pos-signin/a-token-about-to-expire-is-refreshed-and-kept ()
  (pos-signin-test-with-issuer
    (setq pos-signin-test-lifetime 30)
    (pos-signin "https://one.keeper.example" 10)
    (should (equal "access-2" (pos-signin-token "https://one.keeper.example")))
    (should (equal "refresh-2"
                   (alist-get 'refresh_token
                              (cdr (assoc "https://issuer.example a-public-client"
                                          (car (pos-signin--load)))))))))

(ert-deftest pos-signin/a-token-that-cannot-be-refreshed-is-no-token ()
  (pos-signin-test-with-issuer
    (setq pos-signin-test-lifetime 30)
    (pos-signin "https://one.keeper.example" 10)
    (setq pos-signin-test-refreshes nil)
    (should-not (pos-signin-token "https://one.keeper.example"))))

(ert-deftest pos-signin/an-answer-that-is-not-the-one-asked-for-is-refused ()
  "The state that comes back must be the state that was sent."
  (pos-signin-test-with-issuer
    (setq pos-signin-test-state "another")
    (pos-test-refused pos-ledger-refused "access" (pos-signin "https://one.keeper.example" 10))
    (should-not (file-exists-p (pos-signin-file)))))

(ert-deftest pos-signin/nobody-signing-in-is-refused-when-the-wait-runs-out ()
  (pos-signin-test-with-issuer
    (let ((pos-signin-browse-function #'ignore))
      (pos-test-refused pos-ledger-refused "access" (pos-signin "https://one.keeper.example" 0.3)))))

(ert-deftest pos-signin/a-request-without-a-token-says-to-sign-in ()
  "Refused for want of a token, with none kept, a request says how to get
one and opens nothing."
  (pos-signin-test-with-issuer
    (let ((pos-remote-send-function (lambda (&rest _) (cons 401 "{\"refused\":\"access\"}"))))
      (should (equal "Not signed in to this keeper; sign in with: sign-in https://one.keeper.example/ledgers/a"
                     (condition-case err
                         (pos-remote-describe
                          (pos-remote-http-create
                           :url "https://one.keeper.example/ledgers/a"))
                       (pos-ledger-refused (nth 2 err)))))
      (should-not pos-signin-test-asked))))

(ert-deftest pos-signin/what-either-library-keeps-the-other-reads ()
  "The shared fixture: a kept file, and the token each keeper's requests
then carry."
  (dolist (named (pos-fixtures "signin"))
    (pos-signin-test-with-issuer
      (let-alist (cdr named)
        (make-directory (file-name-directory (pos-signin-file)) t)
        (pos-test-write-bytes (pos-signin-file) (pos-ledger-json .file) #o600)
        (seq-doseq (carried .carried)
          (ert-info ((alist-get 'url carried))
            (should (equal (alist-get 'token carried)
                           (or (pos-signin-token (alist-get 'url carried)) :null)))))))))

(provide 'pos-signin-test)
;;; pos-signin-test.el ends here
