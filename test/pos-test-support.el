;;; pos-test-support.el --- What the tests share  -*- lexical-binding: t -*-

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

;; The helpers every test file may use, so that no test file carries
;; its own copy: a temporary directory that is cleaned up whatever
;; happens, files written into it, a repository made in it, a clock
;; that does not move, and the shape a refusal is checked in.
;;
;; A test file requires this and nothing of another test file.  What
;; a test needs of the configuration it binds itself, with `let'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'pos-ledger)
(require 'pos-remote)
(require 'pos-seal)
(require 'pos-cid)

;;;; Where the tests are

(defconst pos-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The test directory, holding the fixtures of pos-config.el's shape.")

;;;; Isolation at load

;; No test reads or writes what a person keeps: tokens, indexes, Git
;; configuration.  The variables are set once, when the first test
;; file loads this.
(setenv "XDG_CONFIG_HOME" (make-temp-file "pos-test-config-" t))
(setenv "XDG_CACHE_HOME" (make-temp-file "pos-test-cache-" t))
(setenv "POS_ARCHIVE_TOKEN" nil)
(setenv "GIT_CONFIG_GLOBAL" "/dev/null")
(setenv "GIT_CONFIG_NOSYSTEM" "1")
(setenv "GIT_AUTHOR_NAME" "Test")
(setenv "GIT_AUTHOR_EMAIL" "test@example.org")
(setenv "GIT_COMMITTER_NAME" "Test")
(setenv "GIT_COMMITTER_EMAIL" "test@example.org")
(setenv "GIT_AUTHOR_DATE" "2026-01-01T00:00:00Z")
(setenv "GIT_COMMITTER_DATE" "2026-01-01T00:00:00Z")

;; A test asks no keeper but one a recording answers for.
(setq pos-ledger-offline t)

;; The index of each temporary tree goes to a cache of the tests' own,
;; never to a database of the user's.
(defvar pos-roam-cache-directory)
(with-eval-after-load 'pos-roam
  (setq pos-roam-cache-directory (make-temp-file "pos-test-roam-" t)))

;;;; Directories and files

(defun pos-test-writable (dir)
  "Give every file under DIR its write bit back, so DIR can be deleted."
  (dolist (file (directory-files-recursively dir "" t))
    (unless (file-symlink-p file)
      (set-file-modes file (logior (file-modes file) #o200)))))

(defun pos-test-kill-buffers-under (dir)
  "Kill every buffer visiting a file under DIR, discarding its edits."
  (dolist (buffer (buffer-list))
    (when-let* ((file (buffer-file-name buffer)))
      (when (string-prefix-p dir (file-truename file))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(defmacro pos-test-with-temp-dir (dir &rest body)
  "Evaluate BODY with DIR bound to a new temporary directory.
DIR is a directory name, with its trailing slash.  Afterwards the
buffers visiting files under it are killed, its files made writable
and the directory deleted, however BODY ended."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,dir (file-name-as-directory (make-temp-file "pos-test-" t))))
     (unwind-protect
         (progn ,@body)
       (pos-test-kill-buffers-under (file-truename ,dir))
       (pos-test-writable ,dir)
       (delete-directory ,dir t))))

(defun pos-test-write-files (dir files)
  "Write FILES under DIR: (RELATIVE-PATH . TEXT) pairs, directories made."
  (dolist (file files)
    (let ((path (expand-file-name (car file) dir)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path (insert (cdr file))))))

(defmacro pos-test-with-files (dir files &rest body)
  "Evaluate BODY with DIR a temporary directory holding FILES.
FILES as for `pos-test-write-files'."
  (declare (indent 2) (debug (symbolp form body)))
  `(pos-test-with-temp-dir ,dir
     (pos-test-write-files ,dir ,files)
     ,@body))

(defun pos-test-file-string (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun pos-test-relative (files root)
  "Return FILES as paths relative to ROOT, sorted."
  (sort (mapcar (lambda (file) (file-relative-name file root)) files)
        #'string<))

;;;; Repositories

(defun pos-test-git (dir &rest args)
  "Run git with ARGS in DIR and return what it printed, trimmed.
Signal an error if git did not succeed."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "Git %s in %s: %s" (string-join args " ") dir
               (string-trim (buffer-string)))))
    (string-trim (buffer-string))))

(defun pos-test-git-init (dir)
  "Make DIR a repository on master, with a deterministic author."
  (pos-test-git dir "init" "-q" "-b" "master")
  dir)

;;;; Time

(defun pos-test-time (year month day hour minute)
  "Return the local time YEAR, MONTH, DAY, HOUR and MINUTE name."
  (encode-time (list 0 minute hour day month year nil -1 nil)))

(defconst pos-test-sunday (encode-time (list 0 0 23 6 9 2026 nil -1 nil))
  "Sunday 2026-09-06 23:00 local: the sweep boundary the tests use.")

(defmacro pos-test-with-clock (time &rest body)
  "Evaluate BODY with `current-time' and `float-time' answering TIME.
TIME is a time value; a test that depends on today binds it here."
  (declare (indent 1) (debug (form body)))
  (let ((now (make-symbol "now")))
    `(let ((,now ,time))
       (cl-letf (((symbol-function 'current-time) (lambda () ,now))
                 ((symbol-function 'float-time)
                  (lambda (&optional time) (time-to-seconds (or time ,now)))))
         ,@body))))

;;;; Bytes and JSON

(defun pos-test-write-bytes (file bytes &optional mode)
  "Write the unibyte string BYTES to FILE, with MODE, default #o644."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'binary))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes)))
  (set-file-modes file (or mode #o644)))

(defun pos-test-same-json (expected actual)
  "Check that EXPECTED and ACTUAL are the same JSON value."
  (should (equal (pos-ledger-json expected) (pos-ledger-json actual))))

(defun pos-test-report-relative (report dir)
  "Return REPORT with its absolute paths made relative to DIR."
  (let ((root (file-name-as-directory (file-truename dir))))
    (vconcat
     (mapcar (lambda (entry)
               (mapcar (lambda (pair)
                         (pcase (car pair)
                           ('archive (cons 'archive (file-relative-name (cdr pair) root)))
                           (_ pair)))
                       entry))
             report))))

;;;; Sealing

(defun pos-test-approve (plan)
  "Apply PLAN as a reviewer approves it: with the hash of its own bytes."
  (pos-seal-apply plan (pos-ledger-sha (pos-ledger-json plan))))

(defmacro pos-test-with-scope (&rest body)
  "Evaluate BODY with `scope' a temporary scope holding archives/ and an item."
  (declare (indent 0))
  `(pos-test-with-temp-dir dir
     (let ((scope (expand-file-name "scope" dir)))
       (make-directory (expand-file-name "archives" scope) t)
       (pos-test-write-bytes (expand-file-name "trial/result.md" scope) "result")
       ,@body)))

(defun pos-test-scope-plan (scope)
  "Return the plan to seal SCOPE's trial into its archive."
  (pos-seal-plan (expand-file-name "trial" scope)
                 (expand-file-name "archives/trial" scope)))

(defun pos-test-scope-sealed (scope)
  "Seal SCOPE's trial into its archive; return the archive."
  (let ((plan (pos-test-scope-plan scope)))
    (pos-test-approve plan)
    (file-truename (expand-file-name "archives" scope))))

;;;; Keepers

(defun pos-test-tape (recorded)
  "Return a function that sends requests by playing RECORDED, a keeper's.
RECORDED is a fixture's keeper: its url, token and exchanges.  Each
request must be the next exchange recorded, and is answered as it was.
Called with no argument, the function gives the exchanges not yet
played."
  (let ((left (append (alist-get 'exchanges recorded) nil))
        (base (alist-get 'url recorded)))
    (lambda (&optional method url headers body)
      (if (null method)
          left
        (let* ((exchange (or (pop left)
                             (error "A request the recording does not have: %s %s"
                                    method url)))
               (sent `((method . ,method)
                       (path . ,(substring url (length base)))
                       (authorization . ,(or (cdr (assoc "Authorization" headers)) :null))
                       ,@(when body
                           `((content_type . ,(cdr (assoc "Content-Type" headers)))
                             (body_sha256 . ,(pos-ledger-sha body)))))))
          (unless (equal (pos-ledger-json sent)
                         (pos-ledger-json (alist-get 'request exchange)))
            (error "Not the request recorded: %S" sent))
          (cons (alist-get 'status (alist-get 'response exchange))
                (encode-coding-string (alist-get 'body (alist-get 'response exchange))
                                      'utf-8)))))))

(defmacro pos-test-with-keeper (recorded tape &rest body)
  "Evaluate BODY with RECORDED, a fixture's keeper, answering requests.
TAPE, if a symbol, is bound to the function playing it, which
`funcall' with no argument asks for what is left.  With a recording,
checks ask it and every exchange must be played; with none, they ask
nobody."
  (declare (indent 2) (debug (form symbolp body)))
  (let ((keeper (make-symbol "keeper"))
        (tape (or tape (make-symbol "tape"))))
    `(let* ((,keeper ,recorded)
            (,tape (and ,keeper (pos-test-tape ,keeper)))
            (pos-ledger-offline (not ,tape))
            (pos-remote-send-function (or ,tape pos-remote-send-function))
            (pos-remote-keeper-function
             (if ,tape
                 (lambda (url)
                   (pos-remote-http-create :url url :token (alist-get 'token ,keeper)))
               pos-remote-keeper-function)))
       (prog1 (progn ,@body)
         (when (and ,tape (funcall ,tape))
           (error "The recording was not played out"))))))

;;;; Refusals

(defmacro pos-test-refused (error kind &rest body)
  "Check that BODY signals ERROR whose first datum is KIND.
A refusal in poslib is an error symbol a module defines, whose data
begins with the kind, a symbol; KIND may be a string.  BODY that
signals nothing, or another kind, fails with both kinds shown."
  (declare (indent 2) (debug (form form body)))
  `(should (equal (format "%s" ,kind)
                  (format "%s" (condition-case refused
                                   (progn ,@body :nothing-refused)
                                 (,error (car (cdr refused))))))))

(provide 'pos-test-support)
;;; pos-test-support.el ends here
