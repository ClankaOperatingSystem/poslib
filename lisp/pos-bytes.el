;;; pos-bytes.el --- Bytes, hashes and DAG-JSON -*- lexical-binding: t; -*-

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

;; A file's bytes, their hash, and DAG-JSON as doc/formats.org specifies
;; it, in lockstep with pyposlib.
;;
;; - `pos-bytes-read': a file's bytes.
;; - `pos-bytes-sha': the SHA-256 of bytes.
;; - `pos-bytes-json': DAG-JSON bytes of a value, and a newline.
;; - `pos-bytes-block': the same without the newline, a block.
;; - `pos-bytes-parse': the value in JSON bytes.
;; - `pos-bytes-key': an object's key as a string.
;; - `pos-bytes-utf8<': the order of two strings as UTF-8 bytes.
;;
;; JSON values are Lisp values: objects alists, arrays vectors, strings,
;; integers and :null.

;;; Code:

;;;; Files

(defun pos-bytes-read (file)
  "Return FILE's bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun pos-bytes-sha (bytes)
  "Return the lower-case hex SHA-256 of BYTES."
  (secure-hash 'sha256 bytes))

;;;; DAG-JSON

(defun pos-bytes-utf8< (a b)
  "Return non-nil if the string A precedes B as UTF-8 bytes."
  (string< (encode-coding-string a 'utf-8 t) (encode-coding-string b 'utf-8 t)))

(defun pos-bytes-key (key)
  "Return KEY, a symbol or string, as a string."
  (if (symbolp key) (symbol-name key) key))

(defun pos-bytes--string (string)
  "Return STRING as a JSON string literal."
  (concat "\""
          (mapconcat
           (lambda (c)
             (pcase c
               (?\" "\\\"") (?\\ "\\\\")
               (?\b "\\b") (?\f "\\f") (?\n "\\n") (?\r "\\r") (?\t "\\t")
               ((pred (> #x20)) (format "\\u%04x" c))
               (_ (string c))))
           string "")
          "\""))

(defun pos-bytes--encode (value)
  "Return VALUE as canonical JSON text, without the final newline."
  (cond
   ((eq value :null) "null")
   ((stringp value) (pos-bytes--string value))
   ((integerp value)
    (unless (<= (- (expt 2 63)) value (1- (expt 2 63)))
      (error "No DAG-JSON for an integer of more than 64 bits: %S" value))
    (number-to-string value))
   ((vectorp value) (concat "[" (mapconcat #'pos-bytes--encode value ",") "]"))
   ((listp value)
    (concat "{"
            (mapconcat (lambda (pair)
                         (concat (pos-bytes--string (pos-bytes-key (car pair)))
                                 ":" (pos-bytes--encode (cdr pair))))
                       (sort (copy-sequence value)
                             (lambda (a b) (pos-bytes-utf8< (pos-bytes-key (car a))
                                                            (pos-bytes-key (car b)))))
                       ",")
            "}"))
   (t (error "No canonical JSON for %S" value))))

(defun pos-bytes-json (value)
  "Return the canonical JSON bytes of VALUE, with its final newline."
  (encode-coding-string (concat (pos-bytes--encode value) "\n") 'utf-8))

(defun pos-bytes-block (value)
  "Return VALUE as a DAG-JSON block: its canonical bytes, with no newline."
  (encode-coding-string (pos-bytes--encode value) 'utf-8))

(defun pos-bytes-parse (bytes)
  "Return the JSON value in BYTES."
  (json-parse-string (decode-coding-string bytes 'utf-8) :object-type 'alist
                     :null-object :null :false-object :false))

(provide 'pos-bytes)
;;; pos-bytes.el ends here
