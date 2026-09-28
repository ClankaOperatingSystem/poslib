;;; pos-cid-fixtures.el --- Trees with CIDs known from IPFS  -*- lexical-binding: t -*-

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

;; Each fixture builds a file or tree and names the CID that
;; `ipfs add --only-hash' gave it: kubo 0.43.1, profile unixfs-v1-2025.
;; make check-ipfs asks an installed kubo again.

;;; Code:

(defun pos-cid-fixture-write (file bytes)
  "Write the unibyte string BYTES to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'binary))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes))))

(defun pos-cid-fixture-bytes (n)
  "Return N bytes of a repeating pattern."
  (apply #'unibyte-string (mapcar (lambda (i) (% i 251)) (number-sequence 0 (1- n)))))

(defun pos-cid-fixture-names (dir)
  "Fill DIR with names that sort differently as bytes and as text."
  (pos-cid-fixture-write (expand-file-name "Zed/x" dir) "1")
  (pos-cid-fixture-write (expand-file-name "ärger/big" dir)
                         (make-string (1+ 1048576) 0))
  (pos-cid-fixture-write (expand-file-name "zebra" dir) "2")
  (pos-cid-fixture-write (expand-file-name "Äpfel" dir) "3")
  (pos-cid-fixture-write (expand-file-name "a-b" dir) "4")
  (pos-cid-fixture-write (expand-file-name "a_b" dir) "5"))

(defun pos-cid-fixture-threshold (dir pad)
  "Fill DIR so its directory block is 262144 bytes, plus PAD."
  (dotimes (i 1044)
    (pos-cid-fixture-write
     (expand-file-name (format "f%04d%s" i (make-string 200 ?x)) dir) "z"))
  (pos-cid-fixture-write
   (expand-file-name (concat "g" (make-string (+ 51 pad) ?y)) dir) "z"))

(defconst pos-cid-fixtures
  `((empty-file
     "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku"
     ,(lambda (d) (pos-cid-fixture-write (expand-file-name "f" d) "")))
    (hello
     "bafkreicysg23kiwv34eg2d7qweipxwosdo2py4ldv42nbauguluen5v6am"
     ,(lambda (d) (pos-cid-fixture-write (expand-file-name "f" d) "hello\n")))
    (one-chunk
     "bafkreibq4fevl27rgurgnxbp7adh42aqiyd6ouflxhj3gzmcxcxzbh6lla"
     ,(lambda (d) (pos-cid-fixture-write (expand-file-name "f" d)
                                         (make-string 1048576 0))))
    (one-chunk-and-a-byte
     "bafybeihd4yzq7n5umhjngdum4r6k2to7egxfkf2jz6thvwzf6djus22cmq"
     ,(lambda (d) (pos-cid-fixture-write (expand-file-name "f" d)
                                         (make-string (1+ 1048576) 0))))
    (three-levels
     "bafybeibelkboygzcmatovx3eerk3vraxcbewm6y2dunddy2tu4mzq6gtcm"
     ,(lambda (d) (pos-cid-fixture-write (expand-file-name "f" d)
                                         (pos-cid-fixture-bytes (+ (* 256 17) 5))))
     :chunk 256 :links 4)
    (empty-directory
     "bafybeiczsscdsbs7ffqz55asqdf3smv6klcw3gofszvwlyarci47bgf354"
     ,(lambda (d) (make-directory (expand-file-name "t" d))))
    (small-tree
     "bafybeicoixhc5l34x6xelxadn5wpjho42domvdbpkzljc2pn6vwh4mrw4u"
     ,(lambda (d)
        (pos-cid-fixture-write (expand-file-name "t/B" d) "a")
        (pos-cid-fixture-write (expand-file-name "t/a" d) "b")
        (pos-cid-fixture-write (expand-file-name "t/sub/c" d) "c")
        (pos-cid-fixture-write (expand-file-name "t/.hidden" d) "x")))
    (names
     "bafybeia2rqy4et7dyd7ygfk4ujt6yh6pvo552gjwbwlbpa3ve4sctd4anm"
     ,(lambda (d) (pos-cid-fixture-names (expand-file-name "t" d))))
    (composed-name
     "bafybeihzs3po2lwiatggwgrhfacxzaognui3a7udf5h5r52mkhsdeuyqpm"
     ,(lambda (d) (pos-cid-fixture-write
                   (expand-file-name (concat "t/" (decode-coding-string "\303\204pfel" 'utf-8))
                                     d)
                   "3")))
    (decomposed-name
     "bafybeihkgvpjyivvugf24d4fkqqrzfyuurjajwloejkja4i37olw4wbinu"
     ,(lambda (d) (pos-cid-fixture-write
                   (expand-file-name (concat "t/" (decode-coding-string "A\314\210pfel" 'utf-8))
                                     d)
                   "3")))
    (at-sharding-threshold
     "bafybeib4htzo5urethxn7whbpfsvdjhpvrsanokpai4uhdesreh3n5sxcu"
     ,(lambda (d) (pos-cid-fixture-threshold (expand-file-name "t" d) 0))))
  "Fixtures: name, CID from IPFS, builder, then chunk and link limits.
A builder writes into a directory and leaves one entry there.")

(defun pos-cid-fixture (name)
  "Return fixture NAME."
  (or (assq name pos-cid-fixtures) (error "No fixture %s" name)))

(defun pos-cid-fixture-build (fixture dir)
  "Build FIXTURE in DIR; return the path of its one entry."
  (funcall (nth 2 fixture) dir)
  (car (directory-files dir t "\\`[^.]")))

(defmacro pos-cid-fixture-with (fixture path &rest body)
  "Evaluate BODY with PATH bound to FIXTURE built in a temporary directory.
The fixture's chunk and link limits are in force."
  (declare (indent 2))
  `(let* ((fixture ,fixture)
          (dir (make-temp-file "pos-cid" t))
          (pos-cid-chunk-size (or (plist-get (nthcdr 3 fixture) :chunk)
                                  pos-cid-chunk-size))
          (pos-cid-file-max-links (or (plist-get (nthcdr 3 fixture) :links)
                                      pos-cid-file-max-links)))
     (unwind-protect
         (let ((,path (pos-cid-fixture-build fixture dir))) ,@body)
       (delete-directory dir t))))

(provide 'pos-cid-fixtures)
;;; pos-cid-fixtures.el ends here
