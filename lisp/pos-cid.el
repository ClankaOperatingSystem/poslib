;;; pos-cid.el --- IPFS content identifiers, computed locally -*- lexical-binding: t; -*-

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

;; CIDs exactly as `ipfs add' computes them under the unixfs-v1-2025
;; import profile (IPIP-499), without IPFS: CIDv1, sha2-256, raw
;; leaves, 1 MiB chunks, balanced layout, 1024 links per file node.
;; Hidden files are left out, as `ipfs add' leaves them out.
;;
;; - `pos-cid-bytes': CID of a byte string.
;; - `pos-cid-file': CID of a file.
;; - `pos-cid-directory': CID of a directory tree.
;; - `pos-cid-tree': CID of every file and directory in a tree.
;; - `pos-cid-inventory': the same from the files' paths, CIDs and
;;   sizes alone, nothing read: how a ledger gives its archive's root.
;; - `pos-cid-decode': the bytes of a CID written in base32.
;; - `pos-cid-block': the CID of one block under a codec.
;;
;; A directory large enough to need HAMT sharding signals
;; `pos-cid-sharding-unsupported' rather than return a wrong CID.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar pos-cid-chunk-size 1048576
  "Bytes per file chunk.")

(defvar pos-cid-file-max-links 1024
  "Most links in one file node.")

(defvar pos-cid-sharding-threshold 262144
  "Largest directory block, in bytes, before IPFS shards it.")

(define-error 'pos-cid-sharding-unsupported
  "Directory needs HAMT sharding, which is not implemented")

;;;; Encoding

(defconst pos-cid--raw #x55 "Multicodec for raw bytes.")
(defconst pos-cid--dag-pb #x70 "Multicodec for dag-pb.")
(defconst pos-cid-dag-json #x0129 "Multicodec for dag-json.")

(defun pos-cid--varint (n)
  "Return N as an unsigned LEB128 byte string."
  (let (bytes)
    (while (>= n #x80)
      (push (logior (logand n #x7f) #x80) bytes)
      (setq n (ash n -7)))
    (push n bytes)
    (apply #'unibyte-string (nreverse bytes))))

(defun pos-cid--varint-field (number value)
  "Return protobuf field NUMBER holding the varint VALUE."
  (concat (pos-cid--varint (ash number 3)) (pos-cid--varint value)))

(defun pos-cid--bytes-field (number bytes)
  "Return protobuf field NUMBER holding the byte string BYTES."
  (concat (pos-cid--varint (logior (ash number 3) 2))
          (pos-cid--varint (length bytes))
          bytes))

(defun pos-cid--cid (codec block)
  "Return the binary CIDv1 of BLOCK under CODEC, hashed with sha2-256."
  (concat (unibyte-string 1)
          (pos-cid--varint codec)
          (unibyte-string #x12 32)
          (secure-hash 'sha256 block nil nil t)))

(defconst pos-cid--base32 "abcdefghijklmnopqrstuvwxyz234567"
  "RFC 4648 base32 alphabet, lower case.")

(defun pos-cid--base32 (bytes)
  "Return BYTES in unpadded lower-case base32."
  (let ((bits 0) (width 0) out)
    (dotimes (i (length bytes))
      (setq bits (logior (ash bits 8) (aref bytes i))
            width (+ width 8))
      (while (>= width 5)
        (setq width (- width 5))
        (push (aref pos-cid--base32 (logand (ash bits (- width)) 31)) out)
        (setq bits (logand bits (1- (ash 1 width))))))
    (when (> width 0)
      (push (aref pos-cid--base32 (logand (ash bits (- 5 width)) 31)) out))
    (concat (nreverse out))))

(defun pos-cid--unbase32 (text)
  "Return the bytes TEXT encodes, in unpadded lower-case base32."
  (let ((bits 0) (width 0) out)
    (dotimes (i (length text))
      (let ((value (seq-position pos-cid--base32 (aref text i))))
        (unless value
          (error "Not base32: %s" text))
        (setq bits (logior (ash bits 5) value)
              width (+ width 5))
        (when (>= width 8)
          (setq width (- width 8))
          (push (logand (ash bits (- width)) 255) out)
          (setq bits (logand bits (1- (ash 1 width)))))))
    (apply #'unibyte-string (nreverse out))))

(defun pos-cid--text (cid)
  "Return the binary CID in its multibase base32 form."
  (concat "b" (pos-cid--base32 cid)))

;;;; Nodes

;; A node is (CID TSIZE FILESIZE): its binary CID, the bytes of its
;; whole DAG, and for files the bytes of content.

(defun pos-cid--pb-node (links data)
  "Return the dag-pb block of LINKS and DATA, links first.
Each link is (CID NAME TSIZE)."
  (concat (mapconcat (lambda (link)
                       (pos-cid--bytes-field
                        2 (concat (pos-cid--bytes-field 1 (nth 0 link))
                                  (pos-cid--bytes-field 2 (nth 1 link))
                                  (pos-cid--varint-field 3 (nth 2 link)))))
                     links)
          (pos-cid--bytes-field 1 data)))

(defun pos-cid--pb (links data)
  "Return the node of the dag-pb block of LINKS and DATA."
  (let ((block (pos-cid--pb-node links data)))
    (list (pos-cid--cid pos-cid--dag-pb block)
          (apply #'+ (length block) (mapcar (lambda (l) (nth 2 l)) links)))))

(defun pos-cid--leaf (bytes)
  "Return the raw leaf node of BYTES."
  (list (pos-cid--cid pos-cid--raw bytes) (length bytes) (length bytes)))

(defun pos-cid--file-node (children)
  "Return the file node linking CHILDREN, each a node."
  (let* ((sizes (mapcar (lambda (c) (nth 2 c)) children))
         (data (concat (pos-cid--varint-field 1 2)
                       (pos-cid--varint-field 3 (apply #'+ sizes))
                       (mapconcat (lambda (s) (pos-cid--varint-field 4 s))
                                  sizes)))
         (node (pos-cid--pb (mapcar (lambda (c) (list (nth 0 c) "" (nth 1 c)))
                                    children)
                            data)))
    (append node (list (apply #'+ sizes)))))

(defun pos-cid--balance (nodes)
  "Return the root of a balanced file DAG over leaf NODES.
One leaf is its own root; otherwise each level groups the one below
into file nodes of at most `pos-cid-file-max-links' links."
  (while (cdr nodes)
    (let (level)
      (while nodes
        (let (group)
          (dotimes (_ pos-cid-file-max-links)
            (when nodes (push (pop nodes) group)))
          (push (pos-cid--file-node (nreverse group)) level)))
      (setq nodes (nreverse level))))
  (car nodes))

(defun pos-cid--file-tsize (size)
  "Return the size of the DAG of a file of SIZE bytes, from SIZE alone.
A leaf's size is its chunk's and a node's is its block's plus its
children's, and a binary CID is 36 bytes whatever it hashes, so no
content is needed."
  (let ((cid (make-string 36 0)) (start 0) leaves)
    (while (progn
             (let ((n (- (min size (+ start pos-cid-chunk-size)) start)))
               (push (list cid n n) leaves))
             (setq start (+ start pos-cid-chunk-size))
             (< start size)))
    (nth 1 (pos-cid--balance (nreverse leaves)))))

(defun pos-cid--chunk (file start end)
  "Return bytes START to END of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil start end)
    (buffer-string)))

(defun pos-cid--content (size read)
  "Return the node of SIZE bytes of content, READ from START to END."
  (let ((start 0) leaves)
    (while (progn
             (push (pos-cid--leaf
                    (funcall read start (min size (+ start pos-cid-chunk-size))))
                   leaves)
             (setq start (+ start pos-cid-chunk-size))
             (< start size)))
    (pos-cid--balance (nreverse leaves))))

(defun pos-cid--file (file)
  "Return the node of FILE's content."
  (pos-cid--content (file-attribute-size (file-attributes file))
                    (lambda (start end) (pos-cid--chunk file start end))))

(defun pos-cid--entries (dir)
  "Return the entries of DIR that IPFS would add, in its order.
Each is (NAME . PATH), NAME the name's bytes as stored, whatever
their Unicode normalisation."
  (let ((coding (or file-name-coding-system default-file-name-coding-system))
        (names (let ((file-name-coding-system 'binary))
                 (directory-files dir nil "\\`[^.]"))))
    (sort (mapcar (lambda (raw)
                    (cons raw (expand-file-name (decode-coding-string raw coding)
                                                dir)))
                  names)
          (lambda (a b) (string< (car a) (car b))))))

(defun pos-cid--directory (dir rel visit)
  "Return the node of DIR, at REL in its tree, calling VISIT on each node.
VISIT receives a relative path and a node."
  (let* ((links
          (mapcar
           (lambda (entry)
             (let* ((path (cdr entry))
                    (child (concat rel (unless (string-empty-p rel) "/")
                                   (file-name-nondirectory path)))
                    (node (cond
                           ((file-symlink-p path)
                            (error "Symlinks have no CID here: %s" path))
                           ((file-directory-p path)
                            (pos-cid--directory path child visit))
                           ((file-regular-p path)
                            (let ((n (pos-cid--file path)))
                              (funcall visit child n)
                              n))
                           (t (error "Not a regular file: %s" path)))))
               (list (nth 0 node) (car entry) (nth 1 node))))
           (pos-cid--entries dir)))
         (data (pos-cid--varint-field 1 1))
         (block (pos-cid--pb-node links data)))
    (when (> (length block) pos-cid-sharding-threshold)
      (signal 'pos-cid-sharding-unsupported (list dir (length block))))
    (let ((node (pos-cid--pb links data)))
      (funcall visit (if (string-empty-p rel) "." rel) node)
      node)))

;;;; Interface

(defun pos-cid-bytes (bytes)
  "Return the CID of the unibyte string BYTES, as a file's content."
  (when (multibyte-string-p bytes)
    (error "Encode the string first: CIDs are of bytes"))
  (pos-cid--text (car (pos-cid--content (length bytes)
                                        (lambda (start end)
                                          (substring bytes start end))))))

(defun pos-cid-file (file)
  "Return the CID of FILE's content."
  (pos-cid--text (car (pos-cid--file file))))

(defun pos-cid-directory (dir)
  "Return the CID of the tree at DIR."
  (pos-cid--text (car (pos-cid--directory dir "" #'ignore))))

(defun pos-cid-tree (dir)
  "Return the CID of every file and directory under DIR, root included.
An alist of relative path and CID; the root is \".\"."
  (let (cids)
    (pos-cid--directory dir ""
                        (lambda (rel node)
                          (push (cons rel (pos-cid--text (car node))) cids)))
    (nreverse cids)))

(defun pos-cid-decode (text)
  "Return the binary CID of TEXT, a CID in lower-case multibase base32."
  (unless (and (> (length text) 1) (eq (aref text 0) ?b))
    (error "Not a base32 CID: %s" text))
  (pos-cid--unbase32 (substring text 1)))

(defun pos-cid--inventory-directory (node rel visit)
  "Return the node of the directory NODE at REL, calling VISIT on each CID.
NODE maps names to child NODEs, or to (CID TSIZE TEXT) for files; VISIT
receives a relative path and a CID as text, files as given."
  (let* ((names (sort (hash-table-keys node)
                      (lambda (a b)
                        (string< (encode-coding-string a 'utf-8)
                                 (encode-coding-string b 'utf-8)))))
         (links (mapcar
                 (lambda (name)
                   (let* ((child (gethash name node))
                          (path (concat rel (unless (string-empty-p rel) "/") name))
                          (n (if (hash-table-p child)
                                 (pos-cid--inventory-directory child path visit)
                               (funcall visit path (nth 2 child))
                               child)))
                     (list (nth 0 n) (encode-coding-string name 'utf-8) (nth 1 n))))
                 names))
         (data (pos-cid--varint-field 1 1))
         (block (pos-cid--pb-node links data)))
    (when (> (length block) pos-cid-sharding-threshold)
      (signal 'pos-cid-sharding-unsupported (list rel (length block))))
    (let ((n (pos-cid--pb links data)))
      (funcall visit (if (string-empty-p rel) "." rel) (pos-cid--text (car n)))
      n)))

(defun pos-cid-block (codec block)
  "Return the CID of BLOCK, a unibyte string, under the multicodec CODEC.
With `pos-cid-dag-json', how a DAG-JSON ledger event is named."
  (pos-cid--text (pos-cid--cid codec block)))

(defun pos-cid-inventory (entries &optional empty)
  "Return the CID of every file and directory over ENTRIES, root included.
ENTRIES lists a tree's files as (PATH CID SIZE): the relative path, the
CID as text and the size in bytes.  Directories are derived from the
paths as `pos-cid-tree' finds them on disk; one that holds nothing has
no file to derive it from and is listed in EMPTY, by its path.  A hidden
component is refused since IPFS would leave it out.  An alist of
relative path and CID, files as given, the root \".\".  Signals
`pos-cid-sharding-unsupported' as `pos-cid-directory' does."
  (let ((tree (make-hash-table :test #'equal)) cids)
    (dolist (entry entries)
      (let* ((path (nth 0 entry))
             (parts (split-string path "/"))
             (node tree))
        (dolist (part parts)
          (when (or (string-empty-p part) (string-prefix-p "." part))
            (error "Not a path IPFS would add: %s" path)))
        (dolist (part (butlast parts))
          (let ((child (gethash part node)))
            (unless (or (null child) (hash-table-p child))
              (error "A file and a directory share a path: %s" path))
            (setq node (or child
                           (puthash part (make-hash-table :test #'equal) node)))))
        (when (gethash (car (last parts)) node)
          (error "A file and a directory share a path: %s" path))
        (puthash (car (last parts))
                 (list (pos-cid-decode (nth 1 entry))
                       (pos-cid--file-tsize (nth 2 entry))
                       (nth 1 entry))
                 node)))
    (dolist (path empty)
      (let ((parts (split-string path "/"))
            (node tree))
        (dolist (part parts)
          (when (or (string-empty-p part) (string-prefix-p "." part))
            (error "Not a path IPFS would add: %s" path)))
        (dolist (part (butlast parts))
          (let ((child (gethash part node)))
            (unless (or (null child) (hash-table-p child))
              (error "A file and a directory share a path: %s" path))
            (setq node (or child
                           (puthash part (make-hash-table :test #'equal) node)))))
        (when (gethash (car (last parts)) node)
          (error "Not an empty directory: %s" path))
        (puthash (car (last parts)) (make-hash-table :test #'equal) node)))
    (pos-cid--inventory-directory
     tree "" (lambda (rel cid) (push (cons rel cid) cids)))
    (nreverse cids)))

(provide 'pos-cid)
;;; pos-cid.el ends here
