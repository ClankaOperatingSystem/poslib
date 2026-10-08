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
;; - `pos-cid-blocks': every block of a file's DAG, by CID: what a keeper
;;   is put, block by block.
;; - `pos-cid-decode': the bytes of a CID written in base32.
;; - `pos-cid-block': the CID of one block under a codec.
;;
;; A directory whose node would exceed 256 KiB is a HAMT shard, as IPFS
;; makes it: entries placed by the bytes of the murmur3-x64-64 hash of
;; their names, 256 slots a level.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar pos-cid-chunk-size 1048576
  "Bytes per file chunk.")

(defvar pos-cid-file-max-links 1024
  "Most links in one file node.")

(defvar pos-cid-sharding-threshold 262144
  "Largest directory block, in bytes, before IPFS shards it.")

(defconst pos-cid--hamt-fanout 256 "Slots in each HAMT shard.")
(defconst pos-cid--murmur3-x64-64 #x22 "Multihash code of the HAMT's hash.")

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

(defconst pos-cid--mask64 (1- (ash 1 64)) "The low 64 bits.")

(defun pos-cid--rotl64 (x r)
  "Return the 64-bit X rotated left by R bits."
  (logand (logior (ash x r) (ash x (- r 64))) pos-cid--mask64))

(defun pos-cid--fmix64 (k)
  "Return the MurmurHash3 finalisation mix of the 64-bit K."
  (setq k (logxor k (ash k -33)))
  (setq k (logand (* k #xff51afd7ed558ccd) pos-cid--mask64))
  (setq k (logxor k (ash k -33)))
  (setq k (logand (* k #xc4ceb9fe1a85ec53) pos-cid--mask64))
  (logxor k (ash k -33)))

(defun pos-cid--le64 (bytes start end)
  "Return the little-endian integer in BYTES from START below END."
  (let ((n 0))
    (while (> end start)
      (setq end (1- end)
            n (logior (ash n 8) (aref bytes end))))
    n))

(defun pos-cid-murmur3-x64-64 (bytes)
  "Return the first 64 bits of MurmurHash3 x64 128 of BYTES, seed 0.
An 8-byte big-endian string: the multihash murmur3-x64-64, by which a
HAMT places a name."
  (let* ((mask pos-cid--mask64)
         (c1 #x87c37b91114253d5) (c2 #x4cf5ad432745937f)
         (h1 0) (h2 0)
         (n (length bytes))
         (blocks (- n (% n 16)))
         (i 0))
    (while (< i blocks)
      (let ((k1 (pos-cid--le64 bytes i (+ i 8)))
            (k2 (pos-cid--le64 bytes (+ i 8) (+ i 16))))
        (setq h1 (logxor h1 (logand (* (pos-cid--rotl64 (logand (* k1 c1) mask) 31) c2) mask))
              h1 (logand (+ (pos-cid--rotl64 h1 27) h2) mask)
              h1 (logand (+ (* h1 5) #x52dce729) mask)
              h2 (logxor h2 (logand (* (pos-cid--rotl64 (logand (* k2 c2) mask) 33) c1) mask))
              h2 (logand (+ (pos-cid--rotl64 h2 31) h1) mask)
              h2 (logand (+ (* h2 5) #x38495ab5) mask)))
      (setq i (+ i 16)))
    (when (> (- n blocks) 8)
      (let ((k2 (pos-cid--le64 bytes (+ blocks 8) n)))
        (setq h2 (logxor h2 (logand (* (pos-cid--rotl64 (logand (* k2 c2) mask) 33) c1) mask)))))
    (when (> n blocks)
      (let ((k1 (pos-cid--le64 bytes blocks (min n (+ blocks 8)))))
        (setq h1 (logxor h1 (logand (* (pos-cid--rotl64 (logand (* k1 c1) mask) 31) c2) mask)))))
    (setq h1 (logxor h1 n) h2 (logxor h2 n)
          h1 (logand (+ h1 h2) mask) h2 (logand (+ h2 h1) mask)
          h1 (pos-cid--fmix64 h1) h2 (pos-cid--fmix64 h2)
          h1 (logand (+ h1 h2) mask))
    (apply #'unibyte-string
           (mapcar (lambda (shift) (logand (ash h1 (- shift)) 255))
                   '(56 48 40 32 24 16 8 0)))))

(defun pos-cid--trim-zeros (bytes)
  "Return BYTES without its leading zero bytes, as a HAMT's bitfield is stored."
  (let ((i 0))
    (while (and (< i (length bytes)) (zerop (aref bytes i)))
      (setq i (1+ i)))
    (substring bytes i)))

(defun pos-cid--shard (entries level)
  "Return the HAMT shard node over ENTRIES, placed by byte LEVEL of their hashes.
Each entry is (HASH . LINK), LINK as `pos-cid--pb-node' takes it.  A lone
entry in a slot is linked under the slot's two hex digits and its name;
several are linked under the digits alone, as a sub-shard placed by the
next byte."
  (let ((slots (make-vector pos-cid--hamt-fanout nil))
        (bitfield (make-string (/ pos-cid--hamt-fanout 8) 0))
        links)
    (dolist (entry entries)
      (let ((index (aref (car entry) level)))
        (aset slots index (cons entry (aref slots index)))))
    (dotimes (index pos-cid--hamt-fanout)
      (let ((slot (aref slots index)))
        (when slot
          (let ((at (- (length bitfield) 1 (/ index 8))))
            (aset bitfield at (logior (aref bitfield at) (ash 1 (% index 8)))))
          (let ((prefix (format "%02X" index)))
            (push (if (cdr slot)
                      (let ((sub (pos-cid--shard (nreverse slot) (1+ level))))
                        (list (nth 0 sub) prefix (nth 1 sub)))
                    (let ((link (cdar slot)))
                      (list (nth 0 link) (concat prefix (nth 1 link)) (nth 2 link))))
                  links)))))
    (pos-cid--pb (nreverse links)
                 (concat (pos-cid--varint-field 1 5)
                         (pos-cid--bytes-field 2 (pos-cid--trim-zeros bitfield))
                         (pos-cid--varint-field 5 pos-cid--murmur3-x64-64)
                         (pos-cid--varint-field 6 pos-cid--hamt-fanout)))))

(defun pos-cid--directory-node (links)
  "Return the node of a directory linking LINKS, each (CID NAME TSIZE).
A plain directory node, or a HAMT shard where that node would exceed
`pos-cid-sharding-threshold' bytes."
  (let ((data (pos-cid--varint-field 1 1)))
    (if (> (length (pos-cid--pb-node links data)) pos-cid-sharding-threshold)
        (pos-cid--shard (mapcar (lambda (link)
                                  (cons (pos-cid-murmur3-x64-64 (nth 1 link)) link))
                                links)
                        0)
      (pos-cid--pb links data))))

(defun pos-cid--leaf (bytes)
  "Return the raw leaf node of BYTES."
  (list (pos-cid--cid pos-cid--raw bytes) (length bytes) (length bytes)))

(defun pos-cid--file-links (children)
  "Return (LINKS . DATA) of the file node linking CHILDREN, each a node."
  (let ((sizes (mapcar (lambda (c) (nth 2 c)) children)))
    (cons (mapcar (lambda (c) (list (nth 0 c) "" (nth 1 c))) children)
          (concat (pos-cid--varint-field 1 2)
                  (pos-cid--varint-field 3 (apply #'+ sizes))
                  (mapconcat (lambda (s) (pos-cid--varint-field 4 s)) sizes)))))

(defun pos-cid--file-node (children)
  "Return the file node linking CHILDREN, each a node."
  (let ((parts (pos-cid--file-links children)))
    (append (pos-cid--pb (car parts) (cdr parts))
            (list (apply #'+ (mapcar (lambda (c) (nth 2 c)) children))))))

(defun pos-cid-blocks (bytes)
  "Return the blocks of the unibyte string BYTES as a file, (CID . BLOCK) each.
Its leaves, and the nodes over them when it is more than one chunk, the
file's own CID among them; CID is text.  Equal blocks appear once."
  (when (multibyte-string-p bytes)
    (error "Encode the string first: blocks are of bytes"))
  (let ((size (length bytes)) (start 0) nodes held)
    (while (progn
             (let* ((chunk (substring bytes start (min size (+ start pos-cid-chunk-size))))
                    (node (pos-cid--leaf chunk)))
               (unless (assoc (pos-cid--text (car node)) held)
                 (push (cons (pos-cid--text (car node)) chunk) held))
               (push node nodes))
             (setq start (+ start pos-cid-chunk-size))
             (< start size)))
    (setq nodes (nreverse nodes))
    (while (cdr nodes)
      (let (level)
        (while nodes
          (let (group)
            (dotimes (_ pos-cid-file-max-links)
              (when nodes (push (pop nodes) group)))
            (let* ((children (nreverse group))
                   (parts (pos-cid--file-links children))
                   (node (pos-cid--file-node children)))
              (unless (assoc (pos-cid--text (car node)) held)
                (push (cons (pos-cid--text (car node))
                            (pos-cid--pb-node (car parts) (cdr parts)))
                      held))
              (push node level))))
        (setq nodes (nreverse level))))
    (nreverse held)))

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
         (node (pos-cid--directory-node links)))
    (funcall visit (if (string-empty-p rel) "." rel) node)
    node))

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
         (n (pos-cid--directory-node links)))
    (funcall visit (if (string-empty-p rel) "." rel) (pos-cid--text (car n)))
    n))

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
relative path and CID, files as given, the root \".\"."
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
