;;; pos-migrate.el --- Migrate an archive to content identity -*- lexical-binding: t; -*-

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

;; The one sanctioned rewrite of archived bytes, as doc/formats.org
;; specifies: an archive with a legacy ledger is rebuilt so that every
;; link is an ipfs:// link, a rumour's or an annotation, its hidden
;; evidence renamed and its junk removed, its collections declared, and
;; its ledger moved beside it and closed by one conversion event.
;;
;; Planning builds the result in _migrate/ beside the archive and
;; describes it; application replays the plan into a fresh copy, checks
;; it against the plan, and swaps it in.
;;
;; - `pos-migrate-plan', `pos-migrate-apply', `pos-migrate-batch'.

;;; Code:

(require 'pos-cid)
(require 'pos-ledger)
(require 'pos-links)
(require 'pos-seal)

(defconst pos-migrate-junk '(".DS_Store")
  "Hidden files that are never evidence, and are removed.")

;;;; Paths

(defun pos-migrate--write-over (file bytes)
  "Write BYTES to FILE, replacing it, keeping its mode if it has one."
  (let ((modes (and (file-exists-p file) (file-modes file)))
        (coding-system-for-write 'binary))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes))
    (set-file-modes file (or modes #o644))))

(defun pos-migrate--rel (file root)
  "Return FILE relative to ROOT, with no trailing slash."
  (directory-file-name (file-relative-name file (file-name-as-directory root))))

(defun pos-migrate--walk (dir)
  "Return every file and directory under DIR, relative, parents first."
  (let (out)
    (named-let walk ((d dir) (rel nil))
      (dolist (name (pos-ledger--entries d))
        (let ((path (expand-file-name name d)) (r (if rel (concat rel "/" name) name)))
          (push r out)
          (when (and (file-directory-p path) (not (file-symlink-p path)))
            (walk path r)))))
    (nreverse out)))

(defun pos-migrate--through (path renames)
  "Return PATH after the RENAMES, each (OLD . NEW), in order."
  (dolist (r renames path)
    (cond ((equal path (car r)) (setq path (cdr r)))
          ((string-prefix-p (concat (car r) "/") path)
           (setq path (concat (cdr r) (substring path (length (car r)))))))))

(defun pos-migrate--back (path renames)
  "Return the original of PATH, before the RENAMES."
  (dolist (r (reverse renames) path)
    (cond ((equal path (cdr r)) (setq path (car r)))
          ((string-prefix-p (concat (cdr r) "/") path)
           (setq path (concat (car r) (substring path (length (cdr r)))))))))

(defun pos-migrate--hidden-renames (stage)
  "Return the renames of STAGE's hidden entries, applying them there.
A legacy ledger folder or checkpoint folder moves to the archive-integrity
folder beside its archive; other hidden entries are prefixed dot."
  (let (renames found)
    (while (setq found (seq-find (lambda (rel)
                                   (string-prefix-p "." (file-name-nondirectory rel)))
                                 (pos-migrate--walk stage)))
      (let* ((name (file-name-nondirectory found))
             (parent (file-name-directory found))
             (new (cond
                   ((and (equal name pos-ledger-directory) parent
                         (equal (file-name-nondirectory (directory-file-name parent)) "archives"))
                    (concat (or (file-name-directory (directory-file-name parent)) "")
                            pos-ledger-integrity "/ledger"))
                   ((equal name pos-ledger-anchors)
                    (concat (or parent "") pos-ledger-integrity "/checkpoints"))
                   (t (concat (or parent "") "dot" name)))))
        (when (file-exists-p (expand-file-name new stage))
          (pos-ledger--refuse 'destination "Rename would overwrite: %s" new))
        (make-directory (file-name-directory (expand-file-name new stage)) t)
        (rename-file (expand-file-name found stage) (expand-file-name new stage))
        (push (cons found new) renames)))
    (nreverse renames)))

;;;; Collections

(defun pos-migrate--candidates (stage exclude &optional include)
  "Return the directories in STAGE that are collections, less EXCLUDE.
Receipts, capsules, trial and study runs, and retired scopes, as their
shape shows them, and the directories INCLUDE names."
  (let ((found
         (seq-filter
          (lambda (rel)
            (let ((path (expand-file-name rel stage))
                  (parent (file-name-nondirectory
                           (directory-file-name (or (file-name-directory rel) "")))))
              (and (file-directory-p path) (not (member rel exclude))
                   (or (member rel include)
                       (and (file-regular-p (expand-file-name "manifest.json" path))
                            (file-directory-p (expand-file-name "source" path)))
                       (pos-ledger-capsule-p path)
                       (equal parent "capsules")
                       (member parent '("trials" "studies" "pipeline-trials"))
                       (file-directory-p (expand-file-name "archives" path))))))
          (pos-migrate--walk stage))))
    ;; A collection holds all within it: none is declared inside another.
    (sort (seq-remove (lambda (c) (seq-some (lambda (o) (and (not (equal o c))
                                                             (pos-ledger--within-p c o)))
                                            found))
                      found)
          #'string<)))

(defun pos-migrate--declare (stage collection)
  "Declare COLLECTION in STAGE; return (README . KIND), KIND new or line."
  (let* ((dir (expand-file-name collection stage))
         (readme (expand-file-name "README.org" dir))
         (line (concat pos-ledger-declaration "\n")))
    (if (file-exists-p readme)
        (let ((bytes (pos-ledger--read readme)))
          (unless (pos-ledger--declared-p dir)
            (let ((at (if (string-match "\\`\\(?:#\\+[^\n]*\n\\)*" bytes) (match-end 0) 0)))
              (pos-migrate--write-over readme (concat (substring bytes 0 at) line
                                                   (substring bytes at)))))
          (cons (concat collection "/README.org") "line"))
      (let ((entrance (seq-find (lambda (n) (file-exists-p (expand-file-name n dir)))
                                '("README.md" "README" "index.org" "brief.org"))))
        (pos-migrate--write-over
         readme
         (encode-coding-string
          (concat "#+TITLE: " (file-name-nondirectory collection) "\n" line "\n"
                  "A collection, declared when its archive moved to content identity."
                  (if entrance (format " Its entrance is [[file:%s][%s]]." entrance entrance) "")
                  "\n")
          'utf-8))
        (cons (concat collection "/README.org") "new")))))

;;;; Dates, for loops

(defun pos-migrate--date (rel archive)
  "Return the date ordering the record at REL in ARCHIVE among others.
A date its path begins with, else its first commit's, else none."
  (or (seq-some (lambda (part) (and (string-match "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" part)
                                    (match-string 0 part)))
                (split-string rel "/"))
      (let ((dates (ignore-errors
                     (process-lines "git" "-C" archive "log" "--diff-filter=A"
                                    "--format=%as" "--" rel))))
        (car (last dates)))
      "9999-99-99"))

(defun pos-migrate--sccs (nodes edges)
  "Return the strongly connected components of NODES under EDGES.
EDGES is an alist of node and the nodes it points to."
  (let ((index 0) (stack nil) (indices (make-hash-table :test #'equal))
        (lows (make-hash-table :test #'equal)) (on (make-hash-table :test #'equal)) result)
    (named-let visit-all ((todo nodes))
      (when todo
        (unless (gethash (car todo) indices)
          (named-let visit ((v (car todo)))
            (puthash v index indices) (puthash v index lows) (setq index (1+ index))
            (push v stack) (puthash v t on)
            (dolist (w (cdr (assoc v edges)))
              (cond ((not (gethash w indices))
                     (visit w)
                     (puthash v (min (gethash v lows) (gethash w lows)) lows))
                    ((gethash w on)
                     (puthash v (min (gethash v lows) (gethash w indices)) lows))))
            (when (= (gethash v lows) (gethash v indices))
              (let (component w)
                (while (progn (setq w (pop stack)) (remhash w on) (push w component)
                              (not (equal w v))))
                (push component result)))))
        (visit-all (cdr todo))))
    result))

;;;; Planning

(defun pos-migrate--rumour (target root date)
  "Return (DESTINATION . TEXT), the rumour of TARGET, dated DATE.
TARGET's path is given from ROOT, the archive's scope."
  (let ((path (file-relative-name target root)))
    (pos-seal-rumour
     date (concat "#+TITLE: Rumour of " path "\n#+DATE: " date "\n\n"
                  "On " date ", migrating its archive found links to =" path
                  "=, which content identity cannot cite. It was then "
                  (pos-links-description target root) ".\n"))))

(defun pos-migrate--converted-p (archive)
  "Return non-nil if ARCHIVE's ledger records CIDs for every entry."
  (let ((entries (car (pos-ledger-history archive))))
    (and entries (seq-every-p (lambda (e) (assq 'cid (cdr e))) entries))))

(defun pos-migrate--in-capsule-p (rel collections stage)
  "Return non-nil if REL lies in one of COLLECTIONS in STAGE that is a capsule.
A capsule's links are left as written."
  (seq-some (lambda (c) (and (pos-ledger--within-p rel c)
                             (pos-ledger-capsule-p (expand-file-name c stage))))
            collections))

(defun pos-migrate--item-of (rel collections)
  "Return the item holding REL: its outermost of COLLECTIONS, else REL."
  (or (seq-find (lambda (c) (pos-ledger--within-p rel c)) collections) rel))

(defun pos-migrate--stage (archive stage)
  "Copy ARCHIVE, less its legacy ledger, to STAGE, writable."
  (when (file-exists-p stage)
    (pos-migrate--writable stage)
    (delete-directory stage t))
  (make-directory (file-name-directory (directory-file-name stage)) t)
  (copy-directory archive stage t t t)
  (let ((ledger (expand-file-name pos-ledger-directory stage)))
    (when (file-exists-p ledger)
      (pos-migrate--writable ledger)
      (delete-directory ledger t)))
  (pos-migrate--writable stage))

(defun pos-migrate--writable (dir)
  "Make everything under DIR writable."
  (set-file-modes dir (logior (file-modes dir) #o700))
  (dolist (file (directory-files-recursively dir "" t))
    (unless (file-symlink-p file)
      (set-file-modes file (logior (file-modes file) #o200)))))

(defun pos-migrate--prepare (archive stage exclude &optional include)
  "Build STAGE from ARCHIVE, and return what was done there.
Junk is removed, hidden entries renamed and collections, less EXCLUDE
and with INCLUDE, declared.  Return (DISCARD REMOVE RENAMES COLLECTIONS
DECLARATIONS)."
  (pos-migrate--stage archive stage)
  (let* ((known (car (pos-ledger-history archive)))
         (actual (pos-ledger-inventory archive))
         (missing (car (pos-ledger--differences known actual)))
         discard remove)
    (dolist (rel (pos-migrate--walk stage))
      (when (member (file-name-nondirectory rel) pos-migrate-junk)
        (delete-file (expand-file-name rel stage))
        (if (assoc rel known) (push rel remove) (push rel discard))))
    (let* ((renames (pos-migrate--hidden-renames stage))
           (collections (pos-migrate--candidates stage exclude include))
           ;; A capsule declares itself by its manifest, and is kept byte for byte.
           (declarations (mapcar (lambda (c) (pos-migrate--declare stage c))
                                 (seq-remove (lambda (c) (pos-ledger-capsule-p
                                                          (expand-file-name c stage)))
                                             collections))))
      (list (sort discard #'string<) (sort (append missing remove) #'string<)
            renames collections declarations))))

(defun pos-migrate-plan (archive &optional root exclude date ledger-id include)
  "Return the plan migrating ARCHIVE, its canon links repaired under ROOT.
EXCLUDE names candidate collections that are not, INCLUDE directories
that are though their shape does not show it; DATE, by default today's,
dates rumours; LEDGER-ID names a ledger that has none."
  (let* ((archive (file-truename (directory-file-name (expand-file-name archive))))
         (scope (file-name-directory archive))
         (root (file-truename (or root scope)))
         (date (or date (format-time-string "%Y-%m-%d")))
         (stage (expand-file-name (concat "_migrate/" (file-name-nondirectory archive) "/stage")
                                  scope))
         (history (pos-ledger-history archive)))
    (unless (seq-some (lambda (e) (not (assq 'cid (cdr e)))) (car history))
      (pos-ledger--refuse 'converted "Nothing to migrate in %s" archive))
    (let ((changed (nth 1 (pos-ledger--differences (car history) (pos-ledger-inventory archive)))))
      (when changed
        (pos-ledger--refuse 'differs "Enrolled evidence changed in %s: %S" archive changed)))
    (pcase-let* ((inventory-sha (pos-ledger--sha (pos-ledger-json (pos-ledger-inventory archive))))
                 (`(,discard ,remove ,renames ,collections ,declarations)
                  (pos-migrate--prepare archive stage exclude include))
                 (files (seq-filter (lambda (rel)
                                      (and (string-match-p "\\.\\(org\\|md\\)\\'" rel)
                                           (file-regular-p (expand-file-name rel stage))))
                                    (pos-migrate--walk stage)))
                 (links nil) (deps nil) (rumours nil))
      ;; Resolve every link from where it was written, in the archive as it is.
      (dolist (rel (seq-remove (lambda (rel) (pos-migrate--in-capsule-p rel collections stage))
                               files))
        (let* ((original (pos-migrate--back rel renames))
               (written (file-name-directory (expand-file-name original archive))))
          (dolist (link (pos-links-in-file (expand-file-name rel stage)))
            (pcase-let* ((`(,_ ,text ,path ,suffix) link)
                         (written-abs (expand-file-name path written))
                         ;; A path gone from the archive, whose counterpart at the
                         ;; same relative path exists, as a withdrawn record's does
                         ;; in canon, or a record's archived since its link was
                         ;; written: the counterpart is meant.
                         (abs (let ((counterpart
                                     (and (string-prefix-p (file-name-as-directory archive)
                                                           written-abs)
                                          (not (file-exists-p written-abs))
                                          (not (file-exists-p
                                                (expand-file-name
                                                 (pos-migrate--through
                                                  (pos-migrate--rel written-abs archive) renames)
                                                 stage)))
                                          (expand-file-name (pos-migrate--rel written-abs archive)
                                                            scope))))
                                (if (and counterpart (file-exists-p counterpart))
                                    counterpart written-abs)))
                         (inside (string-prefix-p (file-name-as-directory archive) abs)))
              (push
               (cond
                ((and inside (let ((t-rel (pos-migrate--through (pos-migrate--rel abs archive)
                                                                 renames)))
                               (file-exists-p (expand-file-name t-rel stage))))
                 (let* ((target (pos-migrate--through (pos-migrate--rel abs archive) renames))
                        (item (pos-migrate--item-of rel collections))
                        (titem (pos-migrate--item-of target collections)))
                   (cond ((equal target rel) (list rel link "internal" text))
                         ((seq-some (lambda (c) (and (pos-ledger--within-p rel c)
                                                     (pos-ledger--within-p target c)))
                                    collections)
                          (list rel link "internal" text))
                         ((and (file-directory-p (expand-file-name target stage))
                               (not (member target collections)))
                          (let ((r (pos-migrate--rumour abs scope date)))
                            (unless (assoc (car r) rumours) (push r rumours))
                            (list rel link "rumour" :rumour (car r) suffix)))
                         (t (push (cons item titem) deps)
                            (list rel link "cid" :item titem target suffix)))))
                ((or (file-exists-p abs) (file-symlink-p abs))
                 (let ((other (pos-links--archive-of abs)))
                   (cond
                    ((and other (not inside) (pos-links-within-scope-p other scope))
                     (unless (pos-migrate--converted-p other)
                       (pos-ledger--refuse 'order "Migrate %s first: %s links to it" other rel))
                     (list rel link "cid" (concat (pos-links--sealed abs) suffix)))
                    (t (let ((r (pos-migrate--rumour abs scope date)))
                         (unless (assoc (car r) rumours) (push r rumours))
                         (list rel link "rumour" :rumour (car r) suffix))))))
                (t (list rel link "broken" (pos-links-annotate rel link "broken"))))
               links)))))
      ;; Loops among items: a link from an earlier record to a later one
      ;; becomes an annotation.
      (let* ((items (seq-uniq (append (mapcar #'car deps) (mapcar #'cdr deps))))
             (edges (mapcar (lambda (i) (cons i (seq-uniq (mapcar #'cdr (seq-filter (lambda (d) (and (equal (car d) i) (not (equal (cdr d) i)))) deps)))))
                            items))
             (later (make-hash-table :test #'equal)))
        (dolist (component (pos-migrate--sccs items edges))
          (when (cdr component)
            (dolist (a component)
              (dolist (b (cdr (assoc a edges)))
                (when (and (member b component)
                           (let ((da (pos-migrate--date a archive)) (db (pos-migrate--date b archive)))
                             (or (string< da db) (and (equal da db) (string< a b)))))
                  (puthash (cons a b) t later))))))
        (setq links
              (mapcar (lambda (l)
                        (pcase l
                          (`(,rel ,link "cid" :item ,titem ,_ ,_)
                           (if (gethash (cons (pos-migrate--item-of rel collections) titem) later)
                               (list rel link "later" (pos-links-annotate rel link "later"))
                             l))
                          (_ l)))
                      links))
        (setq deps (seq-remove (lambda (d) (gethash d later)) deps)))
      ;; Rumours, then items leaves-first: rewrite each, then take its CID.
      (dolist (r rumours)
        (let ((there (expand-file-name (car r) stage)))
          (if (file-exists-p there)
              (unless (equal (pos-ledger--read there) (encode-coding-string (cdr r) 'utf-8))
                (pos-ledger--refuse 'destination "Rumour destination exists: %s" (car r)))
            (make-directory (file-name-directory there) t)
            (pos-migrate--write-over there (encode-coding-string (cdr r) 'utf-8)))))
      (let ((cids (mapcar (lambda (r) (cons (car r) (pos-cid-file (expand-file-name (car r) stage))))
                          rumours))
            (pending (seq-uniq (mapcar (lambda (l) (pos-migrate--item-of (car l) collections)) links)))
            (done nil) (resolved nil))
        (while pending
          (let ((item (seq-find (lambda (i) (seq-every-p (lambda (d) (or (not (equal (car d) i))
                                                                           (equal (cdr d) i)
                                                                           (member (cdr d) done)
                                                                           (not (member (cdr d) pending))))
                                                         deps))
                                pending)))
            (unless item (pos-ledger--refuse 'loop "Links still loop among %S" pending))
            (setq pending (delete item pending))
            (let ((by-file nil))
              (dolist (l links)
                (when (equal (pos-migrate--item-of (car l) collections) item)
                  (let ((to (pcase l
                              (`(,_ ,_ ,_ :item ,titem ,target ,suffix)
                               (let ((cid (or (cdr (assoc titem cids))
                                              (pos-migrate--cid stage titem))))
                                 (concat "ipfs://" cid
                                         (unless (equal target titem)
                                           (concat "/" (substring target (1+ (length titem)))))
                                         suffix)))
                              (`(,_ ,_ ,_ :rumour ,dest ,suffix)
                               (concat "ipfs://" (cdr (assoc dest cids)) suffix))
                              (`(,_ ,_ ,_ ,to) to))))
                    (push (list (car l) (nth 1 l) (nth 2 l) to) by-file))))
              (dolist (rel (seq-uniq (mapcar #'car by-file)))
                (let* ((file (expand-file-name rel stage))
                       ;; Each as (OFFSET FROM TO KIND): an annotation replaces
                       ;; the whole link, a resolution only its target.
                       (all (mapcar (lambda (l)
                                      (let ((link (nth 1 l))
                                            (whole (member (nth 2 l) '("broken" "later"))))
                                        (list (if whole (nth 4 link) (nth 0 link))
                                              (if whole (nth 5 link) (nth 1 link))
                                              (nth 3 l) (nth 2 l))))
                                    (seq-filter (lambda (l) (equal (car l) rel)) by-file)))
                       (edits (mapcar (lambda (e) (seq-take e 3))
                                      (seq-remove (lambda (e) (equal (nth 1 e) (nth 2 e))) all))))
                  (dolist (e all)
                    (push (list rel (nth 0 e) (nth 1 e) (nth 3 e) (nth 2 e)) resolved))
                  (when edits
                    (pos-migrate--write-over file (pos-links-rewrite (pos-ledger--read file) edits)))))
              (push item done)
              (push (cons item (pos-migrate--cid stage item)) cids))))
        ;; The canon links that name paths the migration moves.
        (let* ((moves (append (list (cons (concat (pos-migrate--rel archive root) "/" pos-ledger-directory)
                                          (concat (pos-migrate--rel scope root) "/"
                                                  pos-ledger-integrity "/ledger")))
                              (mapcar (lambda (r) (cons (concat (pos-migrate--rel archive root) "/" (car r))
                                                        (concat (pos-migrate--rel archive root) "/" (cdr r))))
                                      renames)))
               (canon (pos-migrate--canon root moves))
               (event (pos-migrate--event stage history remove renames
                                          collections ledger-id)))
          `((schema . 1) (operation . "migrate")
            (archive . ,archive) (stage . ,stage) (root . ,root) (date . ,date)
            (exclude . ,(vconcat exclude)) (include . ,(vconcat include))
            (inventory_sha256 . ,inventory-sha)
            (discard . ,(vconcat discard)) (remove . ,(vconcat remove))
            (renames . ,(vconcat (mapcar (lambda (r) `((from . ,(car r)) (to . ,(cdr r)))) renames)))
            (collections . ,(vconcat collections))
            (declarations . ,(vconcat (mapcar (lambda (d) `((readme . ,(car d)) (kind . ,(cdr d))))
                                              declarations)))
            (rumours . ,(vconcat (mapcar (lambda (r) `((destination . ,(car r)) (text . ,(cdr r))))
                                         (sort rumours (lambda (a b) (string< (car a) (car b)))))))
            (links . ,(vconcat
                       (mapcar (lambda (l) `((file . ,(nth 0 l)) (offset . ,(nth 1 l))
                                             (from . ,(nth 2 l)) (kind . ,(nth 3 l))
                                             (to . ,(nth 4 l))))
                               (sort resolved (lambda (a b) (if (equal (car a) (car b))
                                                                (< (nth 1 a) (nth 1 b))
                                                              (string< (car a) (car b))))))))
            (canon . ,(vconcat canon))
            (final_inventory_sha256
             . ,(pos-ledger--sha (pos-ledger-json (pos-ledger-inventory stage))))
            (event . ,event)))))))

(defun pos-migrate--cid (stage item)
  "Return the CID of ITEM in STAGE, a file or a directory."
  (let ((path (expand-file-name item stage)))
    (if (file-directory-p path) (pos-cid-directory path) (pos-cid-file path))))

(defun pos-migrate--canon (root moves)
  "Return the rewrites of canon links under ROOT to moved paths.
Each of MOVES is (OLD . NEW), paths from ROOT."
  (let (out)
    (dolist (file (directory-files-recursively
                   root "\\.\\(org\\|md\\)\\'" nil
                   (lambda (dir) (not (string-match-p "/\\(?:archives\\|_[^/]*\\|\\.[^/]*\\)\\'" dir)))))
      (let ((dir (file-name-directory file)) edits)
        (dolist (link (pos-links-in-file file))
          (pcase-let* ((`(,offset ,text ,path ,suffix) link)
                       (rel (pos-migrate--rel (expand-file-name path dir) root))
                       (new (pos-migrate--through rel moves)))
            (unless (equal new rel)
              (let ((to (file-relative-name (expand-file-name new root) dir)))
                (push `((offset . ,offset) (from . ,text)
                        (to . ,(concat (if (string-prefix-p "file:" text) "file:" "") to suffix)))
                      edits)))))
        (when edits
          (push `((file . ,(pos-migrate--rel file root))
                  (sha256 . ,(pos-ledger--sha (pos-ledger--read file)))
                  (edits . ,(vconcat (nreverse edits))))
                out))))
    (nreverse out)))

(defun pos-migrate--event (stage history remove renames collections ledger-id)
  "Return (NAME . TEXT), the conversion event closing a migration.
STAGE is the archive as it will be; HISTORY its ledger now; REMOVE and
RENAMES what becomes of its entries; COLLECTIONS those declared;
LEDGER-ID names a ledger that has none."
  (pcase-let* ((`(,known ,head ,events ,files) history)
               (legacy (seq-filter (lambda (e) (not (assq 'cid (cdr e)))) known))
               (cids (pos-cid-tree stage))
               (entry (lambda (rel) (cons (cons 'cid (cdr (assoc rel cids)))
                                          (pos-ledger-record (expand-file-name rel stage)))))
               (convert nil) (rename nil) (add nil))
    (dolist (e legacy)
      (unless (member (car e) remove)
        (let ((new (pos-migrate--through (car e) renames)))
          (unless (equal new (car e)) (push (cons (car e) new) rename))
          (push (cons new (cons (cons 'from (alist-get 'sha256 (cdr e))) (funcall entry new)))
                convert))))
    (dolist (rel (pos-migrate--walk stage))
      (when (and (file-regular-p (expand-file-name rel stage))
                 (not (assoc rel convert))
                 (not (assoc rel (seq-filter (lambda (e) (assq 'cid (cdr e))) known))))
        (push (cons rel (funcall entry rel)) add)))
    (let ((bytes (pos-ledger-json
                  `((schema . 2) (kind . "conversion") (previous . ,(or head :null))
                    (ledger_id . ,(or (pos-seal--last-id files) ledger-id (pos-seal--uuid)))
                    (remove . ,(vconcat remove)) (rename . ,(sort rename (lambda (a b) (string< (car a) (car b)))))
                    (convert . ,(sort convert (lambda (a b) (string< (car a) (car b)))))
                    (add . ,(sort add (lambda (a b) (string< (car a) (car b)))))
                    (collections . ,(vconcat collections))
                    (root . ,(cdr (assoc "." cids)))))))
      `((name . ,(format "%08d-%s.json" (1+ events) (pos-ledger--sha bytes)))
        (text . ,(decode-coding-string bytes 'utf-8))))))

;;;; Application

(defun pos-migrate--rebuild (plan build)
  "Rebuild PLAN's archive in BUILD, as planned; refuse any difference."
  (let-alist plan
    (pcase-let ((`(,discard ,remove ,renames ,collections ,declarations)
                 (pos-migrate--prepare .archive build (append .exclude nil)
                                       (append .include nil))))
      (unless (equal (pos-ledger-json
                      `((discard . ,(vconcat discard)) (remove . ,(vconcat remove))
                        (renames . ,(vconcat (mapcar (lambda (r) `((from . ,(car r)) (to . ,(cdr r))))
                                                     renames)))
                        (collections . ,(vconcat collections))
                        (declarations . ,(vconcat (mapcar (lambda (d) `((readme . ,(car d))
                                                                        (kind . ,(cdr d))))
                                                          declarations)))))
                     (pos-ledger-json
                      `((discard . ,.discard) (remove . ,.remove) (renames . ,.renames)
                        (collections . ,.collections) (declarations . ,.declarations))))
        (pos-ledger--refuse 'plan "Archive differs from its plan: %s" .archive)))
    (seq-doseq (r .rumours)
      (let-alist r
        (let ((there (expand-file-name .destination build)))
          (unless (file-exists-p there)
            (make-directory (file-name-directory there) t)
            (pos-migrate--write-over there (encode-coding-string .text 'utf-8))))))
    (dolist (rel (seq-uniq (mapcar (lambda (l) (alist-get 'file l)) .links)))
      (let ((edits (delq nil (mapcar (lambda (l) (let-alist l
                                                   (and (equal .file rel) (not (equal .from .to))
                                                        (list .offset .from .to))))
                                     .links)))
            (file (expand-file-name rel build)))
        (when edits
          (pos-migrate--write-over file (pos-links-rewrite (pos-ledger--read file) edits)))))
    (unless (equal (pos-ledger--sha (pos-ledger-json (pos-ledger-inventory build)))
                   .final_inventory_sha256)
      (pos-ledger--refuse 'plan "Rebuilt archive differs from its plan: %s" .archive))))

(defun pos-migrate--protect (dir)
  "Remove the write bits of every file under DIR."
  (dolist (file (directory-files-recursively dir "" nil))
    (unless (file-symlink-p file)
      (set-file-modes file (logand (file-modes file) (lognot #o222))))))

(defun pos-migrate-apply (plan expected)
  "Apply PLAN, whose canonical JSON has the SHA-256 EXPECTED.
Rebuild the archive as planned, swap it in, move its ledger beside it,
close the ledger with the conversion event and repair canon.  Resume if
interrupted.  Return (EVENT-FILE . ROOT)."
  (unless (equal (pos-ledger--sha (pos-ledger-json plan)) expected)
    (pos-ledger--refuse 'plan "Reviewed plan hash mismatch"))
  (let-alist plan
    (unless (and (eql .schema 1) (equal .operation "migrate"))
      (pos-ledger--refuse 'plan "Not a migration plan"))
    (let* ((scope (file-name-directory .archive))
           (work (expand-file-name (concat "_migrate/" (file-name-nondirectory .archive)) scope))
           (build (expand-file-name "build" work))
           (previous (expand-file-name "previous" work))
           (ledger (expand-file-name (concat pos-ledger-integrity "/ledger") scope))
           (checkpoints (expand-file-name (concat pos-ledger-integrity "/checkpoints") scope))
           (event (expand-file-name .event.name ledger))
           (bytes (encode-coding-string .event.text 'utf-8)))
      ;; 1. Rebuild, then swap: previous holds the archive as it was.
      (unless (file-exists-p previous)
        (unless (equal (pos-ledger--sha (pos-ledger-json (pos-ledger-inventory .archive)))
                       .inventory_sha256)
          (pos-ledger--refuse 'plan "Archive changed since review: %s" .archive))
        (pos-migrate--rebuild plan build)
        (rename-file .archive previous))
      (when (file-exists-p build)
        (rename-file build .archive))
      ;; 2. The ledger and checkpoints, beside the archive.
      (let ((legacy (expand-file-name pos-ledger-directory previous))
            (anchors (expand-file-name pos-ledger-anchors scope)))
        (when (file-exists-p legacy)
          (make-directory (file-name-directory ledger) t)
          (rename-file legacy ledger))
        (when (file-exists-p anchors)
          (make-directory (file-name-directory checkpoints) t)
          (if (file-exists-p checkpoints)
              (dolist (f (directory-files anchors t directory-files-no-dot-files-regexp))
                (rename-file f (expand-file-name (file-name-nondirectory f) checkpoints)))
            (rename-file anchors checkpoints))
          (when (file-exists-p anchors) (delete-directory anchors))))
      ;; 3. The conversion event.
      (unless (file-exists-p event)
        (pos-seal--write-new event bytes))
      ;; 4. Write bits, canon, checkpoint.
      (pos-migrate--protect .archive)
      (seq-doseq (c .canon)
        (let ((root .root))
         (let-alist c
          (let* ((file (expand-file-name .file root)) (now (pos-ledger--read file)))
            (cond ((equal (pos-ledger--sha now) .sha256)
                   (pos-migrate--write-over
                    file (pos-links-rewrite now (mapcar (lambda (e) (let-alist e (list .offset .from .to)))
                                                        .edits))))
                  ((seq-every-p (lambda (e) (let-alist e
                                              (string-match-p (regexp-quote .to)
                                                              (decode-coding-string now 'utf-8))))
                                .edits))
                  (t (pos-ledger--refuse 'plan "Canon changed since review: %s" file)))))))
      (pos-seal--checkpoint .archive (pos-ledger--sha bytes))
      (cons event (alist-get 'root (pos-ledger--parse bytes))))))

;;;; Command line

(defun pos-migrate-batch ()
  "Run a migration command from `command-line-args-left'.
plan ARCHIVE [ROOT [DIR...]] prints a plan, each DIR excluding a
candidate collection, or with a leading + including one; apply PLAN HASH
applies it.  Exit 0 done, 2 refused."
  (condition-case err
      (pcase (prog1 command-line-args-left (setq command-line-args-left nil))
        (`("plan" ,archive . ,rest)
         ;; After ROOT, +DIR includes a collection and DIR excludes one.
         (let ((names (cdr rest)))
           (princ (decode-coding-string
                   (pos-ledger-json
                    (pos-migrate-plan
                     archive (car rest)
                     (seq-remove (lambda (n) (string-prefix-p "+" n)) names) nil nil
                     (mapcar (lambda (n) (substring n 1))
                             (seq-filter (lambda (n) (string-prefix-p "+" n)) names))))
                   'utf-8))))
        (`("apply" ,plan-file ,hash)
         (let ((result (pos-migrate-apply (pos-ledger--parse (pos-ledger--read plan-file)) hash)))
           (princ (decode-coding-string
                   (pos-ledger-json `((event . ,(car result)) (root . ,(cdr result))))
                   'utf-8))))
        (_ (message "Usage: plan ARCHIVE [ROOT [DIR... +DIR...]] | apply PLAN HASH")
           (kill-emacs 2)))
    (pos-ledger-refused
     (message "%s: %s" (nth 1 err) (nth 2 err))
     (kill-emacs 2))
    (json-error
     (message "plan: Not a readable plan")
     (kill-emacs 2))))

(provide 'pos-migrate)
;;; pos-migrate.el ends here
