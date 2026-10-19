;;; pos-keep.el --- An archive kept by a keeper, and brought back  -*- lexical-binding: t; -*-

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

;; An archive may be kept by a keeper rather than on disk beside its
;; ledger, as doc/formats.org and doc/remote-archive-protocol.txt
;; specify.  A keeper keeps a ledger of schema 3, so a ledger of
;; schema 2 is first brought to schema 3 by one conversion event.
;; Then the archive is moved to its keeper, each event sent with the
;; files it enrolled, and removed from disk; and brought back, each
;; file read from the keeper and held to the CID the ledger enrols it
;; under.  Each of the three considers every archive under a root
;; before it acts on any, so that a refusal comes before anything is
;; written or sent, and each resumes if interrupted.
;;
;; - `pos-keep-convert': bring schema 2 ledgers to schema 3.
;; - `pos-keep': move archives on disk to their keepers.
;; - `pos-keep-recall': bring kept archives back to disk.
;; - `pos-keep-relocate-plan', `pos-keep-relocate': the plan to move
;;   each scope's archive and bookkeeping into its selected directory,
;;   and the move, by renames and with no byte rewritten.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pos-bytes)
(require 'pos-checkpoint)
(require 'pos-cid)
(require 'pos-git)
(require 'pos-ledger)
(require 'pos-node)
(require 'pos-remote)
(require 'pos-seal)
(require 'pos-tree)

;;;; Over the archives of a root

(defun pos-keep--each-archive (root key consider act)
  "Return an alist of KEY, the archives under ROOT acted on, and skipped.
CONSIDER is called with each archive and its history, and returns a
reason to skip it, a string, or what ACT needs, a list, having refused
anything that would stop the whole.  Every archive is considered
before any is acted on, so that a refusal comes before anything is
written or sent.  ACT is then called with each archive not skipped and
what CONSIDER returned for it, and returns the archive's row."
  (let (rows skipped pending)
    (dolist (archive (pos-ledger-roots root))
      (let ((considered (funcall consider archive (pos-ledger-history archive))))
        (if (stringp considered)
            (push `((archive . ,archive) (reason . ,considered)) skipped)
          (push (cons archive considered) pending))))
    (pcase-dolist (`(,archive . ,found) (nreverse pending))
      (push (apply act archive found) rows))
    `((,key . ,(vconcat (nreverse rows)))
      (skipped . ,(vconcat (nreverse skipped))))))

;;;; Conversion

(defun pos-keep-convert (root)
  "Bring each schema 2 ledger under ROOT to schema 3, by one event.
The event links the head as a block, names the hash it had, and enrols
the empty directories the archive holds, so that the fold of the ledger
is the archive's CID.  Refused unless every archive is as its ledger
says, before any event is written.  Return an alist: converted, each an
archive with its event file and new head; and skipped, each an archive
with the reason."
  (pos-keep--each-archive
   root 'converted
   (lambda (archive history)
     (pcase-let (((cl-struct pos-history entries head events files (root recorded))
                  history))
       (cond
        ((null head) "no ledger")
        ((pos-ledger-event-cid-p head) "schema 3")
        ((seq-some (lambda (e) (not (assq 'cid (cdr e)))) entries) "legacy entries")
        (t
         (pcase-let (((cl-struct pos-comparison missing changed new cids)
                      (pos-ledger-against-disk archive entries)))
           (when (or missing changed new
                     (not (equal recorded (cdr (assoc "." cids)))))
             (pos-ledger-refuse
              'unclean "The archive is not as its ledger says; check it first: %s"
              archive)))
         (let* ((empty (pos-seal-empty-directories archive ""))
                (folded (cdr (assoc "." (pos-ledger-fold entries empty)))))
           (unless (equal folded recorded)
             (pos-ledger-refuse
              'root "The ledger does not account for the archive's CID: %s" archive))
           (let* ((head-file (car (last files)))
                  (bytes (pos-bytes-block
                          `((schema . 3) (kind . "conversion") (from . ,head)
                            (previous . ,(pos-ledger-link
                                          (pos-ledger-event-cid
                                           (pos-ledger-as-block
                                            (pos-bytes-read head-file)))))
                            (ledger_id . ,(pos-ledger-identity files))
                            (empty . ,(vconcat empty)) (root . ,folded))))
                  (name (pos-ledger-event-cid bytes))
                  (file (expand-file-name (pos-ledger-event-file-name (1+ events) name)
                                          (file-name-directory head-file))))
             (list file bytes name)))))))
   (lambda (archive file bytes name)
     (pos-ledger-write-new file bytes)
     (pos-ledger-checkpoint-head archive name)
     `((archive . ,archive) (event . ,file) (head . ,name)))))

;;;; Keeping an existing archive

(defun pos-keep--added-paths (files entries)
  "Return (ADDED . FIRST) for a ledger of event FILES enrolling ENTRIES.
ADDED is, for each event in order, the paths it enrolled as the ledger
now has them: an event before a schema 2 conversion enrolled paths that
conversion may have renamed or removed.  FIRST is the index of the first
schema 3 event, or nil."
  (let* ((values (mapcar (lambda (file) (pos-bytes-parse (pos-bytes-read file)))
                         files))
         (conversion (seq-position values nil
                                   (lambda (value _)
                                     (and (eql (alist-get 'schema value) 2)
                                          (equal (alist-get 'kind value) "conversion")))))
         (rename (and conversion
                      (mapcar (lambda (pair) (cons (pos-bytes-key (car pair)) (cdr pair)))
                              (alist-get 'rename (nth conversion values)))))
         (index -1))
    (cons (mapcar
           (lambda (value)
             (setq index (1+ index))
             (let ((paths (mapcar (lambda (pair) (pos-bytes-key (car pair)))
                                  (alist-get 'add value))))
               (when (and conversion (< index conversion))
                 (setq paths (mapcar (lambda (path) (or (cdr (assoc path rename)) path))
                                     paths)))
               (seq-filter (lambda (path) (assoc path entries)) paths)))
           values)
          (seq-position values nil
                        (lambda (value _) (eql (alist-get 'schema value) 3))))))

(defun pos-keep--check-keepable (archive history)
  "Refuse unless ARCHIVE, whose ledger is HISTORY, may go to its keeper.
Its ledger is the one its entry names (identity), is of schema 3 (kept)
and lies beside it (ledger); the archive is as the ledger says
\(unclean); and no enrolled path is hidden (hidden)."
  (pcase-let (((cl-struct pos-history entries head files (root recorded) empty)
               history))
    (pos-ledger-as-named archive files)
    (unless (pos-ledger-event-cid-p head)
      (pos-ledger-refuse
       'kept "A keeper keeps a ledger of schema 3; convert this one first: %s"
       archive))
    (when (string-prefix-p (file-name-as-directory archive) (car files))
      (pos-ledger-refuse
       'ledger "A kept archive's ledger lies beside it, not inside: %s" archive))
    (pcase-let (((cl-struct pos-comparison missing changed new cids)
                 (pos-ledger-against-disk archive entries)))
      (when (or missing changed new
                (not (equal recorded (cdr (assoc "." cids))))
                (not (equal recorded
                            (cdr (assoc "." (pos-ledger-fold entries empty))))))
        (pos-ledger-refuse
         'unclean "The archive is not as its ledger says; check it first: %s"
         archive))
      (when (seq-some (lambda (pair)
                        (seq-some (lambda (part) (string-prefix-p "." part))
                                  (split-string (car pair) "/")))
                      entries)
        (pos-ledger-refuse
         'hidden "A keeper holds no hidden file, and this ledger enrols one: %s"
         archive)))))

(defun pos-keep--send-archive (archive url history)
  "Send ARCHIVE, whose ledger is HISTORY, to the keeper at URL; remove it.
The keeper is sent each event it lacks with the files the event
enrolled, as `pos-keep' says.  Refuse `chain' if the keeper holds
another ledger, or does not hold this one once all is sent.  Return
the archive's row among those kept."
  (pcase-let* (((cl-struct pos-history entries head files (root recorded)) history)
               (keeper (funcall pos-remote-keeper-function url))
               (claims (funcall pos-seal-claims-function `((archive . ,archive)) nil))
               (described (pos-remote-describe keeper))
               (version (pos-remote-version described))
               (held (alist-get 'events described))
               (names (mapcar #'file-name-nondirectory files))
               (events (cl-mapcar (lambda (name file) (cons name (pos-bytes-read file)))
                                  names files))
               (sent nil))
    (when (or (> held (length files))
              (and (> held 0)
                   (not (equal (alist-get 'head described)
                               (pos-ledger-event-file-id (nth (1- held) names))))))
      (pos-ledger-refuse 'chain "The keeper holds another ledger than this one: %s"
                         archive))
    (pcase-let* ((`(,added . ,first) (pos-keep--added-paths files entries))
                 (batch-of
                  (lambda (paths)
                    (let (batch)
                      (dolist (path paths)
                        (let ((cid (alist-get 'cid (cdr (assoc path entries)))))
                          (unless (or (member cid sent) (assoc cid batch))
                            (push (cons cid (pos-bytes-read
                                             (expand-file-name path archive)))
                                  batch))))
                      batch))))
      (dotimes (i (length files))
        (when (>= i held)
          (let ((batch (funcall batch-of (nth i added)))
                (event (cdr (nth i events)))
                (vouching (pos-remote-following events (1+ i))))
            (condition-case err
                (pos-seal-send-event keeper version (nth i names) event batch claims vouching)
              (pos-ledger-refused
               ;; The keeper took an earlier event without its files:
               ;; they go with the event at which it requires them.
               (unless (and (eq (cadr err) 'entry) (eql i first) (> held 0))
                 (signal (car err) (cdr err)))
               (setq batch (funcall batch-of (apply #'append (seq-take added (1+ i)))))
               (pos-seal-send-event keeper version (nth i names) event batch claims vouching)))
            (setq sent (append (mapcar #'car batch) sent))))))
    (let ((now (pos-remote-describe keeper)))
      (unless (and (equal head (alist-get 'head now))
                   (equal (length files) (alist-get 'events now))
                   (equal recorded (alist-get 'root now)))
        (pos-ledger-refuse 'chain "The keeper does not hold this ledger as it is: %s"
                           archive)))
    (delete-directory archive t)
    `((archive . ,archive) (keeper . ,url)
      (events . ,(- (length files) held)) (files . ,(length sent)))))

(defun pos-keep (root)
  "Move to its keeper each archive under ROOT that is to be kept by one.
That is, each whose scope's entry gives it to a keeper and whose files
are still on disk.  The keeper is sent the events it lacks, in order,
each with the files it enrolled: inside the append under version 1 of
the protocol, put as blocks first under version 2.  A keeper that holds
earlier events is
taken to hold their files; if it refuses the first schema 3 event for
want of them, that event is sent again with every file enrolled before
it, since it is where a keeper requires them.  A first event that names
no ledger, as those of a ledger begun before events carried a
ledger_id, is sent with the events after it up to the first that names
one, by which the keeper knows whose it is (`pos-remote-following').
Once the keeper holds the ledger's head and its root, the archive is
removed from disk.
Refused unless
every such archive is as its ledger says, with a ledger of schema 3
beside it and nothing hidden, before anything is sent.  Interrupted, it
resumes.  Return an alist: kept, each an archive with its keeper and
the events and files sent; and skipped, each an archive with the
reason."
  (pos-keep--each-archive
   root 'kept
   (lambda (archive history)
     (let ((url (pos-ledger-kept archive)))
       (cond ((null url) "on disk")
             ((null (pos-history-head history)) "no ledger")
             ((null (pos-ledger-inventory archive)) "kept")
             (t (pos-keep--check-keepable archive history)
                (list url history)))))
   #'pos-keep--send-archive))

;;;; Recall

(defun pos-keep--check-recallable (archive history)
  "Return the paths HISTORY enrols that ARCHIVE lacks on disk.
Refuse unless ARCHIVE's ledger is the one its entry names (identity),
is of schema 3 (kept) and folds to its recorded root (root), and what
is on disk already is enrolled and unchanged (differs)."
  (pcase-let (((cl-struct pos-history entries head files (root recorded) empty)
               history))
    (pos-ledger-as-named archive files)
    (unless (pos-ledger-event-cid-p head)
      (pos-ledger-refuse 'kept "A keeper keeps a ledger of schema 3: %s" archive))
    (unless (equal recorded (cdr (assoc "." (pos-ledger-fold entries empty))))
      (pos-ledger-refuse
       'root "The ledger does not fold to its recorded root: %s" archive))
    (pcase-let (((cl-struct pos-comparison missing changed new)
                 (pos-ledger-against-disk archive entries)))
      (when (or changed new)
        (pos-ledger-refuse
         'differs "What is on disk is not what the ledger enrols: %s" archive))
      missing)))

(defun pos-keep--fetch-archive (archive url history missing)
  "Write in ARCHIVE each of the MISSING paths, read from the keeper at URL.
HISTORY is ARCHIVE's ledger.  Each file is held to the CID the ledger
enrols it under, and the empty directories the ledger records are
made.  Refuse `chain' if the keeper does not hold the ledger as it is,
`entry' for bytes that are not their CID's, and `root' if the archive
is not then its recorded root.  Return the archive's row among those
recalled."
  (pcase-let* (((cl-struct pos-history entries head files (root recorded) empty)
                history)
               (keeper (funcall pos-remote-keeper-function url))
               (described (pos-remote-describe keeper))
               (read nil))
    (unless (and (equal head (alist-get 'head described))
                 (equal (length files) (alist-get 'events described))
                 (equal recorded (alist-get 'root described)))
      (pos-ledger-refuse 'chain "The keeper does not hold this ledger as it is: %s"
                         archive))
    (dolist (path missing)
      (let* ((entry (cdr (assoc path entries)))
             (cid (alist-get 'cid entry)))
        (unless (assoc cid read)
          (let ((bytes (pos-remote-read keeper cid)))
            (unless (equal cid (pos-cid-bytes bytes))
              (pos-ledger-refuse 'entry "The keeper's bytes are not those of %s" cid))
            (push (cons cid bytes) read)))
        (pos-ledger-write-new (expand-file-name path archive)
                            (cdr (assoc cid read)) (alist-get 'mode entry))))
    (dolist (path empty)
      (make-directory (expand-file-name path archive) t))
    (make-directory archive t)
    (unless (equal recorded (cdr (assoc "." (pos-cid-tree archive))))
      (pos-ledger-refuse
       'root "The archive brought back is not its recorded root: %s" archive))
    `((archive . ,archive) (keeper . ,url) (files . ,(length read)))))

(defun pos-keep-recall (root)
  "Bring back to disk each archive under ROOT that is with a keeper.
The reverse of `pos-keep'.  Every file the ledger enrols and the
disk lacks is read from the keeper, by the CID the ledger enrols it
under, held to that CID, and written at its path with its recorded
mode; the empty directories the ledger records are made.  Refused
before anything is asked unless the ledger is of schema 3 and folds to
its recorded root, and what is on disk already is enrolled and
unchanged.  The keeper must hold the ledger as it is.  Interrupted, it
resumes.  The archive is then on disk as its ledger says, and still its
keeper's by its entry, the state `pos-keep' begins from.  Return
an alist: recalled, each an archive with its keeper and the files read;
and skipped, each an archive with the reason."
  (pos-keep--each-archive
   root 'recalled
   (lambda (archive history)
     (let ((url (pos-ledger-kept archive)))
       (cond ((null url) "on disk")
             ((null (pos-history-head history)) "no ledger")
             (t (list url history (pos-keep--check-recallable archive history))))))
   #'pos-keep--fetch-archive))

;;;; Relocation

(defun pos-keep--repository (dir)
  "Return the top of the repository holding DIR, or nil."
  (when-let* ((top (locate-dominating-file dir ".git")))
    (directory-file-name (expand-file-name top))))

(defun pos-keep--scopes (root)
  "Return (SCOPES . REPOSITORIES): the active scopes and repositories under ROOT.
SCOPES are the directories the outermost walk of discovery visits,
ROOT included: no hidden or underscored directory, no archives
directory, no symbolic link.  A directory below ROOT holding .git is
another repository: it is not entered, and is among REPOSITORIES."
  (let ((root (file-truename (pos-ledger-checked root))) scopes repositories)
    (unless (file-directory-p root)
      (pos-ledger-refuse 'root "Root must be an existing directory"))
    (named-let walk ((dir root))
      (push dir scopes)
      (dolist (name (pos-ledger-entry-names dir))
        (let ((path (expand-file-name name dir)))
          (when (and (file-directory-p path) (not (file-symlink-p path))
                     (not (string-match-p "\\`[._]" name))
                     (not (equal name "archives")))
            (if (file-exists-p (expand-file-name ".git" path))
                (push path repositories)
              (walk path))))))
    (cons (sort scopes #'pos-ledger--path<) (sort repositories #'pos-ledger--path<))))

(defun pos-keep--tracked-p (repository path)
  "Return non-nil if the REPOSITORY tracks a file at or beneath PATH."
  (and repository
       (not (string-empty-p
             (or (pos-git-line repository "ls-files" "--" (file-relative-name path repository))
                 "")))))

(defun pos-keep--untracked (repository path)
  "Return the files at or beneath PATH that REPOSITORY does not track.
Absolute paths, sorted; ignored files among them."
  (let ((out (pos-git-line repository "ls-files" "--others" "--"
                           (file-relative-name path repository))))
    (sort (mapcar (lambda (line) (expand-file-name line repository))
                  (and out (split-string out "\n" t)))
          #'string<)))

(defun pos-keep--blocker (reason paths format &rest args)
  "Return a blocker of REASON naming PATHS, with a message from FORMAT and ARGS."
  `((reason . ,(format "%s" reason))
    (paths . ,(vconcat (sort (copy-sequence paths) #'string<)))
    (message . ,(apply #'format format args))))

(defun pos-keep--renames (scope selected layout)
  "Return the renames that bring SCOPE's bookkeeping into SELECTED.
SELECTED is the name of the directory selected at SCOPE.  Each rename
is (FROM . TO): an archive, an archive-integrity directory or the
legacy checkpoint directory of LAYOUT, a `pos-layout', that lies
elsewhere."
  (let* ((base (expand-file-name selected scope))
         (archive (expand-file-name "archives" base))
         (integrity (expand-file-name pos-ledger-integrity base)))
    (delq nil
          (mapcar (lambda (path)
                    (cond ((member path (list archive integrity)) nil)
                          ((equal (file-name-nondirectory path) "archives")
                           (cons path archive))
                          ((equal (file-name-nondirectory path) pos-ledger-anchors)
                           (cons path (expand-file-name "checkpoints" integrity)))
                          (t (cons path integrity))))
                  (pos-layout-components layout)))))

(defun pos-keep--check-scope (layout)
  "Return the check report on the archive of LAYOUT, a `pos-layout', or nil.
No keeper is asked.  Nil for a scope with no archive and no ledger."
  (let ((archive (or (pos-layout-archive layout)
                     (and (pos-layout-ledger layout)
                          (pos-ledger-paired-archive (pos-layout-ledger layout))))))
    (when archive
      (let ((pos-ledger-offline t))
        (pos-ledger-check archive)))))

(defun pos-keep--plan-scope (scope root)
  "Return the relocation of SCOPE, an alist, or nil if it has nothing to move.
Paths are relative to ROOT.  The alist has scope, selected, renames,
each with from, to and move, git for Git's move of a path the
repository tracks and rename for the file system's; untracked, the
files Git does not track in what moves, but an archive no file of
which is tracked, sorted; and blockers, each with reason, paths and
message.  Each move takes a directory whole, its untracked files with
it, still untracked."
  (let* ((rel (lambda (path) (directory-file-name (file-relative-name path root))))
         (repository (pos-keep--repository scope))
         blockers renames selected untracked)
    (condition-case err
        (let ((layout (pos-ledger-locations scope)))
          (when (pos-layout-components layout)
            (setq selected (pos-ledger-select scope))
            (pcase-dolist (`(,from . ,to) (pos-keep--renames scope selected layout))
              (when (or (file-exists-p to) (file-symlink-p to))
                (push (pos-keep--blocker 'destination (list (funcall rel from) (funcall rel to))
                                         "The destination exists")
                      blockers))
              (unless (and (file-writable-p (file-name-directory from))
                           (file-writable-p (pos-keep--writable-parent to)))
                (push (pos-keep--blocker 'access (list (funcall rel from))
                                         "The directory is not writable")
                      blockers))
              (let ((tracked (pos-keep--tracked-p repository from)))
                (when (and repository
                           (or tracked (not (equal (file-name-nondirectory from) "archives"))))
                  (setq untracked (append untracked
                                          (mapcar rel (pos-keep--untracked repository from)))))
                (push `((from . ,(funcall rel from)) (to . ,(funcall rel to))
                        (move . ,(if tracked "git" "rename")))
                      renames)))
            (let ((ledger (pos-layout-ledger layout)))
              (if (and renames ledger
                       (equal (file-name-nondirectory ledger) pos-ledger-directory))
                  (push (pos-keep--blocker 'legacy-ledger (list (funcall rel ledger))
                                           "A legacy ledger inside the archive: migrate it first")
                        blockers)
                (when renames
                  (condition-case err
                      (let ((report (pos-keep--check-scope layout)))
                        (when (and report (pos-checkpoint-findings-p report))
                          (push (pos-keep--blocker
                                 'unclean (mapcar (lambda (a) (funcall rel (alist-get 'archive a)))
                                                  report)
                                 "The check has findings")
                                blockers)))
                    (pos-ledger-refused
                     (push (pos-keep--blocker (nth 1 err) (list (funcall rel scope))
                                              "%s" (nth 2 err))
                           blockers))))))))
      (pos-ledger-refused
       (push (pos-keep--blocker (nth 1 err) (mapcar rel (nth 4 err)) "%s" (nth 2 err))
             blockers)))
    (when (or renames blockers)
      `((scope . ,(funcall rel scope))
        (selected . ,(or selected :null))
        (renames . ,(vconcat (nreverse renames)))
        (untracked . ,(vconcat (sort untracked #'string<)))
        (blockers . ,(vconcat (nreverse blockers)))))))

(defun pos-keep--writable-parent (path)
  "Return the nearest existing directory at or above PATH's parent."
  (let ((dir (file-name-directory (directory-file-name path))))
    (while (not (file-exists-p dir))
      (setq dir (file-name-directory (directory-file-name dir))))
    dir))

(defun pos-keep--mount-blockers (scope root)
  "Return a blocker for each repository SCOPE declares that is not there.
SCOPE's configuration declares children; each with a remote is a
repository mounted at its path, and one whose path is not a directory
blocks relocation (mount), since the walk cannot reach its scopes.
Paths are relative to ROOT.  A configuration that cannot be read
declares nothing here."
  (let ((file (ignore-errors (pos-node-config-file scope))))
    (when file
      (let ((config (ignore-errors
                      (pos-node-read-config
                       (decode-coding-string
                        (pos-bytes-read (expand-file-name file scope)) 'utf-8)))))
        (delq nil
              (mapcar (lambda (child)
                        (let ((path (expand-file-name (alist-get 'path child) scope)))
                          (when (and (stringp (alist-get 'remote child))
                                     (or (file-symlink-p path)
                                         (not (file-directory-p path))))
                            (pos-keep--blocker
                             'mount (list (directory-file-name (file-relative-name path root)))
                             "A declared repository is not there: clone or fetch it, \
or remove its entry"))))
                      (alist-get 'children config)))))))

(defun pos-keep--with-mounts (entry scope root)
  "Return ENTRY, the relocation of SCOPE, with its mount blockers added.
ENTRY may be nil; with mount blockers, an entry is made.  Paths are
relative to ROOT."
  (let ((mounts (pos-keep--mount-blockers scope root)))
    (cond ((null mounts) entry)
          (entry (setf (alist-get 'blockers entry)
                       (vconcat (alist-get 'blockers entry) mounts))
                 entry)
          (t `((scope . ,(directory-file-name (file-relative-name scope root)))
               (selected . :null) (renames . []) (untracked . [])
               (blockers . ,(vconcat mounts)))))))

(defun pos-keep-relocate-plan (root)
  "Return the plan to move each scope's bookkeeping under ROOT into place.
An alist: root; scopes, each as `pos-keep--plan-scope' gives it, in
path order, for the scopes with something at an old location or a
blocker, a declared repository that is not there among them;
repositories, the other repositories below ROOT, each to be relocated
by a run of its own; and blocked, the number of blockers.
Nothing is written, and no keeper is asked."
  (let* ((root (file-name-as-directory (file-truename (expand-file-name root))))
         (found (pos-keep--scopes root))
         (scopes (delq nil (mapcar (lambda (scope)
                                     (pos-keep--with-mounts
                                      (pos-keep--plan-scope scope root) scope root))
                                   (car found)))))
    `((root . ,(directory-file-name root))
      (scopes . ,(vconcat scopes))
      (repositories . ,(vconcat (mapcar (lambda (r) (directory-file-name
                                                     (file-relative-name r root)))
                                        (cdr found))))
      (blocked . ,(apply #'+ (mapcar (lambda (s) (length (alist-get 'blockers s)))
                                     scopes))))))

(defun pos-keep--summary (report)
  "Return what must not change in a relocation, of the check REPORT."
  (mapcar (lambda (a) (let-alist a (list .head .events .files .root .recorded_root)))
          report))

(defun pos-keep-relocate (root)
  "Move each scope's bookkeeping under ROOT into its selected directory.
Refuse `blocked' if the plan has any blocker, before anything moves.
Each scope in path order: each rename is Git's move where the
repository tracks the path, else the file system's; then the scope is
checked, asking no keeper, and must report what it reported before;
then the managed archive exclusions of its repository, if it has
them, are made again.  Nothing is committed.  Return an alist: scopes,
each with scope, moved and check; and repositories, as the plan
gives them."
  (let* ((plan (pos-keep-relocate-plan root))
         (root (file-name-as-directory (alist-get 'root plan)))
         done)
    (unless (zerop (alist-get 'blocked plan))
      (pos-ledger-refuse
       'blocked "Relocation blocked: %s"
       (mapconcat (lambda (s)
                    (mapconcat (lambda (b) (format "%s %s" (alist-get 'reason b)
                                                   (string-join (alist-get 'paths b) " ")))
                               (alist-get 'blockers s) "; "))
                  (seq-filter (lambda (s) (> (length (alist-get 'blockers s)) 0))
                              (alist-get 'scopes plan))
                  "; ")))
    (seq-doseq (entry (alist-get 'scopes plan))
      (let* ((scope (expand-file-name (alist-get 'scope entry) root))
             (repository (pos-keep--repository scope))
             (before (pos-keep--summary (pos-keep--check-scope (pos-ledger-locations scope)))))
        (seq-doseq (rename (alist-get 'renames entry))
          (let-alist rename
            (let ((from (expand-file-name .from root)) (to (expand-file-name .to root)))
              (make-directory (file-name-directory to) t)
              (if (equal .move "git")
                  (let ((result (pos-git repository "mv" "--"
                                         (file-relative-name from repository)
                                         (file-relative-name to repository))))
                    (unless (eq 0 (car result))
                      (pos-ledger-refuse 'relocation "Git could not move %s: %s"
                                         .from (cdr result))))
                (rename-file from to)))))
        (let* ((report (pos-keep--check-scope (pos-ledger-locations scope)))
               (after (pos-keep--summary report)))
          (unless (equal before after)
            (pos-ledger-refuse 'unclean "The check differs after the move: %s" scope))
          (let ((excludes (and repository (pos-keep--managed-excludes-p repository))))
            (when excludes
              (pos-tree-ignore-archives repository))
            (push `((scope . ,(alist-get 'scope entry))
                    (moved . ,(alist-get 'renames entry))
                    (check . ,(vconcat (mapcar (lambda (a)
                                                 (let-alist a
                                                   `((archive . ,(directory-file-name
                                                                  (file-relative-name .archive root)))
                                                     (head . ,.head) (root . ,.root))))
                                               report)))
                    (excludes . ,(if excludes "regenerated" :null)))
                  done)))))
    `((scopes . ,(vconcat (nreverse done)))
      (repositories . ,(alist-get 'repositories plan)))))

(defun pos-keep--managed-excludes-p (repository)
  "Return non-nil if REPOSITORY's info/exclude has a managed archive block."
  (let ((file (pos-tree--exclude-file repository)))
    (and file (file-regular-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (pos-tree--archive-block (buffer-string))))))

(provide 'pos-keep)
;;; pos-keep.el ends here
