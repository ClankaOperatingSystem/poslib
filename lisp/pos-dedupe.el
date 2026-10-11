;;; pos-dedupe.el --- Resolve tasks held in more than one place  -*- lexical-binding: t -*-

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

;; Tasks with the same heading in more than one place of a tree's Org
;; files, resolved by a plan that is reviewed and then applied.  The
;; plan is Org text: a table for each group of copies, each copy marked
;; keep, drop or ?.  Applying it cuts each dropped copy, merges a body
;; that differs under the kept copy, and points links by ID at the
;; kept copy.
;;
;; - `pos-dedupe-groups', `pos-dedupe-suggest': the groups of copies,
;;   and what is suggested for each copy of one.
;; - `pos-dedupe-plan', `pos-dedupe-apply': the two steps.
;; - `pos-dedupe-read-plan': a reviewed plan, read from its file.
;; - `pos-dedupe-report': what applying a plan did, in a sentence.
;; - `pos-dedupe-plan-batch', `pos-dedupe-apply-batch': the command
;;   line.

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'cl-lib)
(require 'pos)
(require 'pos-roam)
(require 'pos-corpus)

(defun pos--subtree-body ()
  "Return the subtree at point below its heading, trimmed.
A property drawer is not body: two copies alike but for their IDs say
the same thing."
  (save-excursion
    (let* ((end (save-excursion (org-end-of-subtree t t) (point)))
           (start (min end (line-beginning-position 2)))
           (planning (progn (goto-char start)
                            (when (looking-at-p org-planning-line-re) (forward-line 1))
                            (buffer-substring-no-properties start (point))))
           (rest (progn (when (looking-at org-property-drawer-re)
                          (goto-char (min end (1+ (match-end 0)))))
                        (buffer-substring-no-properties (point) end))))
      (string-trim (concat planning rest)))))

(defun pos--task-entries (root)
  "Return a plist per keyword heading in covered files under ROOT.
Keys: :key :heading :file :line :parent :body :id."
  (mapcan (lambda (file)
            (pos-map-headings
             file
             (lambda ()
               (when (org-get-todo-state)
                 (let ((heading (string-trim (org-get-heading t t t t))))
                   (list :key (downcase heading)
                         :heading heading
                         :file file
                         :line (line-number-at-pos)
                         :parent (car (last (org-get-outline-path)))
                         :body (pos--subtree-body)
                         :id (org-entry-get nil "ID")))))))
          (pos-files root)))

(defun pos-dedupe-groups (root)
  "Return groups of duplicate tasks under ROOT."
  (seq-filter (lambda (group) (> (length group) 1))
              (mapcar #'cdr
                      (seq-group-by (lambda (entry) (plist-get entry :key))
                                    (pos--task-entries root)))))

(defun pos-dedupe-suggest (group root)
  "Return GROUP as (ACTION . ENTRY) pairs; ACTION is keep, drop or ?.
Pillar beats intray (under ROOT); in one file, longer body wins;
two pillar files: ?."
  (let* ((intray (expand-file-name "intray.org" root))
         (pillar (seq-remove (lambda (e) (equal (plist-get e :file) intray)) group))
         (candidates (or pillar group))
         (files (delete-dups (mapcar (lambda (e) (plist-get e :file)) candidates)))
         (survivor
          (cond ((= 1 (length candidates)) (car candidates))
                ((= 1 (length files))
                 (car (sort (copy-sequence candidates)
                            (lambda (a b) (> (length (plist-get a :body))
                                             (length (plist-get b :body)))))))
                (t nil))))
    (mapcar (lambda (entry)
              (cons (cond ((null survivor) "?")
                          ((eq entry survivor) "keep")
                          (t "drop"))
                    entry))
            group)))

(defun pos-dedupe--referrer-lines (root)
  "Return a function of an ID giving \"FILE:LINE\" per link to it under ROOT.
Call within `pos-roam-with-index'."
  (lambda (id)
    (mapcar (lambda (referrer)
              (pcase-let ((`(,file . ,pos) referrer))
                ;; The index names files by their true paths.
                (format "%s:%d" (file-relative-name file (file-truename root))
                        (with-current-buffer (pos-roam-buffer file)
                          (line-number-at-pos pos)))))
            (pos-roam-referrers id))))

(defun pos-dedupe-plan (root &optional groups)
  "Return the dedupe plan for ROOT as Org text.
GROUPS defaults to `pos-dedupe-groups'.  Each group lists, after its
table, the \"id:\" links the index records to each copy that has an ID."
  (pos-roam-with-index root
    (let ((referrers (pos-dedupe--referrer-lines root)))
      (with-temp-buffer
        (insert "#+TITLE: Duplicate tasks\n\n"
                "- Per group: one keep, rest drop; ? skips.\n"
                "- Differing dropped bodies merge under the keep.\n"
                "- Links to a dropped copy's ID are pointed at the keep;\n"
                "  a keep without an ID takes the first dropped one's.\n"
                "- Then: pos dedupe apply.\n")
        (dolist (group (or groups (pos-dedupe-groups root)))
          (insert "\n* " (plist-get (car group) :heading) "\n"
                  "| act | file | line | under | body lines | id |\n|-\n")
          (let (links)
            (dolist (suggestion (pos-dedupe-suggest group root))
              (pcase-let* ((`(,action . ,entry) suggestion)
                           (file (file-relative-name (plist-get entry :file) root))
                           (id (plist-get entry :id)))
                (insert (format "| %s | %s | %d | %s | %d | %s |\n"
                                action file
                                (plist-get entry :line)
                                (or (plist-get entry :parent) "")
                                (if (string-empty-p (plist-get entry :body)) 0
                                  (length (split-string (plist-get entry :body) "\n")))
                                (or id "")))
                (when id
                  (let ((from (funcall referrers id)))
                    (when from
                      (push (format "- Links to %s:%d from %s\n" file (plist-get entry :line)
                                    (string-join from ", "))
                            links))))))
            (apply #'insert (nreverse links))))
        (org-mode)
        (org-table-map-tables #'org-table-align t)
        (buffer-string)))))

(defun pos-dedupe-read-plan (file)
  "Read the plan in FILE as (KEY (ACTION FILE LINE)...) groups."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((groups nil) (current nil))
      (while (not (eobp))
        (cond ((looking-at "^\\* \\(.*\\)$")
               (when current (push (nreverse current) groups))
               (setq current (list (downcase (string-trim (match-string 1))))))
              ((looking-at "^| *\\([^| ]+\\) *| *\\([^|]*?\\) *| *\\([0-9]+\\) *|")
               (push (list (match-string 1) (match-string 2)
                           (string-to-number (match-string 3)))
                     current)))
        (forward-line 1))
      (when current (push (nreverse current) groups))
      (nreverse groups))))

(defun pos--merge-under (drop keep origin)
  "Paste subtree DROP under KEEP, without keyword, headed ORIGIN."
  (progn
    (with-current-buffer (marker-buffer drop)
      (goto-char drop)
      (org-copy-subtree))
    (with-current-buffer (marker-buffer keep)
      (goto-char keep)
      (let ((level (org-current-level)))
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (let ((start (point)))
          (org-paste-subtree (1+ level))
          (goto-char start)
          (org-todo 'none)
          (org-edit-headline origin)
          ;; The merged note is not the task: links to the dropped
          ;; copy's ID go to the kept one, not here.
          (org-entry-delete nil "ID"))))))

(defun pos-dedupe--relink (from to dry-run)
  "Point each \"id:\" link to FROM the index records at TO instead.
With DRY-RUN count only.  Return (COUNT . BUFFERS): the links rewritten,
or that would be, and the buffers it wrote.  Call within
`pos-roam-with-index'; a link no longer where the index says is passed
over."
  (let ((count 0) buffers)
    ;; Later links first, so that a rewrite moves no earlier one.
    (dolist (referrer (reverse (pos-roam-referrers from)))
      (pcase-let* ((`(,file . ,pos) referrer)
                   (buffer (pos-roam-rewrite-link file pos from to dry-run)))
        (when buffer
          (setq count (1+ count))
          (cl-pushnew buffer buffers))))
    (cons count buffers)))

(defun pos-dedupe--jobs (root plan corpus)
  "Read PLAN's groups for ROOT into jobs, each with its copies' markers.
A job is (KEY KEEP . DROPS): KEEP the kept copy's marker, each of
DROPS (MARKER . LABEL) with the label its merged body is filed
under.  A group without one keep and a drop is skipped; one with a
copy in a file CORPUS says ROOT may not write is unwritable; one
whose copies are not at their lines is stale.  Return (JOBS :skipped
N :unwritable N :stale N), JOBS in the plan's order."
  (let ((skipped 0) (unwritable 0) (stale 0) jobs)
    (dolist (group (pos-dedupe-read-plan plan))
      (let* ((key (car group))
             (copies (cdr group))
             (keeps (seq-filter (lambda (c) (equal (car c) "keep")) copies))
             (drops (seq-filter (lambda (c) (equal (car c) "drop")) copies)))
        (cond
         ((not (and (= 1 (length keeps)) drops))
          (setq skipped (1+ skipped)))
         ((seq-some (lambda (c)
                      (not (pos-corpus-writable-p corpus (expand-file-name (nth 1 c) root))))
                    copies)
          (setq unwritable (1+ unwritable)))
         (t
          (let ((markers (mapcar (lambda (c) (pos-marker-at root (nth 1 c) (nth 2 c) key))
                                 (cons (car keeps) drops))))
            (if (memq nil markers)
                (setq stale (1+ stale))
              (push (cons key
                          (cons (car markers)
                                (cl-mapcar (lambda (marker d)
                                             (cons marker (format "Merged copy from %s:%d"
                                                                  (nth 1 d) (nth 2 d))))
                                           (cdr markers) drops)))
                    jobs)))))))
    (list (nreverse jobs) :skipped skipped :unwritable unwritable :stale stale)))

(defun pos-dedupe--relinks-unwritable-p (job root corpus)
  "Return non-nil if resolving JOB would relink a file ROOT may not write.
A dropped copy's ID that does not pass to the kept copy has its links
pointed at the kept copy's; a link the index records in a file CORPUS
says ROOT may not write cannot be.  Call within `pos-roam-with-index'."
  (pcase-let* ((`(,key ,keep . ,drops) job)
               (keep-id (and (pos-marker-still-at-p keep key)
                             (org-entry-get keep "ID")))
               (true-root (file-truename root)))
    (seq-some
     (lambda (drop)
       (when-let* ((drop-id (and (pos-marker-still-at-p (car drop) key)
                                 (org-entry-get (car drop) "ID"))))
         (if (not keep-id)
             (progn (setq keep-id drop-id) nil)
           (seq-some
            (lambda (referrer)
              ;; The index names a file by its true path, the corpus as
              ;; ROOT was given.
              (not (pos-corpus-writable-p
                    corpus
                    (expand-file-name (file-relative-name (car referrer) true-root)
                                      root))))
            (pos-roam-referrers drop-id)))))
     drops)))

(defun pos-dedupe--resolve (job dry-run)
  "Resolve JOB, a group with its markers, as `pos-dedupe--jobs' gives it.
Each dropped copy whose body differs from the kept one's is merged
beneath it under the drop's label, then cut; a dropped ID passes to
the kept copy where it has none, else the links to it are pointed at
the kept copy's.  With DRY-RUN count only.  Return (:resolved 0 or 1
:merged N :relinked N :vanished N :touched BUFFERS): vanished counts
copies no longer at their marker, touched the buffers relinking
wrote."
  (pcase-let ((`(,key ,keep . ,drops) job)
              (resolved 0) (merged 0) (relinked 0) (vanished 0) (touched nil))
    (if (not (pos-marker-still-at-p keep key))
        (setq vanished (1+ vanished))
      (setq resolved 1)
      (let ((keep-body (with-current-buffer (marker-buffer keep)
                         (goto-char keep) (pos--subtree-body)))
            (keep-id (org-entry-get keep "ID")))
        (pcase-dolist (`(,drop . ,label) drops)
          (if (not (pos-marker-still-at-p drop key))
              (setq vanished (1+ vanished))
            (let ((drop-body (with-current-buffer (marker-buffer drop)
                               (goto-char drop) (pos--subtree-body)))
                  (drop-id (org-entry-get drop "ID")))
              (when drop-id
                (if keep-id
                    (pcase-let ((`(,count . ,buffers)
                                 (pos-dedupe--relink drop-id keep-id dry-run)))
                      (setq relinked (+ relinked count))
                      (dolist (buffer buffers) (cl-pushnew buffer touched)))
                  (setq keep-id drop-id)
                  (unless dry-run (org-entry-put keep "ID" drop-id))))
              (when (and (not (string-empty-p drop-body))
                         (not (string= drop-body keep-body)))
                (setq merged (1+ merged))
                (unless dry-run (pos--merge-under drop keep label)))
              (unless dry-run
                (with-current-buffer (marker-buffer drop)
                  (goto-char drop)
                  (org-cut-subtree))))))))
    (list :resolved resolved :merged merged :relinked relinked
          :vanished vanished :touched touched)))

(defun pos-dedupe-apply (root plan &optional dry-run)
  "Apply dedupe PLAN to ROOT; with DRY-RUN change nothing.
Return (:resolved :merged :relinked :skipped :unwritable :vanished
:stale) counts.  A group with a copy in a file a command of ROOT may
not write, one of another repository's, is left as it is and counted
unwritable.  A dropped copy's ID passes to the kept copy where it has
none; else each \"id:\" link to it that the index records is pointed
at the kept copy's, and :relinked counts them.  A group with such a
link in a file ROOT may not write is left and counted unwritable too,
since cutting the copy would leave that link pointing at nothing."
  (pos-roam-with-index root
    (let ((resolved 0) (merged 0) (relinked 0) (vanished 0) (unlinkable 0)
          (corpus (pos-corpus root))
          (make-backup-files nil)
          (touched nil))
      ;; Every marker is resolved before any edit, so that a plan whose
      ;; lines have moved is found stale as a whole.
      (pcase-let ((`(,jobs . ,counts) (pos-dedupe--jobs root plan corpus)))
        (dolist (job jobs)
          (if (pos-dedupe--relinks-unwritable-p job root corpus)
              (setq unlinkable (1+ unlinkable))
            (let ((result (pos-dedupe--resolve job dry-run)))
              (setq resolved (+ resolved (plist-get result :resolved))
                    merged (+ merged (plist-get result :merged))
                    relinked (+ relinked (plist-get result :relinked))
                    vanished (+ vanished (plist-get result :vanished)))
              (dolist (buffer (plist-get result :touched)) (cl-pushnew buffer touched)))))
        (unless dry-run
          (dolist (job jobs)
            (dolist (marker (cons (cadr job) (mapcar #'car (cddr job))))
              (cl-pushnew (marker-buffer marker) touched)))
          (dolist (buffer touched)
            (with-current-buffer buffer
              (when (buffer-modified-p) (save-buffer)))))
        (list :resolved resolved :merged merged :relinked relinked
              :skipped (plist-get counts :skipped)
              :unwritable (+ unlinkable (plist-get counts :unwritable))
              :vanished vanished :stale (plist-get counts :stale))))))

(defun pos-dedupe-report (result dry-run)
  "Return a report of RESULT from `pos-dedupe-apply'; DRY-RUN words it."
  (format "%s %d duplicate group%s (%d merged, %d links relinked, %d skipped as undecided, %d with a copy the root may not write, %d gone with an earlier cut, %d stale)"
          (if dry-run "Would resolve" "Resolved")
          (plist-get result :resolved)
          (if (= 1 (plist-get result :resolved)) "" "s")
          (plist-get result :merged)
          (plist-get result :relinked)
          (plist-get result :skipped)
          (plist-get result :unwritable)
          (plist-get result :vanished)
          (plist-get result :stale)))

(defun pos-dedupe-plan-batch (file)
  "Write the dedupe plan to FILE; print the group count."
  ;; Scan once: a rescan would prompt about the changed plan buffer.
  (let* ((root (file-name-as-directory pos-directory))
         (groups (pos-dedupe-groups root)))
    (with-temp-file file (insert (pos-dedupe-plan root groups)))
    (princ (format "%d duplicate groups written to %s\n"
                   (length groups)
                   (if (string-prefix-p root (expand-file-name file))
                       (file-relative-name file root)
                     file)))))

(defun pos-dedupe-apply-batch (file dry-run)
  "Apply the dedupe plan in FILE with DRY-RUN; print the report.
The index is brought up to date with what was written."
  (let ((root (file-name-as-directory pos-directory)))
    (princ (pos-dedupe-report (pos-dedupe-apply root file dry-run) dry-run))
    (terpri)
    (unless dry-run (pos-roam-sync root))))

(provide 'pos-dedupe)
;;; pos-dedupe.el ends here
