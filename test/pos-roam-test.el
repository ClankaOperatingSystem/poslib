;;; pos-roam-test.el --- Tests for pos-roam -*- lexical-binding: t; -*-

;;; Commentary:
;; The index of a tree: what it covers, what it records, and whose
;; database it is.

;;; Code:

(require 'ert)
(require 'pos-roam)
(require 'pos-test-support)

;; The index of each temporary repository goes to a cache of the tests'
;; own, never to a database of the user's.
(setq pos-roam-cache-directory (make-temp-file "pos-roam-test-" t))

(ert-deftest pos-roam-files/never-enters-an-excluded-directory ()
  "Archives, attics, and hidden and underscore directories are not
indexed, at any depth."
  (pos-test-with-files root '(("a.org" . "") ("sub/b.org" . "")
                              ("archives/x.org" . "") ("sub/attic/y.org" . "")
                              ("_work/z.org" . "") (".hidden/h.org" . "")
                              ("sub/_deep/archives/w.org" . ""))
    (should (equal '("a.org" "sub/b.org")
                   (mapcar (lambda (file) (file-relative-name file root))
                           (pos-roam-files root))))))

(ert-deftest pos-roam-referrers/are-the-id-links-the-index-records ()
  "A link is recorded where a node holds it: in a file with an ID, or
under a heading with one.  A link in a file with neither is not."
  (pos-test-with-files root '(("a.org" . "* Target\n:PROPERTIES:\n:ID: target\n:END:\n")
                              ("b.org" . ":PROPERTIES:\n:ID: file-b\n:END:\nSee [[id:target][it]].\n")
                              ("c.org" . "* Note\n:PROPERTIES:\n:ID: note-c\n:END:\n[[id:target]]\n")
                              ("d.org" . "Unindexed: [[id:target]]\n")
                              ("archives/e.org" . ":PROPERTIES:\n:ID: file-e\n:END:\n[[id:target]]\n"))
    (pos-roam-with-index root
      (should (equal '(("b.org" . 36) ("c.org" . 39))
                     (mapcar (lambda (referrer)
                               (cons (file-relative-name (car referrer) (file-truename root))
                                     (cdr referrer)))
                             (pos-roam-referrers "target")))))))

(ert-deftest pos-roam-with-index/follows-what-changed-on-disk ()
  "The index is brought up to date each time it is used."
  (pos-test-with-files root '(("a.org" . "* Target\n:PROPERTIES:\n:ID: target\n:END:\n")
                              ("b.org" . ":PROPERTIES:\n:ID: file-b\n:END:\n"))
    (pos-roam-with-index root
      (should-not (pos-roam-referrers "target")))
    (with-temp-file (expand-file-name "b.org" root)
      (insert ":PROPERTIES:\n:ID: file-b\n:END:\n[[id:target]]\n"))
    (pos-roam-with-index root
      (should (equal 1 (length (pos-roam-referrers "target")))))))

(ert-deftest pos-roam-rewrite-link/rewrites-the-link-there-and-no-other ()
  "The link at the recorded place is pointed elsewhere; one that has
moved is left, and reported; a dry run reports without writing."
  (pos-test-with-files root '(("b.org" . "Lead [[id:old][x]] and [[id:old]].\n"))
    (let ((file (expand-file-name "b.org" root)))
      (should (pos-roam-rewrite-link file 6 "old" "new" t))
      (with-current-buffer (find-file-noselect file)
        (should-not (buffer-modified-p)))
      (should (pos-roam-rewrite-link file 6 "old" "new"))
      (should-not (pos-roam-rewrite-link file 7 "old" "new"))
      (with-current-buffer (find-file-noselect file) (save-buffer))
      (should (equal "Lead [[id:new][x]] and [[id:old]].\n"
                     (pos-test-file-string file))))))

(ert-deftest pos-roam-db-location/is-the-users-only-around-the-root ()
  "A root within `org-roam-directory' is served by that index; any
other root has one of its own, named by its path, in the cache."
  (pos-test-with-files root '(("a.org" . ""))
    (let ((org-roam-directory (file-name-directory (directory-file-name root)))
          (org-roam-db-location "/nowhere/theirs.db"))
      (should (equal "/nowhere/theirs.db" (pos-roam-db-location root))))
    (let ((org-roam-directory (expand-file-name "elsewhere" root))
          (org-roam-db-location "/nowhere/theirs.db"))
      (make-directory org-roam-directory)
      (should (equal (expand-file-name (concat (sha1 (file-truename root)) ".db")
                                       pos-roam-cache-directory)
                     (pos-roam-db-location root))))))

(ert-deftest pos-roam-with-index/writes-no-database-of-the-users-elsewhere ()
  "Indexing a tree outside the user's `org-roam-directory' leaves their
database alone, whatever org-roam's variables are rebound to meanwhile."
  (pos-test-with-files root '(("a.org" . "* Target\n:PROPERTIES:\n:ID: target\n:END:\n"))
    (let* ((theirs (expand-file-name "theirs.db" root))
           (org-roam-directory (expand-file-name "elsewhere" root))
           (org-roam-db-location theirs))
      (make-directory org-roam-directory)
      (pos-roam-with-index root
        (should (equal 1 (caar (org-roam-db-query [:select (funcall count *) :from nodes])))))
      (should-not (file-exists-p theirs))
      (should (file-exists-p (expand-file-name (concat (sha1 (file-truename root)) ".db")
                                               pos-roam-cache-directory))))))

(provide 'pos-roam-test)
;;; pos-roam-test.el ends here
