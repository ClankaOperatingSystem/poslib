;;; pos-person-test.el --- Tests for pos-person.el  -*- lexical-binding: t -*-

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

;; Run: make test.
;;
;; Each test is named for the rule it pins.  The tree is a root whose
;; projects lie in projects/, one of them a file and one a directory,
;; and one responsibility, home, whose configuration is in .clanka/.

;;; Code:

(require 'ert)
(require 'pos-person)
(require 'pos-test-support)

(defmacro pos-person-test-with-tree (&rest body)
  "Evaluate BODY with `root' holding a root, two projects and home."
  (declare (indent 0))
  `(pos-test-with-files root
       '((".pos/config.yaml"
          . "pos: 2\nprojects: projects/\nchildren:\n  - path: home\n")
         ("intray.org" . "* Unsorted\n")
         ("projects/mend-roof.org" . "* NEXT Buy slates\n")
         ("projects/paint-hall/project.org" . "* NEXT Choose the colour\n")
         ("projects/paint-hall/notes.org" . "* Notes\n")
         ("home/.clanka/config.yml" . "pos: 2\nkind: responsibility\n")
         ("home/index.org" . "* Tasks\n"))
     ,@body))

(defun pos-person-test--id (file)
  "Return the ID in the first property drawer of FILE."
  (let ((text (pos-test-file-string file)))
    (and (string-match "^:ID:[ \t]+\\(\\S-+\\)$" text)
         (match-string 1 text))))

(ert-deftest pos-person/a-person-is-a-file-in-the-roots-configuration-directory ()
  "The entity is NAME in lower case with hyphens, with an ID and its title.
The name is trimmed.  The file's name is returned."
  (pos-person-test-with-tree
    (let ((file (pos-person-add root " Ada Brook ")))
      (should (equal file (expand-file-name ".pos/person-entities/ada-brook.org" root)))
      (should (string-match-p
               (concat "\\`:PROPERTIES:\n:ID:       [-[:xdigit:]]\\{36\\}\n:END:\n"
                       "#\\+TITLE: Ada Brook\n\nA person-entity\\.")
               (pos-test-file-string file))))))

(ert-deftest pos-person/a-person-is-recorded-once ()
  "A second entity of the same file name is refused, and the first kept."
  (pos-person-test-with-tree
    (let* ((file (pos-person-add root "Ada Brook"))
           (text (pos-test-file-string file)))
      (should-error (pos-person-add root "ada  brook") :type 'user-error)
      (should (equal text (pos-test-file-string file))))))

(ert-deftest pos-person/a-name-is-one-line-with-a-letter-or-digit ()
  "A blank name, one of two lines and one of punctuation alone are refused."
  (pos-person-test-with-tree
    (dolist (name '("" "  " "Ada\nBrook" "--"))
      (should-error (pos-person-add root name) :type 'user-error))
    (should-not (file-exists-p (expand-file-name ".pos/person-entities" root)))))

(ert-deftest pos-person/a-root-with-no-configuration-keeps-no-people ()
  "The entities lie in the configuration directory, so a root needs one."
  (pos-test-with-files root '(("intray.org" . "* Unsorted\n"))
    (should-error (pos-person-add root "Ada Brook") :type 'user-error)))

(ert-deftest pos-person/an-identity-in-a-configured-scope-is-a-file-there ()
  "The identity is a file in the scope's own configuration directory.
It is named for the entity's file, has an ID of its own and the title
PERSON@SCOPE, and links to nothing."
  (pos-person-test-with-tree
    (let ((entity (pos-person-add root "Ada Brook")))
      (pcase-let ((`(,file ,id ,name ,made) (pos-person-identity root "Ada Brook" "home")))
        (should (equal file (expand-file-name
                             "home/.clanka/person-identities/ada-brook.org" root)))
        (should (equal name "Ada-Brook@home"))
        (should made)
        (should (equal id (pos-person-test--id file)))
        (should-not (equal id (pos-person-test--id entity)))
        (should (string-match-p "^#\\+TITLE: Ada-Brook@home$" (pos-test-file-string file)))
        (should-not (string-match-p "\\[\\[" (pos-test-file-string file)))))))

(ert-deftest pos-person/the-roots-identity-is-named-for-its-directory ()
  "The root has no path, so its identity takes the root directory's name."
  (pos-person-test-with-tree
    (pos-person-add root "Ada Brook")
    (pcase-let ((`(,file ,_id ,name ,_made) (pos-person-identity root "ada-brook" ".")))
      (should (equal file (expand-file-name ".pos/person-identities/ada-brook.org" root)))
      (should (equal name (concat "Ada-Brook@"
                                  (file-name-nondirectory (directory-file-name root))))))))

(ert-deftest pos-person/an-identity-in-a-project-is-a-heading-under-people ()
  "A project with no configuration holds its identities in its file.
A one-file project's file is the project; a directory's is project.org.
The heading People is added at the end when it is not there, and a
second person's identity goes under the same heading."
  (pos-person-test-with-tree
    (pos-person-add root "Ada Brook")
    (pos-person-add root "Cy Dale")
    (pcase-let ((`(,file ,id ,name ,_made)
                 (pos-person-identity root "Ada Brook" "projects/mend-roof")))
      (should (equal file (expand-file-name "projects/mend-roof.org" root)))
      (should (equal name "Ada-Brook@projects.mend-roof"))
      (pcase-let ((`(,_file ,other ,_name ,_made)
                   (pos-person-identity root "Cy Dale" "projects/mend-roof")))
        (should (equal (pos-test-file-string file)
                       (concat "* NEXT Buy slates\n* People\n"
                               "** Ada-Brook@projects.mend-roof\n"
                               ":PROPERTIES:\n:ID:       " id "\n:END:\n"
                               "** Cy-Dale@projects.mend-roof\n"
                               ":PROPERTIES:\n:ID:       " other "\n:END:\n")))))
    (should (equal (car (pos-person-identity root "Ada Brook" "projects/paint-hall"))
                   (expand-file-name "projects/paint-hall/project.org" root)))))

(ert-deftest pos-person/the-entity-links-down-to-each-identity ()
  "The entity gains one list item for each identity: its ID, name and file."
  (pos-person-test-with-tree
    (let* ((entity (pos-person-add root "Ada Brook"))
           (home (pos-person-identity root "Ada Brook" "home"))
           (roof (pos-person-identity root "Ada Brook" "projects/mend-roof")))
      (should (string-suffix-p
               (concat "the person in one scope.\n\n"
                       "- [[id:" (nth 1 home) "][Ada-Brook@home]],"
                       " in =home/.clanka/person-identities/ada-brook.org=\n"
                       "- [[id:" (nth 1 roof) "][Ada-Brook@projects.mend-roof]],"
                       " in =projects/mend-roof.org=\n")
               (pos-test-file-string entity))))))

(ert-deftest pos-person/an-identity-is-made-once ()
  "Asked again, the command returns the identity there and writes nothing.
A project's identity is known by the entity's link even when renamed."
  (pos-person-test-with-tree
    (let* ((entity (pos-person-add root "Ada Brook"))
           (home (pos-person-identity root "Ada Brook" "home"))
           (roof (pos-person-identity root "Ada Brook" "projects/mend-roof")))
      (pos-person-identity-rename root (nth 1 roof) "Ada, on the roof")
      (let ((texts (mapcar #'pos-test-file-string
                           (list entity (car home) (car roof)))))
        (should (equal (pos-person-identity root "Ada Brook" "home")
                       (list (car home) (nth 1 home) "Ada-Brook@home" nil)))
        (should (equal (nth 1 (pos-person-identity root "Ada Brook" "projects/mend-roof"))
                       (nth 1 roof)))
        (should (equal texts (mapcar #'pos-test-file-string
                                     (list entity (car home) (car roof)))))))))

(ert-deftest pos-person/a-missing-link-in-the-entity-is-restored ()
  "An identity that is there and that the entity does not link gains a link."
  (pos-person-test-with-tree
    (let ((entity (pos-person-add root "Ada Brook")))
      (pos-test-write-files
       root '(("home/.clanka/person-identities/ada-brook.org"
               . ":PROPERTIES:\n:ID:       kept-id\n:END:\n#+TITLE: Ada-Brook@home\n")))
      (should (equal (nth 1 (pos-person-identity root "Ada Brook" "home")) "kept-id"))
      (should (string-match-p (regexp-quote "- [[id:kept-id][Ada-Brook@home]]")
                              (pos-test-file-string entity))))))

(ert-deftest pos-person/an-identity-needs-an-entity-and-a-scope ()
  "A person with no entity, and a path that is not a scope's, are refused."
  (pos-person-test-with-tree
    (should-error (pos-person-identity root "Ada Brook" "home") :type 'user-error)
    (pos-person-add root "Ada Brook")
    (should-error (pos-person-identity root "Ada Brook" "shed") :type 'user-error)
    (should-error (pos-person-identity root "Ada Brook" "projects") :type 'user-error)))

(ert-deftest pos-person/a-scope-of-another-repository-is-refused ()
  "A responsibility that is a repository is that repository's to write."
  (pos-person-test-with-tree
    (pos-test-git-init (expand-file-name "home" root))
    (pos-person-add root "Ada Brook")
    (should-error (pos-person-identity root "Ada Brook" "home") :type 'user-error)
    (should-not (file-exists-p (expand-file-name "home/.clanka/person-identities" root)))))

(ert-deftest pos-person/a-rename-changes-the-name-and-each-links-text ()
  "The title or heading takes the new name, and so does each link by ID.
The ID stays.  A link in a property, in an item's text and in the
entity are all renamed, and each file changed is reported."
  (pos-person-test-with-tree
    (let* ((entity (pos-person-add root "Ada Brook"))
           (home (pos-person-identity root "Ada Brook" "home"))
           (id (nth 1 home))
           (index (expand-file-name "home/index.org" root)))
      (pos-test-write-files
       root `(("home/index.org"
               . ,(concat "* WAITING Hear about the slates\n:PROPERTIES:\n"
                          ":WAITING_ON: [[id:" id "][Ada-Brook@home]]\n:END:\n"
                          "Ask [[id:" id "][Ada]] again, and [[id:other][Cy]].\n"))))
      (should (equal (pos-person-identity-rename root id "Ada-Brook@house")
                     (list (car home) (cons entity 1) (cons index 2))))
      (should (equal id (pos-person-test--id (car home))))
      (should (string-match-p "^#\\+TITLE: Ada-Brook@house$"
                              (pos-test-file-string (car home))))
      (should (string-match-p (regexp-quote (concat "[[id:" id "][Ada-Brook@house]], in"))
                              (pos-test-file-string entity)))
      (should (equal (pos-test-file-string index)
                     (concat "* WAITING Hear about the slates\n:PROPERTIES:\n"
                             ":WAITING_ON: [[id:" id "][Ada-Brook@house]]\n:END:\n"
                             "Ask [[id:" id "][Ada-Brook@house]] again, and [[id:other][Cy]].\n"))))))

(ert-deftest pos-person/a-rename-of-a-projects-identity-changes-its-heading ()
  "An identity that is a heading has its heading renamed, and keeps its ID."
  (pos-person-test-with-tree
    (pos-person-add root "Ada Brook")
    (let ((roof (pos-person-identity root "Ada Brook" "projects/mend-roof")))
      (pos-person-identity-rename root (nth 1 roof) "Ada-Brook@roof")
      (should (equal (pos-test-file-string (car roof))
                     (concat "* NEXT Buy slates\n* People\n** Ada-Brook@roof\n"
                             ":PROPERTIES:\n:ID:       " (nth 1 roof) "\n:END:\n"))))))

(ert-deftest pos-person/a-rename-is-of-an-identity ()
  "An ID no entity links to is refused, and a name with a square bracket."
  (pos-person-test-with-tree
    (pos-person-add root "Ada Brook")
    (let ((home (pos-person-identity root "Ada Brook" "home")))
      (should-error (pos-person-identity-rename root "no-such-id" "Ada") :type 'user-error)
      (should-error (pos-person-identity-rename root (nth 1 home) "Ada]") :type 'user-error)
      (should-error (pos-person-identity-rename root (nth 1 home) " ") :type 'user-error)
      (should (string-match-p "^#\\+TITLE: Ada-Brook@home$"
                              (pos-test-file-string (car home)))))))

(ert-deftest pos-person/the-shell-entry-runs-each-command ()
  "The entry prints what each command made, relative to the root."
  (pos-person-test-with-tree
    (let ((pos-directory root))
      (cl-flet ((run (&rest arguments)
                  (let ((command-line-args-left arguments))
                    (with-output-to-string (pos-person-batch)))))
        (should (equal (run "add" "Ada Brook") ".pos/person-entities/ada-brook.org\n"))
        (let ((line (run "identity" "Ada Brook" "home")))
          (should (string-match
                   (concat "\\`home/\\.clanka/person-identities/ada-brook\\.org: "
                           "Ada-Brook@home \\([-[:xdigit:]]\\{36\\}\\), made\n\\'")
                   line))
          (let ((id (match-string 1 line)))
            (should (string-suffix-p ", there already\n"
                                     (run "identity" "Ada Brook" "home")))
            (should (equal (run "rename" id "Ada-Brook@house")
                           (concat "home/.clanka/person-identities/ada-brook.org\n"
                                   "Links renamed: .pos/person-entities/ada-brook.org: 1\n")))))))))

(provide 'pos-person-test)
;;; pos-person-test.el ends here
