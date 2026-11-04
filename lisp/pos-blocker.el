;;; pos-blocker.el --- Items blocked by events -*- lexical-binding: t; -*-

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

;; An item that waits on a person is WAITING.  An item can also be
;; unable to proceed until something happens: a server is repaired, a
;; deploy is made, a ticket is closed.  That happening is an event: an
;; item, a heading with a state and an ID, which is DONE when it
;; happens.  An event that waits on no person in its scope is WAITING,
;; naming who or when, as any waiting item does.
;;
;; An item names the events it cannot proceed without in its
;; `pos-blocker-property', as links to them, [[id:ID][title]].  A
;; blocked item is TODO; blocked is worked out from the property and
;; is not a state.  An event has a `pos-trigger-property' when other
;; scopes hold their own item for it, as links down to those items.
;; Either property links to an item of the same scope or beneath it.
;; The names are the ones org-depend and org-edna use.
;;
;;   * WAITING The glasshouse boiler is repaired
;;     :PROPERTIES:
;;     :ID:       boiler
;;     :END:
;;   * TODO Sow the tomato seed
;;     :PROPERTIES:
;;     :BLOCKER:  [[id:boiler][The glasshouse boiler is repaired]]
;;     :END:
;;
;; - `pos-blocker-property', `pos-trigger-property': the properties.
;; - `pos-blocker-links': the IDs a property's value links to.
;; - `pos-blocker-headings': the headings of a corpus that have an ID
;;   or either property, each a `pos-blocker-heading'.
;; - `pos-blocker-events': each open event of a corpus, with the open
;;   items it blocks.
;; - `pos-blocker-lint': what is wrong with the properties of a tree.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'pos)
(require 'pos-corpus)

(defconst pos-blocker-property "BLOCKER"
  "The property of an item that links to the events it waits for.")

(defconst pos-trigger-property "TRIGGER"
  "The property of an event that links down to the items it releases.")

(cl-defstruct (pos-blocker-heading (:constructor pos-blocker-heading-make)
                                   (:copier nil))
  "A heading of the corpus that has an ID, a blocker or a trigger.
FILE is absolute and LINE the heading's.  ID, STATE and TITLE are its
own, or nil.  DONE is non-nil if STATE is a done state.  SCOPE is the
`pos-scope' its file belongs to.  BLOCKER and TRIGGER are the values
of `pos-blocker-property' and `pos-trigger-property', or nil."
  file line id state title done scope blocker trigger)

(defun pos-blocker-links (value)
  "Return the IDs that VALUE, a property's value, links to, in order.
VALUE is links of the form [[id:ID]] or [[id:ID][title]], with space
between them.  Anything else in it is given as nil, once, at the end."
  (let ((ids nil) (start 0) (rest nil))
    (while (string-match "\\[\\[id:\\([^]]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]"
                         value start)
      (unless (string-blank-p (substring value start (match-beginning 0)))
        (setq rest t))
      (push (match-string 1 value) ids)
      (setq start (match-end 0)))
    (unless (string-blank-p (substring value start))
      (setq rest t))
    (nreverse (if rest (cons nil ids) ids))))

(defun pos-blocker-headings (corpus)
  "Return the headings of CORPUS that have an ID, a blocker or a trigger.
Each is a `pos-blocker-heading', in the order of the corpus's files
and of the headings in each."
  (let (headings)
    (pcase-dolist (`(,file . ,scope) (pos-corpus-entries corpus))
      (with-current-buffer (pos-visit file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((id (org-entry-get nil "ID"))
                  (blocker (org-entry-get nil pos-blocker-property))
                  (trigger (org-entry-get nil pos-trigger-property)))
              (when (or id blocker trigger)
                (push (pos-blocker-heading-make
                       :file file :line (line-number-at-pos)
                       :id (and id (string-trim id))
                       :state (org-get-todo-state)
                       :title (substring-no-properties
                               (org-get-heading t t t t))
                       :done (org-entry-is-done-p)
                       :scope scope :blocker blocker :trigger trigger)
                      headings))))
          nil 'file))))
    (nreverse headings)))

(defun pos-blocker--item (id headings)
  "Return the heading of HEADINGS that is an item with ID, or nil.
An item is a heading with a state."
  (seq-find (lambda (heading)
              (and (equal id (pos-blocker-heading-id heading))
                   (pos-blocker-heading-state heading)))
            headings))

(defun pos-blocker--open-p (heading)
  "Return non-nil if HEADING is an item that is not done."
  (and (pos-blocker-heading-state heading)
       (not (pos-blocker-heading-done heading))))

(defun pos-blocker-events (corpus &optional headings)
  "Return each open event of CORPUS with the open items it blocks.
A list of (EVENT . BLOCKED), each a `pos-blocker-heading', the events
in the order of the corpus and the items each blocks too.  An event is
an item that an open item's `pos-blocker-property' links to, and is
open if it is not done.  HEADINGS are those `pos-blocker-headings'
returns for CORPUS, read again when nil."
  (let* ((headings (or headings (pos-blocker-headings corpus)))
         (blocked (seq-filter (lambda (heading)
                                (and (pos-blocker--open-p heading)
                                     (pos-blocker-heading-blocker heading)))
                              headings)))
    (delq nil
          (mapcar
           (lambda (event)
             (let ((id (pos-blocker-heading-id event)))
               (when (and id (pos-blocker--open-p event))
                 (let ((items
                        (seq-filter
                         (lambda (item)
                           (member id (pos-blocker-links
                                       (pos-blocker-heading-blocker item))))
                         blocked)))
                   (and items (cons event items))))))
           headings))))

(defun pos-blocker--within-p (scope other)
  "Return non-nil if the scope OTHER is SCOPE or lies beneath it."
  (let ((path (pos-scope-path scope)))
    (or (equal path ".")
        (equal path (pos-scope-path other))
        (string-prefix-p (concat path "/") (pos-scope-path other)))))

(defun pos-blocker--finding (heading message)
  "Return a `pos-finding' at HEADING with MESSAGE."
  (pos-finding-make :file (pos-blocker-heading-file heading)
                    :line (pos-blocker-heading-line heading)
                    :message message))

(defun pos-blocker-lint (root)
  "Report what is wrong with the blockers and triggers of the tree at ROOT.
Findings, each a `pos-finding' at the heading of a file the root may
write: a `pos-blocker-property' or `pos-trigger-property' that holds
something other than links by ID, that links to no item, or that
links to an item of a scope that is neither the heading's nor beneath
it; and a NEXT item that an event not done blocks, which is TODO
until the event is done."
  (let* ((corpus (pos-corpus (file-name-as-directory (expand-file-name root))))
         ;; A file the root may not write keeps its own keywords.
         (pos-visit-corpus corpus)
         (headings (pos-blocker-headings corpus))
         findings)
    (dolist (heading headings)
      (when (pos-corpus-writable-p corpus (pos-blocker-heading-file heading))
        (cl-flet ((report (format-string &rest args)
                    (push (pos-blocker--finding
                           heading (apply #'format format-string args))
                          findings)))
          (dolist (property (list pos-blocker-property pos-trigger-property))
            (let ((value (if (equal property pos-blocker-property)
                             (pos-blocker-heading-blocker heading)
                           (pos-blocker-heading-trigger heading))))
              (dolist (id (and value (pos-blocker-links value)))
                (let ((item (and id (pos-blocker--item id headings))))
                  (cond
                   ((not id)
                    (report "%s holds something other than links by ID"
                            property))
                   ((not item)
                    (report "%s links to no item: %s" property id))
                   ((not (pos-blocker--within-p
                          (pos-blocker-heading-scope heading)
                          (pos-blocker-heading-scope item)))
                    (report "%s links to an item of another scope: %s"
                            property
                            (pos-scope-path (pos-blocker-heading-scope item))))
                   ((and (equal property pos-blocker-property)
                         (equal "NEXT" (pos-blocker-heading-state heading))
                         (not (pos-blocker-heading-done item)))
                    (report "NEXT item blocked by an event not done: %s"
                            (pos-blocker-heading-title item)))))))))))
    (nreverse findings)))

(provide 'pos-blocker)
;;; pos-blocker.el ends here
