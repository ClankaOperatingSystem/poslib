;;; pos-sources-test.el --- Tests for pos-sources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chris Gough

;; Author: Chris Gough
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A configuration's sources, added, removed and adopted in its text.

;;; Code:

(require 'ert)
(require 'pos-sources)
(require 'pos-test-support)

(defconst pos-sources-test-config
  (pos-test-lines "pos: 2"
                  "# The skills."
                  "methodologies:"
                  "  # The gnome."
                  "  - source: https://example.org/team/pr-gnome.git"
                  "    ref: v1"
                  "  - source: git@example.org:team/shared.git"
                  "    ref: v2"
                  "    skills: [share-one, share-two]"
                  "    kind: skills-repository"
                  ""
                  "# The agents."
                  "harnesses: [codex]")
  "A configuration that declares two sources, with comments around them.")

(defun pos-sources-test-lock (dir &rest entries)
  "Write DIR's skills-lock.json recording ENTRIES.
Each is (NAME SOURCE REF &optional URL): REF nil for none, and URL
the entry's sourceUrl."
  (with-temp-file (expand-file-name "skills-lock.json" dir)
    (insert
     (json-serialize
      `((version . 1)
        (skills
         . ,(mapcar (pcase-lambda (`(,name ,source ,ref ,url))
                      `(,(intern name)
                        (source . ,source) (sourceType . "git")
                        ,@(and url `((sourceUrl . ,url)))
                        ,@(and ref `((ref . ,ref)))
                        (computedHash . "0")))
                    entries)))))))

(ert-deftest pos-sources/the-block-is-written-afresh-and-the-rest-kept ()
  "The methodologies block is written in one form; the rest is kept.
A comment within the block goes, and those before and after it stay."
  (should (equal (pos-sources-add pos-sources-test-config
                                  "https://example.org/team/garden.git" "v3")
                 (pos-test-lines "pos: 2"
                                 "# The skills."
                                 "methodologies:"
                                 "  - source: https://example.org/team/pr-gnome.git"
                                 "    ref: v1"
                                 "  - source: git@example.org:team/shared.git"
                                 "    ref: v2"
                                 "    skills: [share-one, share-two]"
                                 "    kind: skills-repository"
                                 "  - source: https://example.org/team/garden.git"
                                 "    ref: v3"
                                 ""
                                 "# The agents."
                                 "harnesses: [codex]"))))

(ert-deftest pos-sources/a-block-of-entries-at-the-margin-is-found ()
  "A sequence written at the key's own margin is the block's."
  (should (equal (pos-sources-add
                  (pos-test-lines "pos: 2" "methodologies:"
                                  "- source: https://example.org/a.git"
                                  "  ref: v1"
                                  "bin: bin")
                  "https://example.org/a.git" "v2")
                 (pos-test-lines "pos: 2" "methodologies:"
                                 "  - source: https://example.org/a.git"
                                 "    ref: v2"
                                 "bin: bin"))))

(ert-deftest pos-sources/the-first-source-is-added-at-the-end ()
  "A configuration with no methodologies has the block added at its end.
A last line with no newline is given one first."
  (should (equal (pos-sources-add "pos: 2\nbin: bin" "https://example.org/a.git" "v1"
                                  '("a-one") "skills-repository")
                 (pos-test-lines "pos: 2" "bin: bin" "methodologies:"
                                 "  - source: https://example.org/a.git"
                                 "    ref: v1"
                                 "    skills: [a-one]"
                                 "    kind: skills-repository"))))

(ert-deftest pos-sources/adding-a-declared-source-changes-its-entry ()
  "A declared source takes the new ref, and adds the skills named.
Its entry stays where it is, as its URL was written, and a source
that names no skills still names none."
  (let ((text (pos-sources-add pos-sources-test-config
                               "git@example.org:team/shared" "v3"
                               '("share-two" "share-three"))))
    (should (string-match-p
             (concat "  - source: git@example.org:team/shared.git\n"
                     "    ref: v3\n"
                     "    skills: \\[share-one, share-two, share-three\\]\n"
                     "    kind: skills-repository\n\n")
             text)))
  (should (string-match-p
           "  - source: https://example.org/team/pr-gnome.git\n    ref: v2\n  - "
           (pos-sources-add pos-sources-test-config
                            "https://example.org/team/pr-gnome.git" "v2"))))

(ert-deftest pos-sources/an-addition-the-reader-refuses-is-refused ()
  "A source, a ref or a skill the reader refuses is refused as it is."
  (pos-test-refused pos-node-refused 'bad-value
    (pos-sources-add pos-sources-test-config "https://example.org/b.git" "v1 x"))
  (pos-test-refused pos-node-refused 'bad-value
    (pos-sources-add pos-sources-test-config "https://example.org/b.git" "v1"
                     '("greet")))
  (pos-test-refused pos-node-refused 'bad-value
    (pos-sources-add pos-sources-test-config "not a url" "v1")))

(ert-deftest pos-sources/a-ref-that-needs-quotes-is-quoted ()
  "A value that is not a plain scalar is written in double quotes."
  (should (string-match-p
           "    ref: \"\\*v1\"\n"
           (pos-sources-add "pos: 2\n" "https://example.org/a.git" "*v1"))))

(ert-deftest pos-sources/methodologies-as-a-path-are-refused ()
  "A configuration that declares methodologies as a path is refused."
  (pos-test-refused pos-node-refused 'methodologies-path
    (pos-sources-add "pos: 2\nmethodologies: methodologies/\n"
                     "https://example.org/a.git" "v1")))

(ert-deftest pos-sources/removing-a-source-uninstalls-its-skills ()
  "A source removed takes its entry, and every skill installed from it.
Removing the last source removes the block."
  (pos-test-with-temp-dir dir
    (pos-sources-test-lock dir
                           '("pr-gnome-write" "https://example.org/team/pr-gnome.git" "v1")
                           '("pr-gnome-read" "https://example.org/team/pr-gnome.git" "v1")
                           '("stray" "someone/skills" nil))
    (let ((removed (pos-sources-remove pos-sources-test-config dir
                                       "https://example.org/team/pr-gnome")))
      (should (alist-get 'whole removed))
      (should (equal (alist-get 'skills removed) '("pr-gnome-read" "pr-gnome-write")))
      (should-not (string-match-p "pr-gnome" (alist-get 'text removed)))
      (should (equal (alist-get 'text
                                (pos-sources-remove (alist-get 'text removed) dir
                                                    "git@example.org:team/shared.git"))
                     (pos-test-lines "pos: 2" "# The skills." ""
                                     "# The agents." "harnesses: [codex]"))))))

(ert-deftest pos-sources/removing-skills-leaves-the-others ()
  "Skills removed leave the source naming the rest; the last takes it.
Only those installed are uninstalled."
  (pos-test-with-temp-dir dir
    (pos-sources-test-lock dir
                           '("share-one" "git@example.org:team/shared.git" "v2"))
    (let ((removed (pos-sources-remove pos-sources-test-config dir
                                       "git@example.org:team/shared.git"
                                       '("share-one"))))
      (should-not (alist-get 'whole removed))
      (should (equal (alist-get 'skills removed) '("share-one")))
      (should (string-match-p "    skills: \\[share-two\\]\n" (alist-get 'text removed)))
      (let ((last (pos-sources-remove (alist-get 'text removed) dir
                                      "git@example.org:team/shared.git"
                                      '("share-two"))))
        (should (alist-get 'whole last))
        (should (equal (alist-get 'skills last) '("share-one")))
        (should-not (string-match-p "shared" (alist-get 'text last)))))))

(ert-deftest pos-sources/removing-skills-of-a-source-that-names-none ()
  "A source that names no skills names those installed, less those removed."
  (pos-test-with-temp-dir dir
    (pos-sources-test-lock dir
                           '("pr-gnome-write" "https://example.org/team/pr-gnome.git" "v1")
                           '("pr-gnome-read" "https://example.org/team/pr-gnome.git" "v1"))
    (let ((removed (pos-sources-remove pos-sources-test-config dir
                                       "https://example.org/team/pr-gnome.git"
                                       '("pr-gnome-read"))))
      (should (equal (alist-get 'skills removed) '("pr-gnome-read")))
      (should (string-match-p "    ref: v1\n    skills: \\[pr-gnome-write\\]\n"
                              (alist-get 'text removed))))))

(ert-deftest pos-sources/what-is-not-declared-is-not-removed ()
  "A source not declared, and a skill its source does not name, are refused."
  (pos-test-with-temp-dir dir
    (pos-test-refused pos-node-refused 'not-declared
      (pos-sources-remove pos-sources-test-config dir "https://example.org/other.git"))
    (pos-test-refused pos-node-refused 'not-declared
      (pos-sources-remove pos-sources-test-config dir
                          "git@example.org:team/shared.git" '("share-three")))
    (pos-test-refused pos-node-refused 'not-declared
      (pos-sources-remove pos-sources-test-config dir
                          "https://example.org/team/pr-gnome.git" '("pr-gnome-write")))))

(ert-deftest pos-sources/a-wild-caught-skill-is-adopted-from-its-lock-entry ()
  "Wild-caught skills are declared at their entries' ref, by source.
A source written OWNER/REPOSITORY is its URL on github.com; the kind
is methodology where every skill adopted bears the source's name."
  (pos-test-with-temp-dir dir
    (pos-sources-test-lock dir
                           '("grill-me" "someone/skills" "v4")
                           '("roast-me" "someone/skills" "v4")
                           '("tools-lint" "https://example.org/tools.git" "v1"
                             "https://example.org/tools.git"))
    (should (equal (pos-sources-adopt "pos: 2\n" dir '("roast-me" "tools-lint" "grill-me"))
                   (pos-test-lines "pos: 2" "methodologies:"
                                   "  - source: https://github.com/someone/skills.git"
                                   "    ref: v4"
                                   "    skills: [grill-me, roast-me]"
                                   "    kind: skills-repository"
                                   "  - source: https://example.org/tools.git"
                                   "    ref: v1"
                                   "    skills: [tools-lint]")))))

(ert-deftest pos-sources/only-a-wild-caught-skill-at-a-ref-is-adopted ()
  "A skill not installed, or of a declared source, or at no ref, is refused.
So is a source whose entries adopted are at two refs."
  (pos-test-with-temp-dir dir
    (pos-sources-test-lock dir
                           '("pr-gnome-write" "https://example.org/team/pr-gnome.git" "v1")
                           '("loose" "someone/skills" nil)
                           '("one" "https://example.org/x.git" "v1")
                           '("two" "https://example.org/x.git" "v2"))
    (pos-test-refused pos-node-refused 'not-wild-caught
      (pos-sources-adopt pos-sources-test-config dir '("absent")))
    (pos-test-refused pos-node-refused 'not-wild-caught
      (pos-sources-adopt pos-sources-test-config dir '("pr-gnome-write")))
    (pos-test-refused pos-node-refused 'no-ref
      (pos-sources-adopt pos-sources-test-config dir '("loose")))
    (pos-test-refused pos-node-refused 'no-ref
      (pos-sources-adopt pos-sources-test-config dir '("one" "two")))))

(provide 'pos-sources-test)
;;; pos-sources-test.el ends here
