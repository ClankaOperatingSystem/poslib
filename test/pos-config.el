;;; pos-config.el --- poslib configuration: test fixture  -*- lexical-binding: t -*-

;;; Commentary:

;; Shape of a repository's pos-config.el, loaded by `pos-load-config'.

;;; Code:

(setq pos-pillars '("life" "sport" "people" "work" "body" "meta")
      pos-prose-directories '("meta/journal" "meta/specs")
      pos-refile-rules '(("invoice\\|client" . "work")
                         ("dentist\\|checkup" . "body")))

;;; pos-config.el ends here
