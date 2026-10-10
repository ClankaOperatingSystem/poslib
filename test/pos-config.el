;;; pos-config.el --- poslib configuration: test fixture  -*- lexical-binding: t -*-

;;; Commentary:

;; Shape of a repository's pos-config.el, loaded by `pos-load-config'.

;;; Code:

(setq pos-refile-rules '(("invoice\\|client" . "work/work-projects.org")
                         ("dentist\\|checkup" . "body/body-projects.org")))

;;; pos-config.el ends here
