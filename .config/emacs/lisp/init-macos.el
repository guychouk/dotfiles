;;; init-macos.el --- MacOS Setup

;;; Commentary:
;; General settings for when working on a Mac.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-
(defvar my-term-shell "/bin/zsh" "Default terminal shell.")

(setq org-agenda-files '("~/Drive/etc/work.org"))

;; Set tsserver log file to /tmp/ directory
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")

;; Install exec-path-from-shell and configure
(when (not (package-installed-p 'exec-path-from-shell))
              (package-install 'exec-path-from-shell))
(defvar exec-path-from-shell-check-startup-files nil)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES"))

(provide 'init-macos)

;;; init-macos.el ends here
