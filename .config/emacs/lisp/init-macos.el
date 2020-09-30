;;; init-macos.el --- MacOS Setup

;;; Commentary:
;; General settings for when working on a Mac.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-
(defvar my-term-shell "/bin/zsh" "Default terminal shell.")

(add-hook 'vterm-exit-functions #'(lambda (buf e)
                                    (when buf
                                      (kill-buffer buf)
                                      (call-interactively 'evil-window-delete))))

(setq org-agenda-files '("~/Drive/etc/work.org"))

;; Overwrite default font with Hack on MacOS
(add-to-list 'default-frame-alist '(font . "Hack 13"))
;; Same for doom-modeline font
(set-face-attribute 'mode-line nil :family "Hack" :height 120)
(set-face-attribute 'mode-line-inactive nil :family "Hack" :height 120)

;; Set tsserver log file to /tmp/ directory
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")

;; Install exec-path-from-shell and configure
(when (not (package-installed-p 'exec-path-from-shell))
              (package-install 'exec-path-from-shell))
(defvar exec-path-from-shell-check-startup-files nil)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES"))

(add-to-list 'load-path "~/.emacs.d/emacs-libvterm")
(require 'vterm)

(provide 'init-macos)

;;; init-macos.el ends here
