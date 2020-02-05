;;; base.el --- Basic Setup

;;; Commentary:
;; General settings and variable declerations

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar my-term-shell "/bin/zsh" "Default terminal shell.")

(add-hook 'vterm-exit-functions #'(lambda (buf e)
                                    (when buf
                                      (kill-buffer buf)
                                      (call-interactively 'evil-window-delete))))

(setq-default
 ;; Line spacing
 line-spacing 3
 ;; Use only spaces (no tabs)
 indent-tabs-mode nil)

(setq
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-scratch-message nil
 inhibit-startup-echo-area-message t
 ;; Setup Org Agenda to watch work.org file
 org-agenda-files '("~/Drive/etc/work.org")
 create-lockfiles nil
 auto-save-default nil
 ;; Disable backup, auto-save and lockfiles
 make-backup-files nil
 ;; Set `custom-file` location
 custom-file "~/.emacs.d/custom.el"
 ;; Remove annoying bell
 ring-bell-function 'ignore
 ;; list of tab stop positions
 tab-stop-list (number-sequence 4 200 4)
 ;; Set tab width to 4 spaces
 tab-width 4
 ;; Disable word-wrap
 truncate-lines t
 ;; Don’t add new lines past end of file
 next-line-add-newlines nil
 ;; See https://github.com/lewang/flx for origin of this GC value
 gc-cons-threshold 20000000
 ;; Revert files without asking.
 revert-without-query '(".*"))

;; Start initial frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Replace yes or no prompt with y or n prompt.
(fset 'yes-or-no-p 'y-or-n-p)

;; Set line numbers
(global-display-line-numbers-mode)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set file encoding to UTF-8
(set-language-environment "UTF-8")

;; Attempt to load custom.el file and no error in case it doesn't exist
(load custom-file 'noerror)

;; Enable server for command line support
(if (display-graphic-p)
    (server-start))

;; Enable Winner mode (undo/redo for window layout)
(winner-mode 1)

;; Auto-update changed files
(global-auto-revert-mode t)

(provide 'base)

;;; base.el ends here
