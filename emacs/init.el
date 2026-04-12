;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-

;;; No backup, autosave, or lockfiles
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;; Sane defaults
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq scroll-conservatively 101)

;;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type t)

;;; Font - primary monospace with Hebrew fallback
(set-face-attribute 'default nil :font "Menlo" :height 180)
(set-fontset-font t 'hebrew "Arial Hebrew")

;;; Theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
(load-theme 'gman t)

;;; Hebrew editing
;;
;; Use Emacs's own input method rather than the OS Hebrew keyboard.
;; This keeps the OS in English so all keybindings work at all times.
;; C-\ toggles Hebrew typing on/off in any buffer.
(setq default-input-method "hebrew")

(defun hebrew-edit ()
  "Configure current buffer for Hebrew RTL editing."
  (interactive)
  (setq-local bidi-paragraph-direction 'right-to-left)
  (activate-input-method "hebrew")
  (message "Hebrew editing enabled (C-\\ to toggle input)"))

(defun find-file-hebrew (file)
  "Open FILE for Hebrew RTL editing."
  (interactive "FOpen for Hebrew editing: ")
  (find-file file)
  (setq-local bidi-paragraph-direction 'right-to-left)
  (activate-input-method "hebrew"))

;;; init.el ends here
