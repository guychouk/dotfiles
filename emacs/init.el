;;; -*- lexical-binding: t -*-

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq scroll-conservatively 101)
(setq-default mode-line-format nil)
(setq-default word-wrap t)

(global-display-line-numbers-mode 0)

(set-face-attribute 'default nil :font "Iosevka Term" :height 360)
(set-fontset-font t 'hebrew "Arial Hebrew")

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
