;;; init-gui.el --- GUI Setup
;;; Commentary:
;;

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

;; Remove menu-bar
(menu-bar-mode -1)

;; Remove tool-bar
(tool-bar-mode -1)

;; Remove scroll-bar
(toggle-scroll-bar -1)

;; Set font to Liberation Mono 12 by default
(add-to-list 'default-frame-alist '(font . "Liberation Mono 10"))

;; Themes
(doom-themes-org-config)
(load-theme 'doom-one t)

;; Setup doom-modeline
(require 'doom-modeline)
(add-hook 'after-init-hook 'doom-modeline-mode)
(setq doom-modeline-height 25
      doom-modeline-buffer-file-name-style 'relative-from-project
      doom-modeline-vcs-max-length 60)

(provide 'init-gui)

;;; init-gui.el ends here
