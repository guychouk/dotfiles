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

;; Set font based on OS
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (add-to-list 'default-frame-alist
                 '(font . "Liberation Mono 10"))))
 ((string-equal system-type "darwin")
  (progn
    (add-to-list 'default-frame-alist
                 '(font . "Hack 13")))))

;; Themes
(doom-themes-org-config)
(load-theme 'doom-one t)

;; Setup modeline
(require 'doom-modeline)
(add-hook 'after-init-hook 'doom-modeline-mode)
(setq doom-modeline-height 1
      doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-vcs-max-length 60)

(provide 'init-gui)

;;; init-gui.el ends here
