;;; init-packages.el --- My Packages Setup file.
;;; Commentary:
;;

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

; eyebrowse
(eyebrowse-mode t)

; nov
(defvar nov-text-width 60)
(add-to-list 'auto-mode-alist '("\\.epub$" . nov-mode))

; company
(require 'company)
(setq company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "<tab>") 'company-select-next)
(define-key company-active-map (kbd "<S-tab>") 'company-select-previous)

; company-lsp
(require 'company-lsp)
(push 'company-lsp company-backends)

;; Index and search projects using standard means
;; "brew install the_silver_searcher"
(projectile-mode +1)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'ivy)
(setq projectile-indexing-method 'alien)

; swiper
(define-key evil-normal-state-map (kbd "/") 'swiper)

; ivy
(ivy-mode 1)
(setq ivy-height 20)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-display-style 'fancy)

; yasnippet
(yas-global-mode 1)

; diff-hl
(global-diff-hl-mode 1)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

; wakatime-mode + "pip install wakatime"
(global-wakatime-mode)

; flycheck + "npm i -g eslint"
(global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save mode-enable))
(setq flycheck-indication-mode nil)
(setq js2-include-node-externs t)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(add-hook 'js2-mode-hook 'my/js2-mode-setup)

(provide 'init-packages)

;;; init-packages.el ends here
