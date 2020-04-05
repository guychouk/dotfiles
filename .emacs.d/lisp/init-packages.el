;;; init-packages.el --- My Packages Setup file.
;;; Commentary:
;;

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

;; eyebrowse
(eyebrowse-mode t)

;; nov
(defvar nov-text-width 60)
(add-to-list 'auto-mode-alist '("\\.epub$" . nov-mode))

;; company
(require 'company)
(setq
 company-idle-delay .15
 company-minimum-prefix-length 2
 company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'term-mode-hook (lambda() (company-mode -1)))
(define-key company-active-map (kbd "<tab>") 'company-select-next)
(define-key company-active-map (kbd "<S-tab>") 'company-select-previous)

;; company-lsp
(require 'company-lsp)
(push 'company-lsp company-backends)

;; projectile: Index and search projects using standard means
(require 'projectile)
(projectile-mode +1)
(setq
 projectile-enable-caching nil
 projectile-completion-system 'ivy
 projectile-indexing-method 'alien)

;; swiper
(define-key global-map (kbd "C-s") 'swiper)

;; ivy
(require 'ivy)
(ivy-mode 1)
(setq
 ivy-height 20
 ivy-use-virtual-buffers t
 ivy-count-format "(%d/%d) "
 ivy-display-style 'fancy)

(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist
      '((swiper . nil)
        (t . ivy-posframe-display-at-point)))
(setq ivy-posframe-parameters
      '((left-fringe . 3)
        (right-fringe . 3)))
(ivy-posframe-mode 1)

;; yasnippet
(yas-global-mode 1)

;; diff-hl
(global-diff-hl-mode 1)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; flycheck
(require 'flycheck)
(global-flycheck-mode)
(setq
 flycheck-check-syntax-automatically '(save mode-enable)
 flycheck-indication-mode nil
 js2-include-node-externs t)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(add-hook 'js2-mode-hook 'my/js2-mode-setup)

(provide 'init-packages)

;;; init-packages.el ends here
