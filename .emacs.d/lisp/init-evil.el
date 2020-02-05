;;; init-evil.el --- Evil Setup

;;; Commentary:
;; This file is responsible for setting up Evil.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'evil)

(setq evil-default-state 'normal)
(setq evil-kbd-macro-suppress-motion-error t)

(evil-set-initial-state 'eww-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'neotree-mode 'emacs)
(evil-set-initial-state 'prodigy-mode 'emacs)
(evil-set-initial-state 'dashboard-mode 'emacs)
(evil-set-initial-state 'git-blame-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'emacs)
(evil-set-initial-state 'with-editor-mode 'emacs)
(evil-set-initial-state 'magit-blame-mode 'emacs)
(evil-set-initial-state 'paradox-menu-mode 'emacs)
(evil-set-initial-state 'flycheck-error-list-mode 'emacs)
(evil-set-initial-state 'dap-ui-breakpoints-ui-list-mode 'emacs)

(evil-mode 1)

;; evil-numbers: Enable increment / decrement of numbers with C-= and C-- similar to Vim
(define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

;; evil-escape: Escape from many modes
(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jk")
(global-set-key (kbd "<escape>") 'evil-escape)
(evil-escape-mode)

;; evil-matchit: Match more than parentheses
(global-evil-matchit-mode 1)

;; evil-surround: Emulate vim-surround package
(global-evil-surround-mode 1)

;; evil-commentary: Enable commenting of lines using <s-/>
(evil-commentary-mode)

;; evil-goggles: Show various types of evil actions *visually*
(evil-goggles-mode)
(evil-goggles-use-diff-faces)

;; evil-org: Setup Evil with Org mode
(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; evil-vimish-fold: Enable folding using zf
(evil-vimish-fold-mode 1)

(provide 'init-evil)

;;; init-evil.el ends here
