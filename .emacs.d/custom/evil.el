;;; evil.el --- Evil Setup

;;; Commentary:
;; This file is responsible for setting up Evil.

;;; Code:

(package-install 'evil)
(require 'evil)

(setq evil-default-state 'normal)
(setq evil-kbd-macro-suppress-motion-error t)

(evil-set-initial-state 'eww-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'neotree-mode 'emacs)
(evil-set-initial-state 'dashboard-mode 'emacs)
(evil-set-initial-state 'git-blame-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'emacs)
(evil-set-initial-state 'with-editor-mode 'emacs)
(evil-set-initial-state 'magit-blame-mode 'emacs)
(evil-set-initial-state 'paradox-menu-mode 'emacs)
(evil-set-initial-state 'flycheck-error-list-mode 'emacs)
(evil-set-initial-state 'dap-ui-breakpoints-ui-list-mode 'emacs)

(evil-mode 1)

;; Enable increment / decrement of numbers with C-= and C-- similar to Vim
(package-install 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

;; Escape from many modes
(package-install 'evil-escape)
(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jk")
(global-set-key (kbd "<escape>") 'evil-escape)
(evil-escape-mode)

;; Match more than parentheses
(package-install 'evil-matchit)
(global-evil-matchit-mode 1)

;; Emulate vim-surround package
(package-install 'evil-surround)
(global-evil-surround-mode 1)

;; Enable commenting of lines using <s-/>
(package-install 'evil-commentary)
(evil-commentary-mode)

;; Show various types of evil actions *visually*
(package-install 'evil-goggles)
(evil-goggles-mode)
(evil-goggles-use-diff-faces)

;; Setup Evil with Org mode
(package-install 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; Enable folding using zf
(package-install 'evil-vimish-fold)
(evil-vimish-fold-mode 1)

;;; evil.el ends here
