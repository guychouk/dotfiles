;;; packages.el --- My Packages file.
;;; Commentary:
;;

;;; Code:

(package-install 'gist)

(package-install 'magit)

(package-install 'counsel)

(package-install 'eyebrowse)
(eyebrowse-mode t)

(package-install 'nov)
(defvar nov-text-width 60)
(add-to-list 'auto-mode-alist '("\\.epub$" . nov-mode))

(package-install 'company)
(require 'company)
(setq company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)

(package-install 'company-lsp)
(require 'company-lsp)
(push 'company-lsp company-backends)

;; Index and search projects using standard means
;; "brew install the_silver_searcher"
(package-install 'projectile)
(projectile-mode +1)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'ivy)
(setq projectile-indexing-method 'alien)

(package-install 'counsel-projectile)

(package-install 'swiper)
(define-key evil-normal-state-map (kbd "/") 'swiper)

(package-install 'ivy)
(ivy-mode 1)
(setq ivy-height 20)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-display-style 'fancy)

(package-install 'yasnippet)
(yas-global-mode 1)

(package-install 'yasnippet-snippets)

(package-install 'diff-hl)
(global-diff-hl-mode 1)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

; "pip install wakatime"
(package-install 'wakatime-mode)
(global-wakatime-mode)

; "npm i -g eslint"
(package-install 'flycheck)
(global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save mode-enable))
(setq flycheck-indication-mode nil)
(setq js2-include-node-externs t)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(add-hook 'js2-mode-hook 'my/js2-mode-setup)
