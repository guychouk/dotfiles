;;; init-package.el --- Package Setup

;;; Commentary:
;; Initialize Emac's `package` and
;; install packagesif necessary.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(setq package-selected-packages '(
                                  ace-window
                                  all-the-icons
                                  cargo
                                  company
                                  counsel
                                  counsel-projectile
                                  dap-mode
                                  diff-hl
                                  diminish
                                  doom-modeline
                                  doom-themes
                                  emmet-mode
                                  evil
                                  evil-commentary
                                  evil-matchit
                                  evil-surround
                                  evil-vimish-fold
                                  exec-path-from-shell
                                  eyebrowse
                                  flycheck
                                  flycheck-rust
                                  gist
                                  gnuplot
                                  go-mode
                                  hydra
                                  ivy
                                  ivy-posframe
                                  js2-mode
                                  json-mode
                                  lsp-mode
                                  magit
                                  markdown-mode
                                  nov
                                  org-bullets
                                  org-plus-contrib
                                  php-mode
                                  prodigy
                                  projectile
                                  rust-mode
                                  swiper
                                  typescript-mode
                                  wakatime-mode
                                  web-mode
                                  yaml-mode
                                  yasnippet
                                  yasnippet-snippets))

(setq package-enable-at-startup nil)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(provide 'init-package)

;;; init-package.el ends here
