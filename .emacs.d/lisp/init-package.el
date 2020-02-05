;;; init-package.el --- Package Setup

;;; Commentary:
;; Initialize Emac's `package` and
;; install packagesif necessary.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-


(defvar *is-a-mac* (eq system-type 'darwin) "MacOS check.")

(setq package-selected-packages '(all-the-icons
                                  cargo
                                  company
                                  company-lsp
                                  counsel
                                  counsel-projectile
                                  dap-mode
                                  diff-hl
                                  diminish
                                  doom-modeline
                                  doom-themes
                                  emmet-mode
                                  evil
                                  ;; evil-collection
                                  evil-commentary
                                  evil-escape
                                  evil-goggles
                                  evil-matchit
                                  evil-numbers
                                  evil-org
                                  evil-surround
                                  evil-vimish-fold
                                  exec-path-from-shell
                                  eyebrowse
                                  flycheck
                                  flycheck-rust
                                  gist
                                  gnuplot
                                  hydra
                                  ivy
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

;; Only use this if OS is Mac OSX to fix the $PATH environment variable
(when *is-a-mac*
  (when (not (package-installed-p 'exec-path-from-shell))
              (package-install 'exec-path-from-shell))
  (defvar exec-path-from-shell-check-startup-files nil)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES")))

(provide 'init-package)

;;; init-package.el ends here
