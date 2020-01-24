;;; init.el --- My Init file.
;;; Commentary:
;; Init.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(push (expand-file-name "~/.emacs.d/lisp") load-path)

(defvar *is-a-mac* (eq system-type 'darwin) "MacOS check.")
(defvar best-gc-cons-threshold 4000000 "Recomended GC threshold taken from redguardtoo/emacs.d repo.")
(defvar backup--file-name-handler-alist file-name-handler-alist)

;; See https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#unset-file-name-handler-alist-temporarily
(setq file-name-handler-alist nil)

;; Don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defun doom-defer-garbage-collection-h ()
  "Defer garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold best-gc-cons-threshold))))

(defun require-init (pkg)
  "Load PKG by giving the `load` function an absolute path (faster)."
  (load (file-truename (format "~/.emacs.d/lisp/%s" pkg))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

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
                                  rjsx-mode
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
  (package-install 'exec-path-from-shell)
  (defvar exec-path-from-shell-check-startup-files nil)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require-init 'base)
(require-init 'base-gui)
(require-init 'init-evil)
(require-init 'init-packages)
(require-init 'init-utils)
(require-init 'init-hydras)
(require-init 'init-dap)
(require-init 'init-services)
(require-init 'init-modes)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1
                  file-name-handler-alist backup--file-name-handler-alist)))

;;; init.el ends here
