;;; init.el --- My Init file.
;;; Commentary:
;; Init.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar *is-a-mac* (eq system-type 'darwin) "MacOS check.")
(defvar best-gc-cons-threshold 16777216 "Recomended GC threshold taken from DOOM Emacs (16mb).")
(defvar backup--file-name-handler-alist file-name-handler-alist "Backup 'file-name-handler-alist'.")

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

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(defun require-init (pkg)
  "Load PKG by giving the `load` function an absolute path (faster)."
  (load (file-truename (format "~/.emacs.d/lisp/%s" pkg))))

(push (expand-file-name "~/.emacs.d/lisp") load-path)

(require-init 'base)
(require-init 'init-package)
(require-init 'init-gui)
(require-init 'init-evil)
(require-init 'init-packages)
(require-init 'init-utils)
(require-init 'init-keybindings)
(require-init 'init-dap)
(require-init 'init-modes)

;; Load MacOS settings
(when *is-a-mac*
  (require-init 'init-macos))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold best-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist backup--file-name-handler-alist)))

;;; init.el ends here
