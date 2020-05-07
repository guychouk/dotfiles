;;; init-keybindings.el --- My Keybindings init file.
;;; Commentary:
;; A single file for all of my Hydra definitions.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'hydra)

(defhydra hydra-project (global-map "C-c p" :color blue)
  "Project related"
  ("g" magit-status "Magit Dashboard")
  ("y" counsel-yank-pop "Search kill ring")
  ("s" counsel-projectile-ag "Search in Project")
  ("f" counsel-projectile-find-file "Search for files in project")
  ("o" counsel-projectile-switch-project "Switch Project"))

(defhydra hydra-apropos (:color blue :hint nil)
  "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))

(defhydra hydra-windows (global-map "C-c w" :color blue)
  "Switch eyebrowse window config"
  ("h" winner-undo "Undo Winner")
  ("l" winner-redo "Redo Winner")
  ("1" eyebrowse-switch-to-window-config-1 "Eyebrowse: Window Config 1")
  ("2" eyebrowse-switch-to-window-config-2 "Eyebrowse: Window Config 2")
  ("3" eyebrowse-switch-to-window-config-3 "Eyebrowse: Window Config 3")
  ("4" eyebrowse-switch-to-window-config-4 "Eyebrowse: Window Config 4")
  ("5" eyebrowse-switch-to-window-config-5 "Eyebrowse: Window Config 5")
  ("6" eyebrowse-switch-to-window-config-6 "Eyebrowse: Window Config 6")
  ("7" eyebrowse-switch-to-window-config-7 "Eyebrowse: Window Config 7")
  ("8" eyebrowse-switch-to-window-config-8 "Eyebrowse: Window Config 8")
  ("9" eyebrowse-switch-to-window-config-9 "Eyebrowse: Window Config 9"))

(defhydra hydra-jumps (global-map "C-c j" :exit t)
  "File jumps"
  ("z" (lambda() (interactive)(find-file "~/.zshrc")) "ZSH Config")
  ("i" (lambda() (interactive)(find-file "~/.emacs.d/init.el")) "Init File")
  ("e" (lambda() (interactive)(find-file "~/Drive/etc/.espanso/default.yml")) "Espanso File")
  ("k" (lambda() (interactive)(find-file "~/.emacs.d/lisp/init-keybindings.el")) "Keybindings")
  ("ow" (lambda() (interactive)(find-file "~/Drive/docs/orgs/work.org")) "Work Org")
  ("os" (lambda() (interactive)(find-file "~/Drive/docs/orgs/stem.org")) "STEM Org"))

(defhydra hydra-zoom (global-map "C-c z")
  "Window resizing and text scaling"
  ("q" nil "Quit")
  ("k" shrink-window "Make Smaller")
  ("j" enlarge-window "Make Taller")
  ("l" shrink-window-horizontally "Shrink Horizontally")
  ("h" enlarge-window-horizontally "Enlarge Horizontally")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

;; Remap C-[ in insert mode to "escape"
(define-key evil-insert-state-map (kbd "C-[") 'evil-normal-state)

;; Set ace-window shortcut
(global-set-key (kbd "M-o") 'ace-window)

;; scroll one line at a time
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; hydra help helper
(global-set-key (kbd "C-c h") 'hydra-apropos/body)

;; emmet expand
(global-set-key (kbd "C-c e") 'emmet-expand-yas)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
