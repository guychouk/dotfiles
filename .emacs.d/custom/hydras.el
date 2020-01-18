;;; hydras.el --- My Hydras file.
;;; Commentary:
;; A single file for all of my Hydra definitions.

;;; Code:

(package-install 'hydra)
(require 'hydra)

(defhydra hydra-leader (evil-normal-state-map "SPC" :exit t)
  "Leader key definitions"
  ("w" save-buffer "Save")
  ("k" evil-window-up "Up")
  ("j" evil-window-down "Down")
  ("h" evil-window-left "Left")
  ("l" evil-window-right "Right")
  ("q" evil-window-delete "Quit")
  ("L" evil-window-vsplit "Vertical Split")
  ("J" evil-window-split "Horizontal Split")
  ("b" ivy-switch-buffer "Buffers")
  ("g" magit-status "Magit Dashboard")
  ("f" counsel-find-file "Search for File")
  ("y" counsel-yank-pop "Search kill ring")
  ("n" neotree-toggle "Browse Current Directory")
  ("sp" counsel-projectile-ag "Search in Project")
  ("ps" prodigy "Prodigy")
  ("pp" counsel-projectile-find-file "Search for files in project")
  ("po" counsel-projectile-switch-project "Switch Project")
  ("term" ansi-term "Terminal")
  ("dir" dired-jump "Open Dired in Current Directory")
  ("1" eyebrowse-switch-to-window-config-1 "Eyebrowse: Window Config 1")
  ("2" eyebrowse-switch-to-window-config-2 "Eyebrowse: Window Config 2")
  ("3" eyebrowse-switch-to-window-config-3 "Eyebrowse: Window Config 3")
  ("4" eyebrowse-switch-to-window-config-4 "Eyebrowse: Window Config 4")
  ("5" eyebrowse-switch-to-window-config-5 "Eyebrowse: Window Config 5")
  ("6" eyebrowse-switch-to-window-config-6 "Eyebrowse: Window Config 6")
  ("7" eyebrowse-switch-to-window-config-7 "Eyebrowse: Window Config 7")
  ("8" eyebrowse-switch-to-window-config-8 "Eyebrowse: Window Config 8")
  ("9" eyebrowse-switch-to-window-config-9 "Eyebrowse: Window Config 9")
  ("cp" my/put-file-name-on-clipboard "Copy Full Path to Clipboard")
  ("ez" (lambda() (interactive)(find-file "~/.zshrc")) "ZSH Config")
  ("ei" (lambda() (interactive)(find-file "~/.emacs.d/init.el")) "Init File")
  ("esp" (lambda() (interactive)(find-file "~/Library/Preferences/espanso/default.yml")) "Espanso File")
  ("eow" (lambda() (interactive)(find-file "~/Drive/docs/orgs/work.org")) "Work Org")
  ("eos" (lambda() (interactive)(find-file "~/Drive/docs/orgs/stem.org")) "STEM Org")
  ("esx" (lambda() (interactive)(find-file "~/Drive/etc/mac-setup.sh")) "OSX Setup File"))

(defhydra hydra-zoom (global-map "C-c z")
  "Window resizing and text scaling"
  ("q" nil "Quit")
  ("k" shrink-window "Make Smaller")
  ("j" enlarge-window "Make Taller")
  ("l" shrink-window-horizontally "Shrink Horizontally")
  ("h" enlarge-window-horizontally "Enlarge Horizontally")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

;;; hydras.el ends here
