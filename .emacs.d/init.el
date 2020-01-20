;;; init.el --- My Init file.
;;; Commentary:
;;

;;; Code:

(defconst custom-init-dir "~/.emacs.d/custom/" "Custom init files dir.")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Only use this if OS is Mac OSX to fix the $PATH environment variable
(when (memq window-system '(mac ns))
  (package-install 'exec-path-from-shell)
  (defvar exec-path-from-shell-check-startup-files nil)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(defun load-custom-file (file)
  "Load FILE from 'custom-init-dir'."
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file custom-init-dir)))

(load-custom-file "general.el")
(load-custom-file "gui.el")
(load-custom-file "evil.el")
(load-custom-file "helpers.el")
(load-custom-file "packages.el")
(load-custom-file "hydras.el")
(load-custom-file "dap.el")
(load-custom-file "services.el")
(load-custom-file "modes.el")

;;; init.el ends here
