;;; modes.el --- Modes Setup file

;;; Commentary:
;; This file is responsible for setting up major
;; and minor modes for different source files.

;;; Code:

(defun add-to-mode (mode lst)
  "Add LST of file extensions to MODE."
  (dolist (file lst)
    (add-to-list 'auto-mode-alist
                 (cons file mode))))

(package-install 'rust-mode)
(defvar rust-format-on-save t)
(package-install 'cargo)
(package-install 'flycheck-rust)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(package-install 'js2-mode)
(add-to-mode 'js2-mode (list "\\.js$" "\\.jsx$"))
(package-install 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
(package-install 'rjsx-mode)
(add-to-list 'auto-mode-alist '("components\\/.*\\.js$" . rjsx-minor-mode))

(package-install 'emmet-mode)

(package-install 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(package-install 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(package-install 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(package-install 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . yaml-mode))

; "npm i -g typescript" + "npm i -g typescript-language-server"
(package-install 'lsp-mode)
(add-hook 'js2-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'lsp)
(defvar lsp-prefer-flymake nil)

;;; modes.el ends here
