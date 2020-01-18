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

; JS: "npm i -g typescript" + "npm i -g typescript-language-server"
; RUST: "rustup component add rls rust-analysis rust-src"
(package-install 'lsp-mode)
(defvar lsp-prefer-flymake nil)

(package-install 'cargo)
(package-install 'rust-mode)
(package-install 'flycheck-rust)
(defvar rust-format-on-save t)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-to-mode 'rust-mode (list "\\.rs$"))
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(package-install 'js2-mode)
(add-hook 'js2-mode-hook 'lsp)
(add-to-mode 'js2-mode (list "\\.js$" "\\.jsx$"))

(package-install 'typescript-mode)
(add-hook 'typescript-mode-hook 'lsp)
(add-to-mode 'typescript-mode (list "\\.ts$" "\\.tsx$"))

(package-install 'rjsx-mode)
(add-to-mode 'rjsx-minor-mode (list "components\\/.*\\.js$" "\\.jsx$" "\\.tsx$"))

(package-install 'emmet-mode)

(package-install 'json-mode)
(add-to-mode 'json-mode (list "\\.json$"))

(package-install 'php-mode)
(add-to-mode 'php-mode (list "\\.php$"))

(package-install 'yaml-mode)
(add-to-mode 'yaml-mode (list "\\.yml$"))

(package-install 'web-mode)
(add-to-mode 'web-mode (list "\\.html$"))


;;; modes.el ends here
