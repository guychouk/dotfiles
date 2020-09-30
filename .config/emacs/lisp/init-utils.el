;;; init-utils.el --- Various utility functions

;;; Commentary:
;; Where arbitrary functions live.

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(defun local-require (pkg)
  "Require local PKG in site-lisp directory."
  (unless (featurep pkg)
    (load (expand-file-name
           (cond
            ((eq pkg 'go-mode-load)
             (format "~/.emacs.d/site-lisp/go-mode/%s" pkg))
            (t
             (format "~/.emacs.d/site-lisp/%s/%s" pkg pkg))))
          t t)))

(defun my/window-visible (b-name)
  "Return whether B-NAME is visible."
  (-> (-compose 'buffer-name 'window-buffer)
      (-map (window-list))
      (-contains? b-name)))

(defun org-insert-clipboard-image ()
  "Save image from clipboard with timestamp to /orgs/.imgs directory and paste in org file."
  (interactive)
  (let ((file (concat
               default-directory
               ".imgs/"
                (concat "screenshot" "_" (format-time-string "%Y%m%d_%H%M%S_") ".png"))))
    (shell-command (concat "pngpaste " file))
    (insert (concat "[[" file "]]")))
  (org-display-inline-images))

(defun my-org-hook ()
  "My Org mode hook."
  (local-set-key (kbd "C-c C-p i") 'org-insert-clipboard-image))

(add-hook 'org-mode-hook 'my-org-hook)

(defun sort-lines-by-length (reverse beg end)
  "Sort REVERSE by length from BEG to END."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))

(defun my/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun my/js2-mode-setup nil
  "My js2-mode setup."
  (flycheck-mode t)
  (global-eldoc-mode -1)
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq js2-indent-switch-body t)
  (when (executable-find "eslint")
    (flycheck-select-checker 'javascript-eslint)))

(defun disable-fylcheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(provide 'init-utils)

;;; init-utils.el ends here
