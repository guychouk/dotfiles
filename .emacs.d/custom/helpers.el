;;; helpers.el --- Various helper methods

;;; Commentary:
;; Generic helper utilities file.

;;; Code:

(defun my/window-visible (b-name)
  "Return whether B-NAME is visible."
  (-> (-compose 'buffer-name 'window-buffer)
      (-map (window-list))
      (-contains? b-name)))

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

;;; helpers.el ends here
