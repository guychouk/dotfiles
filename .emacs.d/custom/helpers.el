;;; helpers.el --- Various helper methods

;;; Commentary:
;; Generic helper utilities file.

;;; Code:

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

(defun my/dap-eval-to-clipboard (expression)
  "Eval EXPRESSION and save result to clipboard."
  (interactive "sEval: ")
  (let ((debug-session (dap--cur-active-session-or-die)))
    (if-let ((active-frame-id (-some->> debug-session
                                        dap--debug-session-active-frame
                                        (gethash "id"))))
        (dap--send-message
         (dap--make-request "evaluate"
                            (list :expression (concat "JSON.stringify(" expression ",null,2)")
                                  :frameId active-frame-id))
         (-lambda ((&hash "success" "message" "body"))
           (with-temp-buffer
             (insert (gethash "result" body))
             (clipboard-kill-region (point-min) (point-max))))
         debug-session)
      (error "There is no stopped debug session"))))

(defun my/window-visible (b-name)
  "Return whether B-NAME is visible."
  (-> (-compose 'buffer-name 'window-buffer)
      (-map (window-list))
      (-contains? b-name)))

(defun my/show-debug-windows (session)
  "Show debug windows in SESSION."
  (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
    (save-excursion
      ;; display locals
      (unless (my/window-visible dap-ui--locals-buffer)
        (dap-ui-locals))
      (unless (my/window-visible "*dap-ui-repl*")
        (dap-ui-repl)))))

(defun my/hide-debug-windows nil
  "Hide debug windows when all debug sessions are dead."
  (unless (-filter 'dap--session-running (dap--get-sessions))
    (and (get-buffer dap-ui--locals-buffer)
         (kill-buffer dap-ui--locals-buffer))
    (and (get-buffer "*dap-ui-repl*")
         (kill-buffer "*dap-ui-repl*"))))

;;; helpers.el ends here
