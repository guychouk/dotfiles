;;; init-dap.el --- My DAP Setup
;;; Commentary:
;;

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(defun dap/eval-object-to-clipboard (expression)
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

(defun dap/show-debug-windows (session)
  "Show debug windows in SESSION."
  (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
    (save-excursion
      ;; display locals
      (unless (my/window-visible dap-ui--locals-buffer)
        (dap-ui-locals))
      (unless (my/window-visible "*dap-ui-repl*")
        (dap-ui-repl)))))

(defun dap/hide-debug-windows (session)
  "Hide debug windows when SESSION is terminated."
  (unless (-filter 'dap--session-running (dap--get-sessions))
    (and (get-buffer dap-ui--locals-buffer)
         (kill-buffer dap-ui--locals-buffer))
    (and (get-buffer "*dap-ui-repl*")
         (kill-buffer "*dap-ui-repl*"))))


(defun dap/show-locals (session)
  "Show *dap-locals* buffer after stopping on breakpoint in SESSION."
  (when (not (get-buffer-window "*dap-ui-locals*"))
    (dap-ui-locals)))

(defun dap/hide-locals (session)
  "Kill *dap-locals* buffer after terminating SESSION."
  (when (get-buffer-window "*dap-ui-locals*")
    (kill-buffer "*dap-ui-locals*")))

(require 'dap-mode)
(require 'dap-ui)
(require 'dap-node)
(require 'dap-chrome)

(dap-node-setup)
(dap-chrome-setup)

(add-hook 'after-init-hook (lambda ()
                             (setq dap-auto-show-output nil)
                             (dap-mode 1)
                             (dap-ui-mode 1)))

(define-key global-map (kbd "<f5>") 'dap-debug)
(define-key global-map (kbd "<f6>") 'dap-step-out)
(define-key global-map (kbd "<f7>") 'dap-step-in)
(define-key global-map (kbd "<f8>") 'dap-next)
(define-key global-map (kbd "<f9>") 'dap-continue)
(define-key global-map (kbd "S-<f8>") 'dap-breakpoint-toggle)

(add-hook 'dap-stopped-hook 'dap/show-locals)
(add-hook 'dap-stopped-hook 'dap/show-debug-windows)
(add-hook 'dap-terminated-hook 'dap/hide-locals)
(add-hook 'dap-terminated-hook 'dap/hide-debug-windows)

(provide 'init-dap)

;;; init-dap.el ends here
