;;; init-dap.el --- My DAP Setup
;;; Commentary:
;;

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

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
(add-hook 'dap-terminated-hook 'dap/hide-locals)

(provide 'init-dap)

;;; init-dap.el ends here
