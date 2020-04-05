;;; init-dap.el --- My DAP Setup
;;; Commentary:
;;

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(defun dap/show-locals (session)
  "Show *dap-locals* buffer after stopping on breakpoint in SESSION."
  (when (not (get-buffer-window "*dap-ui-locals*"))
    (dap-ui-locals)))

(defun dap/register-box-node-project-debug-template (name port attach)
  "Register debug template with NAME, PORT and whether to set ATTACH or not."
  (let ((projects-dir "/Users/guyvalariola/Projects/box/projects/"))
    (dap-register-debug-template (capitalize name)
                                 (list :type "node"
                                       :program ""
                                       :port port
                                       :request (if (eq attach t) "attach" "launch")
                                       :remoteRoot "/app"
                                       :localRoot (expand-file-name name projects-dir)))))


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

;; Setup Node.js projects debuggers in `box` campaigns
(dap/register-box-node-project-debug-template "display" "30004" t)
(dap/register-box-node-project-debug-template "player" "30025" t)
(dap/register-box-node-project-debug-template "match" "30010" t)
(dap/register-box-node-project-debug-template "campaign-api" "30008" t)
(dap/register-box-node-project-debug-template "users" "30001" t)
;; Setup Chrome projects debuggers in `box` campaigns
(dap-register-debug-template "SDK"
                             (list :name "SDK"
                                   :type "chrome"
                                   :mode "url"
                                   :request "launch"
                                   :url "https://sdk.apester.local.com/"
                                   :webRoot "/Users/guyvalariola/Projects/box/projects/sdk/src"))

(provide 'init-dap)

;;; init-dap.el ends here
