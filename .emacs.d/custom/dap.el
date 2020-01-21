;;; dap.el --- My DAP Setup
;;; Commentary:
;;

;;; Code:

(package-install 'dap-mode)

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
(setq dap-ui-buffer-configurations
  `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.35)))
    (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.35)))
    (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.35)))
    (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
    (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.35)))))
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

(dap-register-debug-template "Display"
                             (list :type "node"
                                   :program ""
                                   :port "30004"
                                   :request "attach"
                                   :remoteRoot "/app"
                                   :localRoot "/Users/guyvalariola/Projects/box/projects/display"))
(dap-register-debug-template "Player"
                             (list :type "node"
                                   :program ""
                                   :port "30025"
                                   :request "attach"
                                   :remoteRoot "/app"
                                   :localRoot "/Users/guyvalariola/Projects/box/projects/player"))
(dap-register-debug-template "Match"
                             (list :type "node"
                                   :program ""
                                   :port "30010"
                                   :request "attach"
                                   :remoteRoot "/app"
                                   :localRoot "/Users/guyvalariola/Projects/box/projects/match"))
(dap-register-debug-template "Campaign-API"
                             (list :type "node"
                                   :program ""
                                   :port "30008"
                                   :request "attach"
                                   :remoteRoot "/app"
                                   :localRoot "/Users/guyvalariola/Projects/box/projects/campaign-api"))
(dap-register-debug-template "Users"
                             (list :type "node"
                                   :program ""
                                   :port "30001"
                                   :request "attach"
                                   :remoteRoot "/app/src"
                                   :localRoot "/Users/guyvalariola/Projects/box/projects/users/src"))
(dap-register-debug-template "SDK"
                             (list :name "SDK"
                                   :type "chrome"
                                   :cwd nil
                                   :mode "url"
                                   :request "attach"
                                   :port 9222
                                   :url "https://sdk.apester.local.com/*"
                                   :webRoot "/Users/guyvalariola/Projects/box/projects/sdk/src"))

;;; dap.el ends here
