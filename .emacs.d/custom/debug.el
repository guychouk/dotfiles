(package-install 'dap-mode)

(require 'dap-mode)
(require 'dap-node)
(require 'dap-chrome)

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

(define-key global-map (kbd "<f5>") 'dap-debug)
(define-key global-map (kbd "<f6>") 'dap-step-out)
(define-key global-map (kbd "<f7>") 'dap-step-in)
(define-key global-map (kbd "<f8>") 'dap-next)
(define-key global-map (kbd "<f9>") 'dap-continue)
(define-key global-map (kbd "S-<f8>") 'dap-breakpoint-toggle)
(add-hook 'dap-stopped-hook 'my/show-debug-windows)
(add-hook 'dap-terminated-hook 'my/hide-debug-windows)

(setq dap-mode 1)
(setq dap-ui-mode 1)

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
(dap-register-debug-template "SDK" (list
                                    :name "SDK"
                                    :type "chrome"
                                    :cwd nil
                                    :mode "url"
                                    :request "attach"
                                    :port 9222
                                    :url "https://sdk.apester.local.com/*"
                                    :webRoot "/Users/guyvalariola/Projects/box/projects/sdk/src"))
