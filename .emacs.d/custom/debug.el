(package-install 'dap-mode)
(require 'dap-mode)
(require 'dap-chrome)
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
