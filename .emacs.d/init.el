;;; package --- Summary
;;; Commentary:
;; init.el --- Emacs configuration

;;                 Setup
;; --------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install 'use-package)
(use-package use-package-ensure
  :config  (setq use-package-always-ensure t))
(use-package quelpa-use-package)

;; Add a :use-package-ensure-system symbol which enables
;; installation of *system* packages before installing a package.
(use-package use-package-ensure-system-package)

;; Only use this if OS is Mac OSX to fix the $PATH environment variable
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;;               General
;; --------------------------------------

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Use only spaces (no tabs)
(setq-default indent-tabs-mode nil)
(setq
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-scratch-message nil
 inhibit-startup-echo-area-message t
 ;; Setup Org Agenda to watch work.org file
 org-agenda-files '("~/Drive/etc/work.org")
 create-lockfiles nil
 auto-save-default nil
 ;; Disable backup, auto-save and lockfiles
 make-backup-files nil
 ;; Set `custom-file` location
 custom-file "~/.emacs.d/custom.el"
 ;; Remove annoying bell
 ring-bell-function 'ignore
 tab-stop-list (number-sequence 4 200 4)
 ;; Set tab width to 4 spaces
 tab-width 4
 ;; Disable word-wrap
 truncate-lines t
 ;; Don’t add new lines past end of file
 next-line-add-newlines nil
 ;; Revert files without asking.
 revert-without-query '(".*")
 )

;; Start initial frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Replace yes or no prompt with y or n prompt.
(fset 'yes-or-no-p 'y-or-n-p)

;; Set line numbers
(global-display-line-numbers-mode)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set file encoding to UTF-8
(set-language-environment "UTF-8")

(load custom-file 'noerror)

;; Enable server for command line support
(if (display-graphic-p)
    (server-start))

;; Enable Winner mode (undo/redo for window layout)
(winner-mode 1)

;; Auto-update changed files
(global-auto-revert-mode t)

;; Kill terminal buffer on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
	ad-do-it
	(kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Set default ansi-term shell
(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;             GUI Settings
;; --------------------------------------

;; Remove menu-bar
(menu-bar-mode -1)

;; Remove tool-bar
(tool-bar-mode -1)

;; Remove scroll-bar
(toggle-scroll-bar -1)

;; Set font based on OS
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (add-to-list 'default-frame-alist
                 '(font . "Liberation Mono 10"))))
 ((string-equal system-type "darwin")
  (progn
    (add-to-list 'default-frame-alist
                 '(font . "Iosevka 15")))))

;; Remove ARev indicator from modeline
(diminish 'auto-revert-mode)

;; Remove Undo-Tree indicator from modeline
(diminish 'undo-tree-mode)

;; Remove ElDoc indicator from modeline
(diminish 'eldoc-mode)

;; Themes
(use-package doom-themes
  :config
  (doom-themes-org-config)
  (load-theme 'doom-one t))

;; Sets a constant position and size for buffers based on a set of rules
(use-package shackle
  :config
  (setq shackle-rules '(("*dap-ui-repl*" :popup t :align below :ratio 0.25)
                        ("work.org" :popup t :align above :select t :ratio 0.25)))
  (shackle-mode))

;;                Functions
;; --------------------------------------

(defun my/change-of-perspective ()
  "Load main state from Drive"
  (interactive)
  (persp-state-load "~/Drive/etc/main.persp"))

(defun my/perspective-save ()
  "Save project state to Drive"
  (interactive)
  (persp-state-save "~/Drive/etc/main.persp"))

(defun my/js2-mode-setup nil
  "My js2-mode setup."
  (flycheck-mode t)
  (setq js-indent-level 2)
  (setq js2-indent-switch-body t)
  (when (executable-find "eslint")
    (flycheck-select-checker 'javascript-eslint)))

(defun disable-fylcheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(defun my/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun my/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

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
  "Show debug windows."
  (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
    (save-excursion
      ;; display locals
      (unless (my/window-visible dap-ui--locals-buffer)
        (dap-ui-locals))
      (unless (my/window-visible "*dap-ui-repl*")
        (dap-ui-repl)))))

(defun my/hide-debug-windows (session)
  "Hide debug windows when all debug sessions are dead."
  (unless (-filter 'dap--session-running (dap--get-sessions))
    (and (get-buffer dap-ui--locals-buffer)
         (kill-buffer dap-ui--locals-buffer))
    (and (get-buffer "*dap-ui-repl*")
         (kill-buffer "*dap-ui-repl*"))
    (mapcar (function kill-buffer) (remove-if-not (apply-partially #'string-match-p "\*SDK.*") (mapcar (function buffer-name) (buffer-list))))))

;;                Packages
;; --------------------------------------

;; Org-mode + Contrib
(use-package gnuplot)
(use-package org
  :pin org
  :ensure org-plus-contrib
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-pretty-entities t)
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images t)
  :config
  (require 'ob-js)
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell . t)
				 (gnuplot . t)
				 (js . t))))
(use-package org-bullets
  :requires org
  :config
  (org-bullets-mode 1))

;; Amazing buffer & project management tool.
;; Screw Drew Adams and bookmark+.
(use-package perspective
  :bind (
         ("C-x x x" . my/perspective-save)
         ("C-x x l" . my/change-of-perspective))
  :config
  (persp-mode))

(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :custom
  (nov-text-width 60))

(use-package paredit
  :hook (elisp-mode . paredit-mode))

(use-package counsel)

(use-package counsel-projectile
  :requires (counsel projectile))

(use-package avy
  :custom
  (avy-timeout-seconds 0.25))

(use-package swiper
  :after evil
  :bind (:map evil-normal-state-map
         ("/" . swiper)))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy))

(use-package magit)

(use-package gist)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :requires yasnippet)

(use-package neotree
  :custom
  (neo-window-position 'left)
  (neo-window-width 45))

(use-package diff-hl
  :diminish
  :config
  (global-diff-hl-mode 1)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package wakatime-mode
  :diminish
  :ensure-system-package (wakatime . "pip install wakatime")
  :config
  (global-wakatime-mode))

(use-package flycheck
  :diminish
  :init (global-flycheck-mode)
  :ensure-system-package (eslint . "npm i -g eslint")
  :hook
  (org-src-mode . disable-fylcheck-in-org-src-block)
  (emacs-lisp-mode . disable-fylcheck-in-org-src-block)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  (flycheck-indication-mode nil)
  :config
  (setq js2-include-node-externs t)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (add-hook 'js2-mode-hook 'my/js2-mode-setup))

;; Index and search projects using standard means
(use-package projectile
  :after ivy
  :diminish
  :ensure-system-package (ag . "brew install the_silver_searcher")
  :config
  (projectile-mode +1)
  :custom
  (projectile-enable-caching nil)
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien))

;; Enables management of predefined services
(use-package prodigy
  :config
  (prodigy-define-service
    :name "NPM Watch"
    :command "npm"
    :args '("run" "watch")
    :cwd "/Users/guyvalariola/Projects/loans/"
    :stop-signal 'sigterm
    :tags '(personal))
  (prodigy-define-service
    :name "NPM Hot"
    :command "npm"
    :args '("run" "hot")
    :cwd "/Users/guyvalariola/Projects/loans/"
    :stop-signal 'sigterm
    :tags '(personal))
  (prodigy-define-service
    :name "NPM Start"
    :command "npm"
    :args '("start")
    :cwd "/Users/guyvalariola/Projects/loans/"
    :stop-signal 'sigterm
    :tags '(personal))
  (prodigy-define-service
    :name "SDK"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "sdk")
    :stop-signal 'sigterm
    :tags '(work sdk))
  (prodigy-define-service
    :name "SDK (Tests)"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "sdk-tests")
    :stop-signal 'sigterm
    :tags '(work sdk))
  (prodigy-define-service
    :name "Editor"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "editor")
    :stop-signal 'sigterm
    :tags '(work sdk))
  (prodigy-define-service
    :name "Campaign"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "campaign")
    :stop-signal 'sigterm
    :tags '(work sdk))
  (prodigy-define-service
    :name "Display"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "display")
    :stop-signal 'sigterm
    :tags '(work sdk))
  (prodigy-define-service
    :name "Campaign-API"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "campaign-api")
    :stop-signal 'sigterm
    :tags '(work sdk))
  (prodigy-define-service
    :name "Player"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "player")
    :stop-signal 'sigterm
    :tags '(work sdk)))

;; Auto complete engine for Emacs
(use-package company
  :diminish
  :bind (:map evil-insert-state-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-j" . company-complete-selection))
  :custom
  (company-dabbrev-downcase nil)
  :config
  (global-company-mode 1))

;; LSP backend for Company
(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends))

;;                   EVIL
;; --------------------------------------

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (setq evil-default-state 'normal)
  (evil-set-initial-state 'git-commit-mode 'emacs)
  (evil-set-initial-state 'with-editor-mode 'emacs)
  (evil-set-initial-state 'paradox-menu-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'magit-blame-mode 'emacs)
  (evil-set-initial-state 'git-blame-mode 'emacs)
  (evil-set-initial-state 'dap-ui-breakpoints-ui-list-mode 'emacs)
  (add-hook 'git-commit-mode-hook 'evil-emacs-state)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (evil-mode 1))

;; Collection of evil keybindings for the parts of Emacs that Evil does not cover properly
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-setup-debugger-keys t)
  :config
  (evil-collection-init))

;; Enable increment / decrement of numbers with C-= and C-- similar to Vim
(use-package evil-numbers
  :after evil
  :bind (:map evil-normal-state-map
              ("C-=" . evil-numbers/inc-at-pt)
              ("C--" . evil-numbers/dec-at-pt)))

;; Escape from many modes
(use-package evil-escape
  :after evil
  :diminish
  :config
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  (global-set-key (kbd "<escape>") 'evil-escape)
  (evil-escape-mode))

;; Match more than parentheses
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; Emulate vim-surround package
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Enable commenting of lines using <s-/>
(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode))

;; Show various types of evil actions *visually*
(use-package evil-goggles
  :after evil
  :diminish
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; Setup Evil with Org mode
(use-package evil-org
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
	:ensure t
	:config
	)


;; Enable folding using zf
(use-package evil-vimish-fold
  :after evil
  :diminish
  :config
  (evil-vimish-fold-mode 1))

;;                  MODES
;; --------------------------------------

(use-package js2-mode)
(use-package emmet-mode)
(use-package php-mode :mode ("\\.php\\'" . php-mode))
(use-package yaml-mode :mode ("\\.yml\\'" . yaml-mode))
(use-package toml-mode
  :mode (("\\.tscn\\'" . toml-mode)))
(use-package web-mode
  :after js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.html\\'" . web-mode)
         ("\\.jsx\\'" . js2-mode)))

(use-package lsp-mode
  :diminish
  :hook (js2-mode . lsp)
  :ensure-system-package ((tsserver . "npm i -g typescript")
                          (typescript-language-server . "npm i -g typescript-language-server")))

(use-package dap-mode
  :bind (("<f5>" . dap-debug)
         ("<f6>" . dap-step-out)
         ("<f7>" . dap-step-in)
         ("<f8>" . dap-next)
         ("<f9>" . dap-continue)
         ("S-<f8>" . dap-breakpoint-toggle))
  :hook ((dap-stopped . my/show-debug-windows)
         (dap-terminated . my/hide-debug-windows))
  :custom
  (dap-mode 1)
  (dap-ui-mode 1)
  :config
  (require 'dap-node)
  (dap-register-debug-template "Display"
                               (list :type "node"
                                     :program ""
                                     :port "30004"
                                     :request "attach"
                                     :remoteRoot "/app"
                                     :localRoot "/Users/guyvalariola/Projects/box/projects/display"))
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
  (require 'dap-chrome)
  (dap-register-debug-template "SDK" (list
                                      :name "SDK"
                                      :type "chrome"
                                      :cwd nil
                                      :mode "url"
                                      :request "attach"
                                      :port 9222
                                      :url "https://sdk.apester.local.com/*"
                                      :webRoot "/Users/guyvalariola/Projects/box/projects/sdk/src")))

;;                 HYDRAS
;; --------------------------------------

(use-package hydra
  :config

  (defhydra hydra-zoom (global-map "C-c z")
    "window resizing and zoom"
    ("q" nil "Quit")
    ("j" enlarge-window "Make Taller")
    ("k" shrink-window "Make Taller")
    ("h" enlarge-window-horizontally "Enlarge Horizontally")
    ("l" shrink-window-horizontally "Shrink Horizontally")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out"))

  (defhydra hydra-search (evil-normal-state-map "C-s" :exit t)
    "Search"
    ("q" nil "Quit")
    ("s" swiper "Search in File")
    ("p" counsel-projectile-ag "Search in Project"))

  (defhydra hydra-project (evil-normal-state-map "C-p" :exit t)
    "Project management"
    ("q" nil "Quit")
    ("p" counsel-projectile-find-file "Search for files in project")
    ("s" counsel-projectile-switch-project "Switch Project"))

  (defhydra hydra-leader (evil-normal-state-map "SPC" :exit t)
    "Window management"
    ("w" save-buffer "Save")
    ("k" evil-window-up "Up")
    ("j" evil-window-down "Down")
    ("h" evil-window-left "Left")
    ("l" evil-window-right "Right")
    ("q" evil-window-delete "Quit")
    ("p" prodigy "Prodigy")
    ("b" ivy-switch-buffer "Buffers")
    ("g" magit-status "Magit Dashboard")
    ("f" counsel-find-file "Search for File")
    ("y" counsel-yank-pop "Search kill ring")
    ("n" neotree-toggle "Browse Current Directory")
    ("L" evil-window-vsplit "Vertical Split")
    ("J" evil-window-split "Horizontal Split")
    ("cp" my/put-file-name-on-clipboard "Copy Filename")
    ("rj" jump-to-register "Jump to save Window Layout")
    ("dir" dired-jump "Open Dired in Current Directory")
    ("swc" window-configuration-to-register "Save Window Layout")
    ("term" eshell "EShell")
    ("z" (lambda() (interactive)(find-file "~/.zshrc")) "ZSH Config")
    ("e" (lambda() (interactive)(find-file "~/.emacs.d/init.el")) "Init File")
    ("org" (lambda() (interactive)(find-file "~/Drive/docs/orgs/work.org")) "Work Org")
    ("stem" (lambda() (interactive)(find-file "~/Drive/docs/orgs/stem.org")) "STEM Org")
    ("osx" (lambda() (interactive)(find-file "~/Drive/etc/mac-setup.sh")) "OSX Setup File")))

;;; init.el ends here
