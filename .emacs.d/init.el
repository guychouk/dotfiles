;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (defvar use-package-always-ensure t) ; Implicit :ensure for all packages
  (require 'use-package))
(require 'diminish) ; Helps suppress minor modes indications in mode line.
(require 'bind-key) ; Adds option of binding keys to packages.

;; Add a :use-package-ensure-system symbol which enables
;; installation of *system* packages before installing a package.
(use-package use-package-ensure-system-package)

;; Only add this MacOS to fix the $PATH environment variable
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Quiet Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Disable word-wrap
(setq-default truncate-lines t)

;; Use only spaces (no tabs)
(setq-default indent-tabs-mode nil)

;; Set line numbers
(global-display-line-numbers-mode)

;; Set file encoding to UTF-8
(set-language-environment "UTF-8")

;; Set tab width to 4 spaces
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

;; Remove annoying bell
(setq ring-bell-function 'ignore)

;; Set custom `custom-file` location
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Disable backup, auto-save and lockfiles
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Enable server for command line support
(if (display-graphic-p)
    (server-start))

;; Enable Winner mode
;; (Enable undo/redo for window configurations)
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                 '(font . "Iosevka 14")))))

;; Set theme
(use-package night-owl-theme
  :if (display-graphic-p)
  :init
  (load-theme 'night-owl))

;; Sets a constant location and size for the helm split buffer
(use-package shackle
  :config
  (setq helm-display-function #'pop-to-buffer)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.46)))
  (shackle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-js2-mode-setup nil
  "My js2-mode setup."
  (flycheck-mode t)
  (setq js-indent-level 2)
  (when (executable-find "eslint")
    (flycheck-select-checker 'javascript-eslint)))

(defun disable-fylcheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(defun my-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diminish Emacs modes indications
(diminish 'undo-tree)
(diminish 'eldoc-mode)

(use-package neotree
  :bind ("M-1" . neotree-toggle)
  :custom
  (neo-window-position 'right)
  (neo-window-width 45))

(use-package magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

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
  (flycheck-indication-mode nil)
  :config
  (setq js2-include-node-externs t)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (add-hook 'js2-mode-hook 'my-js2-mode-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (setq evil-default-state 'normal)
  ;; Add after you've decided to use it org-mode natively
  ;; (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'git-commit-mode 'emacs)
  (evil-set-initial-state 'with-editor-mode 'emacs)
  (evil-set-initial-state 'paradox-menu-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-set-initial-state 'magit-blame-mode 'emacs)
  (evil-set-initial-state 'git-blame-mode 'emacs)
  (evil-set-initial-state 'indium-repl-mode 'emacs)
  (evil-set-initial-state 'indium-debugger-mode 'emacs)
  (evil-set-initial-state 'dap-ui-breakpoints-ui-list-mode 'emacs)
  (add-hook 'git-commit-mode-hook 'evil-emacs-state)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (evil-mode 1))

;; Collection of evil keybindings for the parts of Emacs that Evil does not cover properly
(use-package evil-collection
  :after (evil indium)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-setup-debugger-keys t)
  :config
  (evil-collection-init))

;; Enable increment / decrement of numbers with C-= and C-- similar to Vim
(use-package evil-numbers
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)))

;; Escape from many modes
(use-package evil-escape
  :config
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  (progn
    (evil-escape-mode)
    (global-set-key (kbd "<escape>") 'evil-escape))
  :diminish evil-escape-mode)

;; Match more than parentheses
(use-package evil-matchit
  :after evil)

;; Emulate vim-surround package
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Enable commenting of lines using <s-/>
(use-package evil-commentary
  :diminish
  :config
  (evil-commentary-mode))

;; Show various types of evil actions *visually*
(use-package evil-goggles
  :diminish
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; Setup evil multi-cursor
;; (use-package evil-mc
;;   :diminish
;;   :requires evil
;;   :config (global-evil-mc-mode 1))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Enable folding using zf
(use-package evil-vimish-fold
  :diminish
  :config
  (evil-vimish-fold-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ESSENTIALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Index and search projects using standard means
(use-package projectile
  :diminish
  :custom
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien))

;; Auto complete engine for Emacs
(use-package company
  :diminish
  :config
  (global-company-mode 1))

;; LSP backend for Company
(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends))

;; Helm <3
(use-package helm
  :diminish
  :config
  (helm-mode t))

;; Manage projectile projects using Helm
;; (similar behaviour to how VSCode uses <C-p> for traversing files in project)
(use-package helm-projectile
  :after (helm projectile)
  :custom
  (projectile-completion-system 'helm)
  :config
  (projectile-mode)
  (helm-projectile-on))

;; Enable nice fuzzy-search for strings in project using the silver searcher
(use-package helm-ag
  :after (helm projectile)
  :ensure-system-package (ag . "brew install ag")
  :bind (("C-S-f" . helm-projectile-ag)
         :map evil-normal-state-map
         ("C-f" . helm-ag-this-file))
  :custom
  (helm-ag-fuzzy-match t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package php-mode)
(use-package js2-mode)
(use-package yaml-mode)
(use-package emmet-mode)
(use-package web-mode
  :after (js2-mode php-mode yaml-mode)
  :mode ("\\.js\\'" . js2-mode)
  :mode ("\\.jsx\\'" . js2-mode)
  :mode ("\\.php\\'" . php-mode)
  :mode ("\\.css\\'" . css-mode)
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.json\\'" . web-mode)
  :mode ("\\.scss\\'" . scss-mode)
  :mode ("\\.yml\\'" . yaml-mode))

(use-package lsp-mode
  :hook (js2-mode . lsp)
  :ensure-system-package ((typescript . "npm i -g typescript")
                          (tsserver . "npm i -g typescript-language-server")))

(use-package indium)
(use-package dap-mode
  :custom
  (dap-mode 1)
  (dap-ui-mode 1)
  :bind (("S-<f8>" . dap-breakpoint-toggle)
         ("<f5>" . dap-debug)
         ("<f7>" . dap-step-in)
         ("<f6>" . dap-step-out)
         ("<f9>" . dap-continue)
         ("<f8>" . dap-next))
  :config
  (require 'dap-node)
  (dap-register-debug-template "Users API"
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
                                      :request "launch"
                                      :url "https://sdk.apester.local.com/"
                                      :webRoot "/Users/guyvalariola/Projects/box/projects/sdk/src")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :requires evil
  :config
  (general-evil-setup t)
  (general-define-key "M-x" 'helm-M-x)
  (general-imap
    "C-n" #'company-select-next
    "C-p" #'company-select-previous
    "C-j" #'company-complete-selection)
  (general-nmap
    "C-p" #'helm-projectile
    "C-S-P" #' helm-projectile-switch-project)
  ;; add "A" and "I" shortcuts for adding MC at the end / beginning
  (general-vmap
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  ;; set Space as leader key and set <SPC> keybindings
  (general-create-definer leader-def :prefix "SPC")
  ;; set leader shortcuts
  (leader-def 'normal 'override
    "w" 'save-buffer
    "b" 'helm-buffers-list
    "m" 'helm-bookmarks
    "q" 'evil-window-delete
    "j" 'evil-window-down
    "h" 'evil-window-left
    "k" 'evil-window-up
    "l" 'evil-window-right
    "J" 'evil-window-split
    "L" 'evil-window-vsplit
    "ff" 'helm-find-files
    "pi" 'package-install
    "pd" 'package-delete
    "z" (lambda() (interactive)(find-file "~/.zshrc"))
    "e" (lambda() (interactive)(find-file "~/.emacs.d/init.el"))))
