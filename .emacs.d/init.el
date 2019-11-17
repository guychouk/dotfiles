;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize package manager
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'diminish)
  (package-install 'use-package))

;; Provides performance boost once compiled (according to the docs)
(eval-and-compile (require 'use-package))
;; Implicit :ensure for all packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; Helps suppress minor modes indications in mode line.
(require 'diminish)
;; Adds option of binding keys to packages.
(require 'bind-key)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Quiet Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message t)

;; Revert these files without asking.
(setq revert-without-query '(".*"))

;; Don’t add new lines past end of file
(setq next-line-add-newlines nil)

;; Replace yes or no prompt with y or n prompt.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable word-wrap
(setq-default truncate-lines t)

;; Use only spaces (no tabs)
(setq-default indent-tabs-mode nil)

;; Set line numbers
(global-display-line-numbers-mode)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set file encoding to UTF-8
(set-language-environment "UTF-8")

;; Set tab width to 4 spaces
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

;; Remove annoying bell
(setq ring-bell-function 'ignore)

;; Set `custom-file` location
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Disable backup, auto-save and lockfiles
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Enable server for command line support
(if (display-graphic-p)
    (server-start))

;; Enable Winner mode (undo/redo for window layout)
(winner-mode 1)

;; Setup Org Agenda to watch work.org file
(setq org-agenda-files '("~/Drive/etc/work.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Sets a constant position and size for buffers based on a set of rules
(use-package shackle
  :config
  (setq shackle-rules '(("work.org" :popup t :align above :select t :ratio 0.25)))
  (shackle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/js2-mode-setup nil
  "My js2-mode setup."
  (flycheck-mode t)
  (setq js-indent-level 2)
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
  "Eval and print EXPRESSION."
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
  (hydra-dap/body)
  (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
    (save-excursion
      ;; display locals
      (unless (my/window-visible dap-ui--locals-buffer)
        (dap-ui-locals))
      ;; display sessions
      (unless (my/window-visible dap-ui--sessions-buffer)
        (dap-ui-sessions)))))

(defun my/hide-debug-windows (session)
  "Hide debug windows when all debug sessions are dead."
  (unless (-filter 'dap--session-running (dap--get-sessions))
    (and (get-buffer dap-ui--sessions-buffer)
         (kill-buffer dap-ui--sessions-buffer))
    (and (get-buffer dap-ui--locals-buffer)
         (kill-buffer dap-ui--locals-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :hook (elisp-mode . paredit-mode))

(use-package counsel)

(use-package counsel-projectile
  :requires (counsel projectile))

(use-package swiper
  :after evil
  :bind (:map evil-normal-state-map
         ("/" . swiper)))

(use-package ivy :ensure t
  :diminish (ivy-mode . "")             ; does not display ivy in the modeline
  :init
  (ivy-mode 1)                          ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
  (setq ivy-height 20)                   ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ")     ; count format, from the ivy help page
  (setq ivy-display-style 'fancy))

(use-package magit)

(use-package gist)

(use-package yasnippet)

(use-package neotree
  :custom
  (neo-window-position 'left)
  (neo-window-width 45))

(use-package git-gutter
  :diminish
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
    :name "SDK"
    :command "docker-compose"
    :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "sdk")
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
  :config
  (global-company-mode 1))

;; LSP backend for Company
(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends))


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

;; Enable folding using zf
(use-package evil-vimish-fold
  :after evil
  :diminish
  :config
  (evil-vimish-fold-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode)
(use-package emmet-mode)
(use-package php-mode :mode ("\\.php\\'" . php-mode))
(use-package yaml-mode :mode ("\\.yml\\'" . yaml-mode))
(use-package web-mode
  :after js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :mode ("\\.jsx\\'" . js2-mode))

(use-package lsp-mode
  :hook (js2-mode . lsp)
  :ensure-system-package ((tsserver . "npm i -g typescript")
                          (typescript-language-server . "npm i -g typescript-language-server")))

(use-package dap-mode
  :hook ((dap-stopped . my/show-debug-windows)
         (dap-terninated . my/hide-debug-windows))
  :custom
  (dap-mode 1)
  (dap-ui-mode 1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HYDRAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :config
  ;; Taken from http://doc.rix.si/org/fsem.html
  (defhydra hydra-zoom (global-map "C-c z")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

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
    ("q" nil "Quit")
    ("w" save-buffer "Save")
    ("k" evil-window-up "Up")
    ("j" evil-window-down "Down")
    ("h" evil-window-left "Left")
    ("l" evil-window-right "Right")
    ("p" winner-undo "Winner: Undo")
    ("n" winner-redo "Winner: Redo")
    ("b" ivy-switch-buffer "Buffers")
    ("Q" evil-window-delete "Quit Buffer")
    ("L" evil-window-vsplit "Vertical Split")
    ("J" evil-window-split "Horizontal Split")
    ("rj" jump-to-register "Jump to save Window Layout")
    ("ff" delete-other-windows "Delete all other windows")
    ("swc" window-configuration-to-register "Save Window Layout")
    ("f" counsel-find-file "Search for File")
    ("n" neotree-toggle "Browse Current Directory")
    ("cp" my/put-file-name-on-clipboard "Copy Filename")
    ("z" (lambda() (interactive)(find-file "~/.zshrc")) "ZSH Config")
    ("e" (lambda() (interactive)(find-file "~/.emacs.d/init.el")) "Init File")
    ("org" (lambda() (interactive)(find-file "~/Drive/etc/work.org")) "Work Org")
    ("osx" (lambda() (interactive)(find-file "~/Drive/etc/mac-setup.sh")) "OSX Setup File"))

  (defhydra hydra-dap (:color pink :hint nil :foreign-keys run)
    "
^Stepping^          ^Switch^                 ^Breakpoints^           ^Eval
^^^^^^^^-----------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bt_: Toggle            _ee_: Eval
^ ^                 ^ ^                      ^ ^                     _ec_: Eval to Clipboard
_i_: Step in        _st_: Thread             _bd_: Delete            _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add               _es_: Eval thing at point
_c_: Continue       _sl_: List locals        _bc_: Set condition     _eii_: Inspect
_r_: Restart frame  _sb_: List breakpoints   _bh_: Set hit count     _eir_: Inspect region
_Q_: Disconnect     _sS_: List sessions      _bl_: Set log message   _eis_: Inspect thing at point
"
    ("n" dap-next)
    ("i" dap-step-in)
    ("o" dap-step-out)
    ("c" dap-continue)
    ("r" dap-restart-frame)
    ("ss" dap-switch-session)
    ("st" dap-switch-thread)
    ("sf" dap-switch-stack-frame)
    ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints)
    ("sS" dap-ui-sessions)
    ("bt" dap-breakpoint-toggle)
    ("ba" dap-breakpoint-add)
    ("bd" dap-breakpoint-delete)
    ("bc" dap-breakpoint-condition)
    ("bh" dap-breakpoint-hit-condition)
    ("bl" dap-breakpoint-log-message)
    ("ee" dap-eval)
    ("ec" my/dap-eval-to-clipboard)
    ("er" dap-eval-region)
    ("es" dap-eval-thing-at-point)
    ("eii" dap-ui-inspect)
    ("eir" dap-ui-inspect-region)
    ("eis" dap-ui-inspect-thing-at-point)
    ("q" nil "quit" :color blue)
    ("Q" dap-disconnect :color red)))
