(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-timeout-seconds 0.25)
 '(company-idle-delay 0.2)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("bd6ced8badda12f95e16e641d76d861de096c691720ede6388a226914e97cf23" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "67798cfaf7b064072bf519ed1ade02a8f4412df89c560e35f25d1936cf35b8ce" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "f30aded97e67a487d30f38a1ac48eddb49fdb06ac01ebeaff39439997cbdd869" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "f357d72451c46d51219c3afd21bb397a33cb059e21db1f4adeffe5b8a9093537" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" default)))
 '(dap-mode 1 nil (dap-mode))
 '(dap-ui-mode 1 nil (dap-ui))
 '(evil-collection-setup-debugger-keys t)
 '(evil-collection-setup-minibuffer t)
 '(exec-path-from-shell-check-startup-files nil)
 '(fci-rule-color "#010F1D")
 '(flycheck-check-syntax-automatically (quote (save mode-enable)))
 '(flycheck-indication-mode nil)
 '(helm-ag-fuzzy-match t)
 '(helm-completion-style (quote emacs))
 '(helm-split-window-inside-p t)
 '(highlight-changes-colors (quote ("#EF5350" "#7E57C2")))
 '(highlight-tail-colors
   (quote
    (("#010F1D" . 0)
     ("#B44322" . 20)
     ("#34A18C" . 30)
     ("#3172FC" . 50)
     ("#B49C34" . 60)
     ("#B44322" . 70)
     ("#8C46BC" . 85)
     ("#010F1D" . 100))))
 '(magit-diff-use-overlays nil)
 '(neo-window-position (quote left))
 '(neo-window-width 45)
 '(nov-text-width 60 t)
 '(org-confirm-babel-evaluate nil t)
 '(org-pretty-entities t t)
 '(org-startup-with-inline-images t t)
 '(package-selected-packages
   (quote
    (multiple-cursors moody org-bullets ace-jump-mode nov gnuplot gnuplot-mode quelpa godot-gdscript quelpa-use-package toml-mode perspective bookmark-plus bookmark+ doom-themes yasnippet-snippets writeroom-mode counsel-projectile counsel org-mode hydra prodigy multi-term company-lsp gist paredit indium buffer-move wakatime-mode shackle evil-vimish-fold flycheck evil-org evil-goggles evil-commentary evil-escape evil-matchit evil-anzu evil-collection org-plus-contrib dap-node dap-mode tern prettier-js spaceline general web-beautify web-mode emmet-mode json-mode yaml-mode php-mode powershell projectile evil-numbers evil-surround evil-mc evil company undo-tree diff-hl magit neotree exec-path-from-shell use-package-ensure-system-package gruvbox-theme diminish use-package)))
 '(pos-tip-background-color "#FFF9DC")
 '(pos-tip-foreground-color "#011627")
 '(powerline-default-separator (quote arrow-fade))
 '(powerline-height 20)
 '(projectile-completion-system (quote ivy))
 '(projectile-enable-caching nil)
 '(projectile-indexing-method (quote alien))
 '(projectile-switch-project-action (quote neotree-projectile-action))
 '(spaceline-highlight-face-func (quote spaceline-highlight-face-evil-state) t)
 '(sublimity-attractive-centering-width 120)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#C792EA")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#FFEB95")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#F78C6C")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#7FDBCA")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#82AAFF"))))
 '(vc-annotate-very-old-color nil)
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil)
 '(weechat-color-list
   (quote
    (unspecified "#011627" "#010F1D" "#DC2E29" "#EF5350" "#D76443" "#F78C6C" "#D8C15E" "#FFEB95" "#5B8FFF" "#82AAFF" "#AB69D7" "#C792EA" "#AFEFE2" "#7FDBCA" "#D6DEEB" "#FFFFFF"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
