(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#c9b4cf" "#8abeb7" "#c5c8c6"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "332e009a832c4d18d92b3a9440671873187ca5b73c2a42fbd4fc67ecf0379b8c" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "018c8326bced5102b4c1b84e1739ba3c7602019c645875459f5e6dfc6b9d9437" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "8e04ea7bf8a736b0bfacd363f4810ffce774ff9ba24f356172ae2b83307aebb2" "ef4edbfc3ec509612f3cf82476beddd2aeb3da7bdc3a35726337a0cc838a4ef4" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "427fa665823299f8258d8e27c80a1481edbb8f5463a6fb2665261e9076626710" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(desktop-restore-in-current-display t)
 '(desktop-save-mode t)
 '(fci-rule-color "#5c5e5e" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(fringe-mode (quote (0)) nil (fringe))
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#81a2be"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(lsp-fsharp-server-runtime (quote net-core))
 '(magit-revert-buffers t t)
 '(menu-bar-mode nil)
 '(objed-cursor-color "#cc6666")
 '(package-selected-packages
   (quote
    (discover-clj-refactor minions doom-modeline lsp-mode doom-themes magit-todos company-prescient ivy-prescient prescient ns-auto-titlebar ns-auto-titlebar-mode org-review olivetti color-theme-sanityinc-tomorrow org-checklist shackle org-bullets org-mode eyebrowse chruby cider clojure-mode smart-mode-line-powerline-theme smart-mode-line yasnippet yaml-mode which-key undo-tree smex restclient pretty-lambdada paredit markdown-mode magit highlight-parentheses idle-highlight-mode flx fic-mode feature-mode projectile company base16-theme avy auto-complete better-defaults use-package)))
 '(safe-local-variable-values
   (quote
    ((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend"))))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(sp-base-key-bindings (quote paredit))
 '(tool-bar-mode nil)
 '(typescript-indent-level 2 t)
 '(vc-annotate-background "#1d1f21")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b5bd68")
    (cons 40 "#c8c06c")
    (cons 60 "#dcc370")
    (cons 80 "#f0c674")
    (cons 100 "#eab56d")
    (cons 120 "#e3a366")
    (cons 140 "#de935f")
    (cons 160 "#d79e84")
    (cons 180 "#d0a9a9")
    (cons 200 "#c9b4cf")
    (cons 220 "#ca9aac")
    (cons 240 "#cb8089")
    (cons 260 "#cc6666")
    (cons 280 "#af6363")
    (cons 300 "#936060")
    (cons 320 "#765d5d")
    (cons 340 "#5c5e5e")
    (cons 360 "#5c5e5e")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-code-action ((t (:foreground "sea green")))))
