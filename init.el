;;; A quick & ugly PATH solution to Emacs on Mac OSX
(when (string-equal "darwin" (symbol-name system-type))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; enable dash lookup
(global-set-key "\C-cd" 'dash-at-point)

;; Set some window defaults
(setq default-frame-alist
      '((width . 110) (height . 46)))
(menu-bar-mode)

;; I *hate* this keybinding outside of the command line
(when window-system
  (if (eq (key-binding "\C-x\C-z") 'suspend-frame)
      (global-unset-key "\C-x\C-z"))
  (if (eq (key-binding "\C-z") 'suspend-frame)
      (global-unset-key "\C-z")))

(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  ;; default font size (point * 10)
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 140
                      :weight 'normal
                      :width 'normal))

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq delete-old-versions t)

;; reduce gc frequency on today's machines
(setq gc-cons-threshold 10000000)

;; ido mode support
(require 'flx-ido)
(ido-mode 1)
(flx-ido-mode 1)
(ido-ubiquitous-mode)
(setq ido-enable-flex-matching t
      ido-use-faces nil
      ido-everywhere t
      require-final-newline t
      visual-bell t
      apropos-do-all t)

(global-company-mode)

;; enable yasnippets
;; (require 'yasnippet)
;; (yas-reload-all)
;; (yas-global-mode 1)

;; don't EVER put tabs in indents
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; Allow auto-fill-mode in all text modes
; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; enable table editing in markdown mode
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

;; allow eash ssh to vagrant instances
(eval-after-load 'tramp
  '(vagrant-tramp-enable))

;; always enable projectile file finding
(projectile-global-mode)

;; clojure support
(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; (add-hook 'clojure-mode-hook
;;           '(lambda ()
;;              (yas-minor-mode)))

;; cider support
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-to-list 'same-window-buffer-names "*cider*")

(setq nrepl-hide-special-buffers t
      cider-repl-use-pretty-printing t
      cider-popup-stacktraces nil
      cider-auto-select-error-buffer nil)

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-<left>") 'backward-sexp)
     (define-key paredit-mode-map (kbd "C-<right>") 'forward-sexp)
     (define-key paredit-mode-map (kbd "C-S-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-S-<right>") 'paredit-forward-slurp-sexp)
     ;; use the function keys for something useful
     (define-key paredit-mode-map [(f5)] 'cider-jack-in)
     (define-key paredit-mode-map [(f6)] 'cider-repl-set-ns)
     ; (define-key paredit-mode-map [(f7)] 'toggle-nrepl-stack-traces-in-repl)
     (define-key paredit-mode-map [(f8)] 'cider-switch-to-repl-buffer)))

;; (defadvice show-paren-function
;;   (after show-matching-paren-offscreen activate)
;;   "If the matching paren is offscreen, show the matching line in the
;;     echo area. Has no effect if the character before point is not of
;;     the syntax class ')'."
;;   (interactive)
;;   (let ((matching-text nil))
;;     ;; Only call `blink-matching-open' if the character before point
;;     ;; is a close parentheses type character. Otherwise, there's not
;;     ;; really any point, and `blink-matching-open' would just echo
;;     ;; "Mismatched parentheses", which gets really annoying.
;;     (if (char-equal (char-syntax (char-before (point))) ?\))
;;         (setq matching-text (blink-matching-open)))
;;     (if (not (null matching-text))
;;         (message matching-text))))

;; Auto complete
;;(require 'auto-complete-config)
;;(ac-config-default)
;;(define-key ac-completing-map "\M-/" 'ac-stop) ; use M-/ to stop completion
;; ac-nrepl
;; (require 'ac-nrepl)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode))

(require 'highlight-parentheses)

;; invoke puppet mode for .pp files
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; default to github-flavored markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start nil)
 '(ac-delay 2.0)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#6c71c4" "#268bd2" "#eee8d5"])
 '(ansi-term-color-vector
   [unspecified "#393939" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#e8e6df"])
 '(background-mode dark)
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(color-theme-sanityinc-solarized-rgb-is-srgb t)
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("e8c75a832ba978b9ff20dd2b61aedfe5ecf3c84e86ed24fe093850f500177076" "635518bf81b80533e3ee3a76d55f992494ea7bf3018bf58cd3d44f100d66fa8e" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "3b68c1b83adeed5a87121839c36143aeaa25fbdcd65c815259b1fbb6f5cc536c" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(desktop-save-mode t)
 '(fci-rule-color "#e9e2cb")
 '(global-visual-line-mode t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(kill-whole-line t)
 '(linum-format " %7i ")
 '(reb-re-syntax (quote string))
 '(scss-compile-at-save nil)
 '(scss-sass-command "sass")
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
