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

;; set up polymode for ConTeXt / Ruby
(require 'polymode)
(defcustom pm-inner/erb-ruby
  (pm-hbtchunkmode "ruby"
                   :mode 'ruby-mode
                   :head-reg  "<%"
                   :tail-reg  "%>")
  "Ruby chunk"
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/latex-ruby
  (pm-polymode-one "latex-ruby"
                   :hostmode 'pm-host/latex
                   :innermode 'pm-inner/erb-ruby)
  "ERB LaTeX/Ruby typical polymode."
  :group 'polymodes
  :type 'object)

(define-polymode poly-latex-ruby-mode pm-poly/latex-ruby)

;; reduce gc frequency on today's machines
(setq gc-cons-threshold 10000000)

;; general configuration
(setq ac-auto-start nil
      ac-delay 2.0
      apropos-do-all t
      color-theme-sanityinc-solarized-rgb-is-srgb t
      cursor-color "#708183"
      fci-rule-color "#e9e2cb"
      fill-column 80
      global-visual-line-mode t
      inhibit-startup-screen t
      js-indent-level 2
      kill-whole-line t
      linum-format " %7i "
      reb-re-syntax (quote string)
      require-final-newline t
      visible-bell t)

;; ido mode support
(require 'flx-ido)
(ido-mode 1)
(flx-ido-mode 1)
(ido-ubiquitous-mode)
(setq ido-enable-flex-matching t
      ido-use-faces nil
      ido-everywhere t)

(global-company-mode)

;; enable yasnippets
(require 'yasnippet)
(yas-reload-all)
(yas-global-mode 1)

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

;; ruby support
(add-hook 'ruby-mode-hook
          (lambda ()
            (set-fill-column 100)))

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

;; scala support
(require 'scala-mode2)

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
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(desktop-save-mode t)
 '(feature-cucumber-command
   "TEST_DRIVER=poltergeist bundle exec cucumber {options} {feature}")
 '(feature-ruby-command "bundle exec ruby")
 '(frame-background-mode (quote dark))
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
