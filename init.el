;;
;; Emacs Configuration
;;
;; A quick & ugly PATH solution to Emacs on Mac OSX
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

;; bootstrap cask so that we can use use-package, because i'm lazy
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; generic setup
;; i *hate* these keybindings outside of the command line
(when window-system
  (when (eq (key-binding (kbd "s-m")) 'iconify-frame)
      (global-unset-key (kbd "s-m")))
  (when (eq (key-binding (kbd "C-x C-z")) 'suspend-frame)
      (global-unset-key (kbd "C-x C-z")))
  (when (eq (key-binding (kbd "C-z")) 'suspend-frame)
      (global-unset-key (kbd "C-z"))))

;; enable winner mode for restoring window configurations
(winner-mode 1)

;; default fonts
(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  ;; default font size (point * 10)
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 140
                      :weight 'normal
                      :width 'normal))
;; temporary fix for El Capitan
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;; Always install missing packages
(setq use-package-always-ensure t)

;; don't make me type, i know what i'm doing
(defalias 'yes-or-no-p 'y-or-n-p)

;; other general settings
(setq apropos-do-all t
      cursor-color "#708183"
      fci-rule-color "#e9e2cb"
      fill-column 80
      global-visual-line-mode t
      inhibit-startup-screen t
      kill-whole-line t
      linum-format " %7i "
      require-final-newline t
      default-tab-width 4)

;; Make sure that we start with sane defaults
(use-package better-defaults)

;; load and configure any required packages
(use-package auto-complete
  :config (setq ac-auto-start nil
                ac-delay 2.0))
(use-package avy
  :config (avy-setup-default)
  :bind (("C-;" . avy-goto-char-2)))
(use-package base16-theme)
(use-package company
  :diminish company-mode
  :config (global-company-mode))
(use-package projectile
  :config (projectile-global-mode))
(use-package feature-mode)
(use-package fic-mode
  :diminish fic-mode
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (fic-mode 1))))
(use-package flx)
(use-package idle-highlight-mode
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (idle-highlight-mode t))))
(use-package highlight-parentheses)
(use-package magit
  :bind ("C-c g" . magit-status))
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl))
(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))
(use-package pretty-lambdada
  :config (pretty-lambda-for-modes))
(use-package restclient)
(use-package smex)
(use-package which-key
  :config (which-key-mode))
(use-package yaml-mode)
(use-package yasnippet
  :config (yas-global-mode 1))

;; changes to generic programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (make-local-variable 'column-number-mode)
            (column-number-mode t)
            (when window-system (hl-line-mode t))))

;; ruby-specific changes
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (set-fill-column 100))))

;; set up polymode for ConTeXt / Ruby
(use-package polymode
  :config
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
  (define-polymode poly-latex-ruby-mode pm-poly/latex-ruby))

;; TODO: set up a mapping for requirements templates (Feature/Ruby modes)

;;
;; generic keybindings
;;
(progn
  ;; i use this constantly - probably a bug
  (global-set-key (kbd "C-x g") 'rgrep)
  ;; Font size
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  ;; OS X fullscreen mode
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  ;; Shift+direction
  (windmove-default-keybindings)
  ;; M-S-6 is awkward
  (global-set-key (kbd "C-c q") 'join-line))

;; emacs internal configuration management
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" default)))
 '(desktop-restore-in-current-display t)
 '(desktop-save-mode t)
 '(fic-background-color "dark gray")
 '(frame-background-mode (quote dark))
 '(magit-revert-buffers t t)
 '(menu-bar-mode t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'base16-eighties-dark)
