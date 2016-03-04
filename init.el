;;
;; Emacs Configuration
;;
;; A quick & ugly PATH solution to Emacs on Mac OSX
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

;; bootstrap cask so that we can bootstrap into use-package
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; generic setup
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
;; keybindings
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-x g") 'rgrep)

;; temporary fix for El Capitan
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;; Always install missing packages
(setq use-package-always-ensure t)

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
(use-package auto-complete
  :config (setq ac-auto-start nil
                ac-delay 2.0))
(use-package avy
  :config (avy-setup-default)
  :bind (("C-;" . avy-goto-char-2)))
(use-package better-defaults)
(use-package base16-theme)
(use-package company
  :config (global-company-mode))
(use-package projectile
  :config (projectile-global-mode))
(use-package feature-mode)
(use-package flx)
(use-package idle-highlight-mode)
(use-package highlight-parentheses)
(use-package magit)
(use-package markdown-mode)
(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))
(use-package restclient)
(use-package smex)
(use-package which-key
  :config (which-key-mode))
(use-package yaml-mode)
(use-package yasnippet)

(defun my-coding-hook ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t))

(add-hook 'prog-mode-hook 'my-coding-hook)

;; ruby support
(add-hook 'ruby-mode-hook
          (lambda ()
            (set-fill-column 100)))

;;
;; generic keybindings
;;
(progn
  ;; Font size
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)
  (windmove-default-keybindings)        ; Shift+direction
  ;; M-S-6 is awkward
  (global-set-key (kbd "C-c q") 'join-line)
  ;; So good!
  (global-set-key (kbd "C-c g") 'magit-status))

;; emacs internal configuration management
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(desktop-restore-in-current-display t)
 '(desktop-save-mode t)
 '(frame-background-mode (quote dark))
 '(magit-revert-buffers t t)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(custom-safe-themes
   (quote
    ("f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'base16-eighties-dark)
