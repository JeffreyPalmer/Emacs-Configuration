;;
;; Emacs Configuration
;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; make sure we have access to melpa-stable
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Always install missing packages
(setq use-package-always-ensure t)

;; generic setup
;; i *hate* these keybindings outside of the command line
(when window-system
  (when (eq (key-binding (kbd "s-m")) 'iconify-frame)
    (global-unset-key (kbd "s-m")))
  (when (eq (key-binding (kbd "C-x C-z")) 'suspend-frame)
    (global-unset-key (kbd "C-x C-z")))
  (when (eq (key-binding (kbd "C-z")) 'suspend-frame)
    (global-unset-key (kbd "C-z")))
  ;; default fonts
  (when (eq system-type 'darwin)
    ;; default Latin font (e.g. Consolas)
    ;; default font size (point * 10)
    (set-face-attribute 'default nil
                        :family "Fira Code"
                        :height 141
                        :weight 'normal
                        :width 'normal)))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; enable winner mode for restoring window configurations
(winner-mode 1)

;; Generic user configuration
(setq user-full-name "Jeffrey Palmer"
      user-mail-address "jeffrey.palmer@acm.org")


;; don't make me type, i know what i'm doing
(defalias 'yes-or-no-p 'y-or-n-p)

;; only use visual-line-mode in text files
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; other general settings
(setq apropos-do-all t
      cursor-color "#708183"
      default-tab-width 4
      fci-rule-color "#e9e2cb"
      fill-column 80
      inhibit-startup-screen t
      kill-whole-line t
      linum-format " %7i "
      require-final-newline t
      ring-bell-function 'ignore
      visible-bell nil)

;; Make sure that we start with sane defaults
(use-package better-defaults)

;; elixir support
(use-package alchemist)

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
(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)))
(use-package projectile
  :diminish projectile-mode
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
  :diminish idle-highlight-mode
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (idle-highlight-mode t))))
(use-package highlight-parentheses)
(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl))
(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "10:00am"))
(use-package paredit
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package pretty-lambdada
  :config (pretty-lambda-for-modes))
(use-package restclient)
(use-package smex)
(use-package undo-tree
  :bind
  ("C-z" . undo)
  ("C-S-z" . undo-tree-redo)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist
        `(("." . (concat user-emacs-directory "backups"))))
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))
(use-package yaml-mode)
(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

;; mode line customization
(use-package smart-mode-line)
(use-package smart-mode-line-powerline-theme)
(use-package powerline)

;; changes to generic programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (make-local-variable 'column-number-mode)
            (column-number-mode t)
            (when window-system (hl-line-mode t))))

;; clojure support
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojurescript-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'turn-on-eldoc-mode))

(use-package cider
  :pin melpa-stable
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; ruby-specific changes
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (set-fill-column 100))))

;; Fira Code Ligature Support
(mac-auto-operator-composition-mode)

;; sort helpers for words and symbols
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

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

;; keep those custom variables out of here!
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; configure themes at the end to make sure we avoid the safe themes warning
(load-theme 'base16-eighties)
(setq sml/theme 'powerline)
(sml/setup)

;;; EXPERIMENTAL
;; Keybindings for Mac Emacs
(global-set-key [(super a)] 'mark-whole-buffer)
(global-set-key [(super v)] 'yank)
(global-set-key [(super c)] 'kill-ring-save)
(global-set-key [(super s)] 'save-buffer)
(global-set-key [(super l)] 'goto-line)
(global-set-key [(super w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(super z)] 'undo)

;; make sure modifier keybindings are sane
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
