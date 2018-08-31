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
;; support for org-mode contributions
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Always install missing packages
(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)

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
    (setq mac-frame-tabbing nil)
    ;; default Latin font (e.g. Consolas)
    ;; default font size (point * 10)
    (set-face-attribute 'default nil
                        :family "Fira Code"
                        :height 141
                        :weight 'normal
                        :width 'normal)))

(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

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
(set-cursor-color "goldenrod")

;; enable pretty symbols mode
(global-prettify-symbols-mode 1)

;; other general settings
(setq apropos-do-all t
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

(use-package alert
  :config
  (setq alert-default-style 'osx-notifier))

;; load and configure any required packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  :bind
  (("C-c u" . auto-package-update-now)))

(use-package avy
  :bind
  (("C-;" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package company
  :diminish company-mode
  :config
  (global-company-mode))

(use-package discover-my-major
  :bind
  (("C-h C-m" . discover-my-major)
   ("C-h M-m" . discover-my-mode)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package eyebrowse
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c M-w"))
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

(use-package projectile
  :diminish projectile-mode)

(use-package fic-mode
  :diminish fic-mode
  :hook prog-mode-hook)

(use-package flycheck
  :pin melpa-stable
  :init
  (global-flycheck-mode))

(use-package highlight-parentheses)

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :hook prog-mode-hook)

;; Ivy/Counsel/Swiper Configuration
(use-package ivy
  :diminish ivy-mode
  :bind
  (:map ivy-mode-map ("C-'" . ivy-avy))
  :config
  (setq projectile-completion-system 'ivy
        ivy-use-virtual-buffers t
        ivy-height 13
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        ivy-virtual-abbreviate 'full ;; show the full virtual file paths
        ivy-extra-directories '("./")
        ivy-wrap t
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

(use-package counsel
  :bind*
  (("M-x" . counsel-M-x)
   ("C-c d d" . counsel-descbinds)
   ("C-c s s" . counsel-ag)
   ("C-c s d" . counsel-ag-projectile)
   ("C-x C-f" . counsel-find-file)
   ("C-x r f" . counsel-recentf)
   ("C-c g g" . counsel-git)
   ("C-c g G" . counsel-git-grep)
   ("C-x l" . counsel-locate)
   ("C-c g s" . counsel-grep-or-swiper)
   ("C-M-y" . counsel-yank-pop)
   ("C-c C-r" . ivy-resume)
   ("C-c i m" . counsel-imenu)
   ("C-c i M" . ivy-imenu-anywhere)
   ("C-c d s" . describe-symbol)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call))
  :config
  (progn
    (defun reloading (cmd)
      (lambda (x)
        (funcall cmd x)
        (ivy--reset-state ivy-last)))
    (defun given-file (cmd prompt)      ; needs lexical-binding
      (lambda (source)
        (let ((target
               (let ((enable-recursive-minibuffers t))
                 (read-file-name
                  (format "%s %s to:" prompt source)))))
          (funcall cmd source target 1))))
    (defun confirm-delete-file (x)
      (dired-delete-file x 'confirm-each-subdirectory))

    (ivy-add-actions
     'counsel-find-file
     `(("c" ,(given-file #'copy-file "Copy") "copy")
       ("d" ,(reloading #'confirm-delete-file) "delete")
       ("m" ,(reloading (given-file #'rename-file "Move")) "move")))

    (ivy-add-actions
     'counsel-projectile-find-file
     `(("c" ,(given-file #'copy-file "Copy") "copy")
       ("d" ,(reloading #'confirm-delete-file) "delete")
       ("m" ,(reloading (given-file #'rename-file "Move")) "move")
       ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))

    ;; to make counsel-ag search the root projectile directory.
    (defun counsel-ag-projectile ()
      (interactive)
      (counsel-ag nil (projectile-project-root)))

    (setq counsel-find-file-at-point t)

    ;; ignore . files or temporary files
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; File names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; File names ending with # or ~
           "\\|\\(?:\\`.+?[#~]\\'\\)"))))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package magit
  :bind
  ("C-c C-g" . magit-status)
  :config
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "10:00am"))

(use-package neotree
  :bind ("<f8>" . neotree-project-dir)
  :config
  (setq neo-smart-open t
        projectile-switch-project-action 'neotree-projectile-action)
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root.")))))

(use-package paredit
  :diminish paredit-mode
  :hook
  ((clojure-mode clojurescript-mode emacs-lisp-mode lisp-mode lisp-interaction-mode) . #'enable-paredit-mode))

(use-package restclient)

(use-package smex)

(use-package sql
  :config
  (progn
    ;; fix for underscores in postgres prompts
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (toggle-truncate-lines t)
                (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
                (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))
    ;; output each query before executing it
    (add-hook 'sql-login-hook
              (lambda ()
                (when (eq sql-product 'postgres)
                  (let ((proc (get-buffer-process (current-buffer))))
                    (comint-send-string proc "\\set ECHO queries\n")))))
    (sql-set-product "postgres")))

(use-package undo-tree
  :bind
  ("C-z" . undo)
  ("C-S-z" . undo-tree-redo)
  :config
  (setq undo-tree-history-directory-alist
        `(("." . (concat user-emacs-directory "backups")))
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode)
)

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(use-package yaml-mode)

(use-package whitespace
  :config
  (setq whitespace-style '(face trailing))
  (global-whitespace-mode '(clojure-mode elisp-mode markdown-mode org-mode)))

;; mode line customization
(use-package powerline)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme))

;; changes to generic programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (make-local-variable 'column-number-mode)
            (column-number-mode t)
            (when window-system (hl-line-mode t))))

;; clojure support
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'turn-on-eldoc-mode))

(use-package cider
  :pin melpa-stable
  :config
  (setq cider-repl-use-pretty-printing t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package clj-refactor
  :hook
  ((clojure-mode . (lambda ()
                     (clj-refactor-mode 1)
                     (yas-minor-mode 1)
                     (cljr-add-keybindings-with-prefix "C-c C-m")))))

;; ruby-specific changes
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook
    (lambda ()
    (set-fill-column 100)))
  :bind (([(meta down)] . ruby-forward-sexp)
         ([(meta up)]   . ruby-backward-sexp)
         (("C-c C-e"    . ruby-send-region))))

(use-package inf-ruby
  :hook ((ruby-mode . #'inf-ruby-minor-mode)))

(use-package smartparens
  :diminish smartparens-mode
  :hook
  ((ruby-mode-hook . #'smartparens-strict-mode))
  :config
  (require 'smartparens-config))

(use-package rubocop
  :diminish rubocop-mode
  :hook ruby-mode)

(use-package chruby)

(use-package robe
  :hook ruby-mode
  :config
  (advice-add 'inf-ruby-console-auto :before #'chruby-use-corresponding)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package rspec-mode
  :config
  (setq rspec-use-rake-when-possible t
        rspec-use-chruby t)
  (rspec-install-snippets)
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile))

(use-package bundler)

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
;; org mode support
;;
(load "~/.emacs.d/org-config.el")

;;
;; generic keybindings
;;
(progn
  ;; i use this constantly - probably a bug
  ;; (global-set-key (kbd "C-x g") 'rgrep)
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
(use-package color-theme-sanityinc-tomorrow)


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
(setq gc-cons-threshold best-gc-cons-threshold)
