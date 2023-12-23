;;
;; DO NOT EDIT!
;;
;; This file was auto-generated from Emacs.org
;; Please edit that file and tangle it to generate both init.el and early-init.el
;;

(defvar jpalmer/default-font "Jetbrains Mono")
(defvar jpalmer/variable-font "Fira Sans")
(defvar jpalmer/default-font-size 120)
(defvar jpalmer/default-variable-font-size 150)

(setq user-full-name "Jeffrey Palmer"
      user-mail-address "jeffrey.palmer@acm.org")

;; The default is 800 kb. Measured in bytes
(setq gc-cons-threshold (* 50 1024 1024))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up use-package/straight integration
(straight-use-package '(use-package :type built-in))
(setq straight-use-package-by-default t)


;; Load the helper package for commands like `straight-x-clean-unused-repos`
;; I don't think that I need this anymore, actually
;; (require 'straight-x)

;; Silence native code compiler warnings, as they're pretty chatty
(setq comp-async-report-warnings-errors nil
      ;; This was generated when I asked emacs to disable the display of these compilation errors
      warning-suppress-types '((comp) (comp)))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;; For debugging purposes only
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "10:00am"))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(server-start)

(scroll-bar-mode -1)                    ; Disable the visible scrollbar
(tool-bar-mode -1)                      ; Disable the toolbar
(tooltip-mode -1)                       ; Disable tooltips
(set-fringe-mode 10)                    ; Give some breathing room
(menu-bar-mode -1)                      ; Disable the menu bar

(column-number-mode)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq inhibit-startup-message t
      visible-bell t
      fill-column 80
      kill-whole-line t
      require-final-newline t)

;; Don't make me type, I know what I'm doing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Name the frame
; (set-frame-parameter nil 'name "Main")

;; Set the default face
(set-face-attribute 'default nil :font jpalmer/default-font :height jpalmer/default-font-size :weight 'thin)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font jpalmer/default-font :height jpalmer/default-font-size :weight 'thin)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font jpalmer/variable-font :height jpalmer/default-variable-font-size :weight 'light)

;; Customize the global cursor color
(set-face-attribute 'cursor nil :background "goldenrod")

;; Enable ligatures in emacs-mac
(mac-auto-operator-composition-mode)

(use-package emacs
  :config
  (require-theme 'modus-themes)
  ;; Include any customization here
  (setq modus-themes-disable-other-themes t
        modus-themes-mode-line '(accented borderless (padding 4) (height 0.9))
        modus-themes-bold-constructs nil
        modus-themes-italic-constructs t
        modus-themes-fringes 'subtle
        ; modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)
        modus-themes-prompts '(bold)
        ; modus-themes-completions 'opinionated
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-org-blocks 'gray-background
        modus-themes-syntax '(faint)
        modus-themes-scale-headings t
        modus-themes-region '(bg-only)
        modus-themes-hl-line '(accented)
        modus-themes-headings
        '((1 . (regular 1.2))
          (2 . (regular 1.1))
          (3 . (regular 1.1))
          (t . (light 1.1)))
        modus-themes-org-agenda
        '((header-block . (variable-pitch 1.2 semibold))
          (header-date . (grayscale workaholic bold-today 1.1))
          (event . (accented italic varied))
          (scheduled . uniform)
          (habit . traffic-light))
        )

  (load-theme 'modus-vivendi t))

(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :config (setq idle-highlight-idle-time 0.5)
  :hook ((prog-mode text-mode) . idle-highlight-mode))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(when window-system
  (when (eq (key-binding (kbd "C-x C-z")) 'suspend-frame)
    (global-unset-key (kbd "C-x C-z")))
  (when (eq (key-binding (kbd "C-z")) 'suspend-frame)
    (global-unset-key (kbd "C-z")))
  (when (eq (key-binding (kbd "<C-tab>")) 'mac-next-tab-or-toggle-tab-bar)
    (global-unset-key (kbd "<C-tab>"))))

;; Keybindings for Mac Emacs
(global-set-key [(super a)] 'mark-whole-buffer)
(global-set-key [(super v)] 'yank)
(global-set-key [(super c)] 'kill-ring-save)
(global-set-key [(super s)] 'save-buffer)
(global-set-key [(super l)] 'goto-line)
(global-set-key [(super w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(super z)] 'undo)

(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package avy
  :bind (("C-;" . avy-goto-char-2)
         ("M-g M-g" . avy-goto-line))
  :config
  (avy-setup-default)
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

;; Try harder apropros
(setq-default apropos-do-all t)

;; If counsel is enabled
(use-package helpful
  ; :custom
  ; (counsel-describe-function-function #'helpful-callable)
  ; (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package corfu
    :init
    (global-corfu-mode)
    (corfu-popupinfo-mode)
    (setq corfu-auto t
          corfu-quit-no-match 'separator))

  (use-package emacs
    :init
    (setq completion-cycle-threshold 3
          tab-always-indent 'complete
))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-cycle t) )

;; Enable savehist to save search history over time
(use-package savehist
  :init
  (savehist-mode))

;; allows for substring search
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(defun jpalmer/consult-line-forward ()
  "Search for a matching line forward."
  (interactive)
  (consult-line))

(defun jpalmer/consult-line-backward ()
  "Search for a matching line backward."
  (interactive)
  (advice-add 'consult--line-candidates :filter-return 'reverse)
  (vertico-reverse-mode +1)
  (unwind-protect (consult-line)
    (vertico-reverse-mode -1)
    (advice-remove 'consult--line-candidates 'reverse)))

(with-eval-after-load 'consult
  (consult-customize jpalmer/consult-line-backward
                     :prompt "Go to line backward: ")
  (consult-customize jpalmer/consult-line-forward
                     :prompt "Go to line forward: "))

(global-set-key (kbd "C-s") 'jpalmer/consult-line-forward)
(global-set-key (kbd "C-r") 'jpalmer/consult-line-backward)

(use-package consult-flycheck
  :after (consult flycheck)
  :bind ("M-g f" . consult-flycheck))

  ;; Example configuration for Consult
(use-package consult
    ;; Replace bindings. Lazily loaded by `use-package'.
    :bind (;; C-c bindings in `mode-specific-map'
           ("C-c M-x" . consult-mode-command)
           ("C-c h" . consult-history)
           ("C-c k" . consult-kmacro)
           ("C-c m" . consult-man)
           ("C-c i" . consult-info)
           ([remap Info-search] . consult-info)
           ;; C-x bindings in `ctl-x-map'
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings in `goto-map'
           ("M-g e" . consult-compile-error)
           ;;("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ;("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings in `search-map'
           ("M-s d" . consult-find)                  ;; Alternative: consult-fd
           ("M-s c" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
    ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    ;;;; 4. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 5. No project support
    ;; (setq consult-project-function nil)
  )

(use-package marginalia
  :init
  (marginalia-mode))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))
  (persp-state-default-file (locate-user-emacs-file "var/.emacs.desktop"))
  :bind
  (("C-x k" . persp-kill-buffer*)
   ("C-x C-b" . persp-list-buffers))
  :hook (kill-emacs . persp-state-save)
  :init
  (persp-mode))

;; Customize consult to support perspective buffer restrictions
(with-eval-after-load 'consult
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

;; Also add support for creating new perspectives in projectile
(use-package persp-projectile
  :straight (:host github :repo "bbatsov/persp-projectile")
  :after (projectile perspective)
  :bind
  (:map projectile-command-map ("p" . projectile-persp-switch-project)))

(winner-mode 1)

;; This allows window navigation by pressing <Shift+Direction>
(windmove-default-keybindings)
(use-package ace-window
  :bind
  (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)
        aw-ignore-current t))

(setq display-buffer-base-action
    '(display-buffer-reuse-mode-window
      display-buffer-reuse-window
      display-buffer-same-window))
;; If a popup does happen, don't resize windows to be equally sized
(setq even-window-sizes nil)

;; Disabled for now in favor of the not-so-smart hungry delete
(use-package smart-hungry-delete
  :disabled
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package hungry-delete
  :init
  (setq hungry-delete-join-reluctantly t)
  :config
  (global-hungry-delete-mode))

(use-package whitespace
  :config
  (setq whitespace-style '(face trailing newline))
  ;; This should probably be enabled everywhere?
  (global-whitespace-mode))

(save-place-mode 1)

(define-key prog-mode-map (kbd "s-/") 'comment-line)

(use-package exec-path-from-shell
  :config
  ; (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

; (use-package highlight-parentheses)
;; Try this other option for now
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package highlight-indent-guides
  :config (setq highlight-indent-guides-method 'bitmap)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package paredit
  :diminish paredit-mode
  :hook
  ((clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode lisp-interaction-mode) . enable-paredit-mode))

(global-subword-mode 1)

(use-package hl-todo
  ;; (global-hl-todo-mode +1)
  ;; Only enable hl-todo-mode for programming buffers
  :hook (prog-mode . hl-todo-mode))

;; Also add consult-todo for nav support with consult
(use-package consult-todo
  :after consult
  :bind ("M-g t" . consult-todo))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Not sure yet why I originally had this disabled
  ; :diminish projectile-mode
  )

(use-package rainbow-mode
  :hook (org-mode emacs-lisp-mode web-mode typescript-mode js2-mode))

(use-package magit
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

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package fringe-helper)

(use-package git-gutter-fringe
  :after (git-gutter fringe-helper)
  :config
  (setq git-gutter-fr:side 'right-fringe))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :after which-key
  :commands lsp lsp-deferred
  :init
  (defun jpalmer/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook ( (typescript-ts-mode . lsp-deferred)
          (js2-ts-mode . lsp-deferred)
          (web-mode . lsp-deferred)
          (lsp-mode . lsp-enable-which-key-integration)
          (lsp-completion-mode . jpalmer/lsp-mode-setup-completion))
  ; :bind (:map lsp-mode-map ("TAB" . completion-at-point))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none)       ; we use Corfu!
  )

;; also install lsp-ui
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package typescript-ts-mode
  :custom
  ((typescript-ts-mode-indent-offset 4)))

;; FIXME: Put this back
(use-package glsl-mode
  :mode "(\\.\\(glsl\\|vert\\|frag\\|geom\\)\\'")

;; Add completion support for glsl
;(use-package company-glsl
;  :config
;  (when (executable-find "glslangValidator")
;    (add-to-list 'company-backends 'company-glsl)))

;; Add flycheck support for glsl
(use-package flycheck-glsl
  :after flycheck
  :straight (flycheck-glsl :type git :host github :repo "yrns/flycheck-glsl"
                           :fork (:host github :repo "JeffreyPalmer/flycheck-glsl"))
  :config (flycheck-glsl-setup))

;; try another package, as the first one requires some rework
;; (use-package flycheck-glsl
;;   :after flycheck
;;   :straight (flycheck-glsl :type git :host github :repo "Kaali/flycheck-glsl"))

;; Using the code directly
;; (with-eval-after-load 'flycheck
;;   (flycheck-define-checker jpalmer/glsl-lang-validator
;;     "A GLSL checker using glslangValidator.
;;   See URL https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"
;;     :command ("glslangValidator" source)
;;     :error-patterns
;;     ((error line-start "ERROR: " column ":" line ": " (message) line-end))
;;     :modes glsl-mode)

;;   (add-to-list 'flycheck-checkers 'jpalmer/glsl-lang-validator))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; Start the server with `httpd-start`
;; Use `impatient-mode` in any buffer
(use-package impatient-mode)

(use-package skewer-mode)

(use-package compile
  :custom
  (compilation-sroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

(use-package flycheck
  :defer t
  :custom
  ; (flycheck-highlighting-mode 'lines)
  ; (flycheck-highlighting-style 'level-face)
  (flycheck-indication-mode 'right-fringe)
  ;; FIXME: This will probably need to be fixed
  ; :hook (lsp-mode glsl-mode)
  :config (global-flycheck-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (defun jpalmer/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))
  (defun jpalmer/markdown-mode-hook ()
    (jpalmer/set-markdown-header-font-sizes))
  (add-hook 'markdown-mode-hook 'jpalmer/markdown-mode-hook))

(use-package org
  ;; :ensure org-contrib
  ;; :pin gnu
  :straight (:type built-in)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         ("<f12>" . org-agenda))
  :hook
  ((org-mode . (lambda () (variable-pitch-mode t)))
   (org-mode . visual-line-mode)
   (org-mode . (lambda ()
                 ;; undefine C-c [ and C-c ]
                 (org-defkey org-mode-map (kbd "C-c [") 'undefined)
                 (org-defkey org-mode-map (kbd "C-c ]") 'undefined)
                 ;; make sure that org-reveal is bound
                 (org-defkey org-mode-map (kbd "C-c r") 'org-reveal))))
  :config
  (setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org"
        org-agenda-files (list org-directory)
        org-agenda-start-day nil
        org-default-notes-file (concat org-directory "/inbox.org")
        org-clock-persist 'history
        org-enforce-todo-dependencies t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-insert-heading-respect-content t
        org-catch-invisible-edits 'show-and-error
        org-use-speed-commands t
        ;; don't reorganize windows when opening the agenda
        org-agenda-window-setup 'current-window
        ;; open org links in the same window
        org-link-frame-setup '((file . find-file))
        ;; calculate completion statistics for multi-level projects
        org-hierarchical-todo-statistics nil
        ;; org-agenda-hide-tags-regexp TODO - figure out what this should be
        ;; don't show scheduled TODO items
        org-agenda-todo-ignore-scheduled 'future
        ;; logging work
        org-log-done 'time
        org-log-into-drawer "LOGBOOK"
        ;; capture settings
        org-capture-templates '(("t" "To Do" entry (file "")
                                 "* TODO %?\n")
                                ("g" "Generic" entry (file "")
                                 "* %?\n")
                                ("j" "Journal Entry"
                                 entry (file+olp+datetree "journal.org")
                                 "* %?")
                                ("l" "A link, for reading later." entry (file "")
                                 "* [[%:link][%:description]]%?"))
        ;; refile settings
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-log-note-headings '((done        . "CLOSING NOTE %t")
                                (note        . "Note taken on %t")
                                (state       . "State %-12s from %-12S %t")
                                (reschedule  . "Rescheduled from %S on %t")
                                (delschedule . "Not scheduled, was %S on %t")
                                (redeadline  . "New deadline from %S on %t")
                                (deldeadline . "Removed deadline, was %S on %t"))
        org-startup-indented t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "TODAY(y)" "IN_PROGRESS(i)" "WAITING(w@/!)" "|" "DONE(d!/!)")
                            (sequence "PROJECT(p)" "ACTIVE(a)" "|" "FINISHED(f!)" "CANCELLED(c@)")
                            (sequence "SOMEDAY(S!)" "MAYBE(m!)"))
        org-todo-keyword-faces '(("TODO" :foreground "DodgerBlue3")
                                 ("NEXT" :foreground "DodgerBlue2")
                                 ("TODAY" :foreground "SpringGreen2")
                                 ("IN_PROGRESS" :foreground "SpringGreen2")
                                 ("DONE" :foreground "forest green")
                                 ("PROJECT" :foreground "cornflower blue")
                                 ("ACTIVE" :foreground "deep sky blue")
                                 ("FINISHED" :foreground "forest green")
                                 ("CANCELLED" :foreground "goldenrod")
                                 ("WAITING" :foreground "coral")
                                 ("SOMEDAY" :foreground "purple")
                                 ("MAYBE" :foreground "purple"))
        org-todo-state-tags-triggers '(("PROJECT" ("project" . t) ("active" . nil))
                                       ("" ("project" . nil) ("active" . nil))
                                       ("ACTIVE" ("active" . t))
                                       ("FINISHED" ("active" . nil))
                                       ("SOMEDAY" ("active" . nil))
                                       ("MAYBE" ("active" . nil)))
        ;; agenda customization
        org-agenda-span 'day
        org-stuck-projects '("/PROJECT|ACTIVE" ("NEXT" "TODAY") nil "")
        org-agenda-compact-blocks nil
        org-agenda-block-separator ?\-
        org-agenda-dim-blocked-tasks nil
        org-agenda-custom-commands
        '(
          ;; a view that supports:
          ;; - most important task of the day
          ;; - secondary tasks
          ;; - other tasks if i have time
          ("d" "Daily View"
           ((agenda "" nil)
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")))
            (tags-todo "/TODAY|IN_PROGRESS"
                       ((org-agenda-overriding-header "Most Important Tasks for Today")))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")))
            (tags-todo "active/NEXT"
                       ((org-agenda-overriding-header "Active Project Next Tasks")
                        (org-agenda-sorting-strategy '(todo-state-down category-keep))))
            (tags "REFILE"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-active+project/NEXT"
                       ((org-agenda-overriding-header "Other Project Next Tasks")
                        (org-agenda-sorting-strategy '(todo-state-down category-keep))))
            (tags-todo "+active/TODO"
                       ((org-agenda-overriding-header "Active Project Tasks")
                        (org-agenda-sorting-strategy '(todo-state-down category-keep))))))
          ("D" "Review completed tasks"
           ((tags-todo "/DONE"
                       ((org-agenda-overriding-header "Completed Tasks and Projects")))))
          ("n" "Non-Project Tasks"
           ((tags-todo "-project-active/!TODO|NEXT|TODAY"
                       ((org-agenda-overriding-header "Non-Project Tasks")))))
          ("p" "Project Review"
           ((tags-todo "/PROJECT|ACTIVE"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo '("NEXT" "TODAY")))))
            (tags-todo "/ACTIVE"
                       ((org-agenda-overriding-header "Active Projects")
                        (org-agenda-skip-function '(org-agenda-skip-subtree-if 'nottodo '("NEXT" "TODAY")))))
            (tags-todo "/PROJECT"
                       ((org-agenda-overriding-header "Other Projects")
                        (org-agenda-skip-function '(org-agenda-skip-subtree-if 'nottodo '("NEXT" "TODAY")))))
            (tags-todo "-CANCELLED/"
                       ((org-agenda-overriding-header "Reviews Scheduled")
                        (org-agenda-skip-function 'org-review-agenda-skip)
                        (org-agenda-cmp-user-defined 'org-review-compare)
                        (org-agenda-sorting-strategy '(user-defined-down))))))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          ("i" "Inbox" tags "REFILE"
           ((org-agenda-overriding-header "Inbox")
            (org-tags-match-list-sublevels nil)))))
  (org-clock-persistence-insinuate))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun jpalmer/org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.
     A block is identified as empty if there are fewer than 2
     non-empty lines in the block (excluding the line with
     `org-agenda-block-separator' characters)."
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char (point-min))
      (let* ((blank-line-re "^\\s-*$")
             (content-line-count (if (looking-at-p blank-line-re) 0 1))
             (start-pos (point))
             (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
        (while (and (not (eobp)) (forward-line))
          (cond
           ((looking-at-p block-re)
            (when (< content-line-count 2)
              (delete-region start-pos (1+ (point-at-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        ;; The above strategy can leave a separator line at the beginning
        ;; of the buffer.
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (point-at-eol))))))
    (setq buffer-read-only t))
(add-hook 'org-agenda-finalize-hook #'jpalmer/org-agenda-delete-empty-blocks)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/OrgRoam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . org-roam-refile)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

;; Install any required org-contrib libraries
(use-package org-contrib
  :config
  (require 'org-checklist))

(use-package org-review
  :bind
  (("C-c v" . org-review-insert-last-review)))

;; FIXME: This is disabled for now
;; Add support for pomodoro time tracking
(use-package org-pomodoro
  :bind
  ("s-p" . org-pomodoro)
  :config
  (setq alert-user-configuration '((((:category . "org-pomodoro")) osx-notifier nil))
        org-pomodoro-format "🍅~%s"))

;; TODO: Enable this once org mode is fully set up
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(defun jpalmer/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "./Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jpalmer/org-babel-tangle-config)))

(use-package neotree
  :bind ("<f8>" . neotree-project-dir)
  :hook
  (neotree-mode . (lambda ()
                    (variable-pitch-mode t)))
  :config
  (setq neo-smart-open t
        projectile-switch-project-action 'neotree-projectile-action
        neo-theme 'icons
        neo-window-width 35)
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

(setq gc-cons-threshold (* 2 1024 1024))
