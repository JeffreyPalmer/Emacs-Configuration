;;
;; DO NOT EDIT!
;;
;; This file was auto-generated from Emacs.org
;; Please edit that file and tangle it to generate both init.el and early-init.el
;;

(defvar jpalmer/default-font "Jetbrains Mono")
;; (defvar jpalmer/variable-font "Avenir Next")
(defvar jpalmer/variable-font "Optima")
(defvar jpalmer/default-font-size 140)
(defvar jpalmer/default-variable-font-size 190)
(defvar jpalmer/is-emacs-mac t)

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
(use-package emacs
  :custom
  (native-comp-async-report-warnings-errors 'silent))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :config
  (no-littering-theme-backups))

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

(unless (and (fboundp 'server-running-p)
         (server-running-p))
  (server-start))

(scroll-bar-mode -1)                    ; Disable the visible scrollbar
(tool-bar-mode -1)                      ; Disable the toolbar
(tooltip-mode -1)                       ; Disable tooltips
(set-fringe-mode 10)                    ; Give some breathing room
(menu-bar-mode (if jpalmer/is-emacs-mac -1 1)) ; Disable the menu bar if on emacs-mac, enable it otherwise

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
(set-face-attribute 'default nil :font jpalmer/default-font :height jpalmer/default-font-size :weight 'light)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font jpalmer/default-font :height jpalmer/default-font-size :weight 'light)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font jpalmer/variable-font :height jpalmer/default-variable-font-size :weight 'regular)

;; Customize the global cursor color
(set-face-attribute 'cursor nil :background "goldenrod")

;; Enable ligatures in emacs-mac
(if jpalmer/is-emacs-mac
    ;; If we're on emacs-mac, use the built-in ligature support
    (mac-auto-operator-composition-mode)

  ;; Otherwise use the ligatures.el package
  (use-package ligature
    :config
    ;; Enable all JetBrains Mono ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                         "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                         "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                         "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                         "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                         "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                         "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                         "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                         "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                         "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                         "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                         ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                         "<:<" ";;;"))

    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    ;; (global-ligature-mode t)
    :hook
    (prog-mode . ligature-mode)))

;; Show lambda as a symbol
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-alist '(("lambda" . ?λ)))
            (prettify-symbols-mode 1)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic t
        doom-themes-padded-modeline t) ; Adds a 4 pixel margin around the modeline
  ; My previous theme
  ; (load-theme 'doom-dark+ t)
  (load-theme 'doom-oceanic-next t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :config (setq idle-highlight-idle-time 0.5)
  :hook ((prog-mode text-mode) . idle-highlight-mode))

(use-package all-the-icons)
(use-package nerd-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-buffer-encoding nil)
           (doom-modeline-buffer-file-name-style 'relative-from-project)))

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
  :straight (:type built-in)
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package avy
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  (avy-orders-alist '((avy-goto-char-2 . avy-order-closest)
                      (avy-goto-line . avy-order-closest)))
  :bind (("s-;" . avy-goto-char-2)
         ("s-g" . avy-goto-line))
  :config
  (avy-setup-default))

(use-package casual-avy
  :bind ("C-M-g" . casual-avy-tmenu))

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

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  (vertico-multiform-mode))

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
  (consult-line
   ;; Uncomment this line to enable search to start with the symbol at
   ;; the current point
   ;;
   ;; (thing-at-point 'symbol)
   ))

(defun jpalmer/consult-line-backward ()
  "Search for a matching line backward."
  (interactive)
  (advice-add 'consult--line-candidates :filter-return 'reverse)
  (vertico-reverse-mode +1)
  (unwind-protect (consult-line
                   ;; Uncomment this line to enable search to start
                   ;; with the symbol at the current point
                   ;;
                   ;; (thing-at-point 'symbol)
                   )
    (vertico-reverse-mode -1)
    (advice-remove 'consult--line-candidates 'reverse)))

(with-eval-after-load 'consult
  (consult-customize consult-line
                     :initial (thing-at-point 'symbol))
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
         ;;("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;;("M-g o" . consult-org-heading)               ;; Alternative: consult-outline
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

(use-package corfu
  :custom
  (corfu-cycle t)
  ;(corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt)
  ;; Try disabling return-based completion
  ;;:bind (:map corfu-map
  ;;            ("RET" . nil))
  ;; enable tab-and-go completion
  ;; See https://github.com/minad/corfu#tab-and-go-completion
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; Add support for next-icons in completions
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package emacs
  :init
  (setq completion-cycle-threshold t
        tab-always-indent 'complete))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

   :init
   (setq prefix-help-command #'embark-prefix-help-command)

   :config
   (add-to-list 'display-buffer-alist
                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                  nil
                  (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  (("M-o" . ace-window)
   ("s-o" . other-window))
  :config
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)
        aw-ignore-current t))

(use-package shackle
  :custom
  (shackle-default-rule '(:select t))
  (shackle-rules '(("\\*sly-mrepl" :regexp t :align t :size 0.2 :select t)
                   ("\\*sly-compilation" :regexp t :align 'below :size 0.3)
                   ("\\*sly-db" :regexp t :align 'right :size 0.4)
                   ("\\*julia\\*" :regexp t :align 'below :size 0.2 :select t)))
  :config
  (shackle-mode))

(use-package popper
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode
                              compilation-mode
                              messages-mode
                              occur-mode
                              "\\*helpful"
                              "\\*sly-mrepl"
                              "\\*julia\\*"))
  (popper-group-function #'popper-group-by-perspective)
  (popper-display-control nil)
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

;; Disabled for now in favor of the not-so-smart hungry delete
(use-package smart-hungry-delete
  :disabled t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package hungry-delete
  ;; This will leave a space between the previous text and the following text
  ;; (setq hungry-delete-join-reluctantly t)
  :config
  (global-hungry-delete-mode))

(use-package whitespace
  :config
  (setq whitespace-style '(face trailing newline))
  ;; This should probably be enabled everywhere?
  (global-whitespace-mode))

(save-place-mode 1)

(define-key prog-mode-map (kbd "s-/") 'comment-line)

(use-package move-text
  :config
  (move-text-default-bindings))

(when jpalmer/is-emacs-mac
  (use-package exec-path-from-shell
    :custom
    (exec-path-from-shell-arguments nil)
    :config
    ;; (setq exec-path-from-shell-arguments nil)
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "CPATH")
      (exec-path-from-shell-copy-env "LIBRARY_PATH"))))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

;; Enable global electric-pair mode
(use-package emacs
 :custom
 (electric-pair-preserve-balance nil)
 :config
 (electric-pair-mode))

(use-package highlight-parentheses
  :custom
  (highlight-parentheses-highlight-adjacent t)
  ;; Custom level colors
  (highlight-parentheses-colors
   '(
     "dodger blue"
     "lime green"
     "dark orchid"
     "deep pink"
     "orange"
     "light sky blue"
     "light green"
     "gold"
     "magenta"))
  :config (global-highlight-parentheses-mode))

;; Try this other option for now
(use-package paren
  :custom
  (show-paren-delay 0)
  :config
  ;(set-face-attribute 'show-paren-match-expression nil :background "#363e4a" :weight 'extra-bold)
  ; Disable this as rainbow delimiters doesn't require it
  (show-paren-mode 0))

(use-package paredit
  ;:diminish paredit-mode
  :hook
  ((clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode lisp-interaction-mode) . enable-paredit-mode)
  :config
  (setq backward-delete-char-untabify-method 'all))

(use-package highlight-indent-guides
  :custom
  ;; See if these are necessary with my new theme
  (highlight-indent-guides-auto-character-face-perc 20)
  (highlight-indent-guides-auto-top-character-face-perc 100)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))

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

(use-package rg
  :init
  (rg-enable-default-bindings))

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode)
  (add-to-list 'global-colorful-modes 'vterm-mode))

(use-package vterm
  :custom
  (vterm-kill-buffer-on-exit nil))

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration))

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
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
    :after which-key
    :commands lsp lsp-deferred
    :custom
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-completion-provider :none)       ; we use Corfu!
    (lsp-enable-snippet nil)
    :init
    ;; Improve IO performance for LSP, from the documentation here:
    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
    (setq read-process-output-max (* 1024 1024)) ; 1mb
    (defun jpalmer/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    :hook (
           ;; Don't automatically enable lsp for all languages?
           ;; (prog-mode . lsp-deferred)
           ;; (web-mode . lsp-deferred)
           (lsp-mode . lsp-enable-which-key-integration)
           (lsp-completion-mode . jpalmer/lsp-mode-setup-completion))
    ; :bind (:map lsp-mode-map ("TAB" . completion-at-point))
    )

  ;; also install lsp-ui
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; LSP UI SIDELINE settings
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-lens-enable t)
  ;; LSP UI DOC settings
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-side 'right)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-show-with-cursor t)
  ;; LSP UI PEEK settings
  (lsp-ui-peek-enable t)
  :config
  (lsp-ui-doc-show))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . lsp-deferred)
  :custom
  (typescript-ts-mode-indent-offset 4))

;; Trying the current version to see if there are still problems in 2025
;; Work around an error in the current version of the typescript treesitter grammar
(defvar jpalmer/tsx-treesit-auto-recipe
  (make-treesit-auto-recipe
   :lang 'tsx
   :ts-mode 'tsx-ts-mode
   :remap 'typescript-tsx-mode
   :url "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "v0.20.3"
   :source-dir "tsx/src"
   :ext "\\.tsx\\'")
  "Recipe for treesitter tsx lib")
;; (add-to-list 'treesit-auto-recipe-list jpalmer/tsx-treesit-auto-recipe)
(defvar jpalmer/typescript-treesit-auto-recipe
  (make-treesit-auto-recipe
   :lang 'typescript
   :ts-mode 'typescript-ts-mode
   :remap 'typescript-mode
   :url "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "v0.20.3"
   :source-dir "typescript/src"
   :ext "\\.ts\\'")
  "Recipe for treesitter typescript lib")
;; (add-to-list 'treesit-auto-recipe-list jpalmer/typescript-treesit-auto-recipe)

(use-package lsp-julia
  :custom
  (lsp-julia-package-dir nil)
  (lsp-julia-default-environment "~/.julia/environments/v1.10"))

(use-package julia-mode
  :hook (julia-mode . lsp-deferred))

;; REPL Support
(use-package julia-repl
  :after vterm
  :hook (julia-mode . julia-repl-mode)
  :config (julia-repl-set-terminal-backend 'vterm))

;; (use-package rust-mode
;;   :init
;;  (setq rust-mode-treesitter-derive nil))

(use-package rustic
  :after rust-mode
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package sly
  ;:custom (inferior-lisp-program "sbcl")
  ;; Configure SLY to support running with QLOT
  :config
  (setq sly-lisp-implementations
        '((qlot ("qlot" "exec" "sbcl" "--dynamic-space-size" "4096") :coding-system utf-8-unix)
          (sbcl ("sbcl" "--dynamic-space-size" "4096") :coding-system utf-8-unix))))

(use-package sly-asdf
  :config (push 'sly-asdf sly-contribs))
;;(use-package sly-quicklisp
;;  :config (push 'sly-quicklisp sly-contribs))
;;(use-package sly-overlay)
(use-package sly-repl-ansi-color
  :config (push 'sly-repl-ansi-color sly-contribs))

(use-package info
  :config
  (info-initialize)
  (push "/Users/jeff/src/personal/ansicl.info/" Info-directory-list))

(use-package info-look
  :config
  (add-to-list 'Info-default-directory-list "/Users/jeff/src/personal/ansicl.info/")
  (info-lookup-add-help
   :mode 'lisp-mode
   :regexp "[^][()'\" \t\n]+"
   :ignore-case t
   :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))

;; Install the base clojure mode
(use-package clojure-mode)

;; Also include CIDER
(use-package cider)

(use-package glsl-mode)

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

(use-package wgsl-ts-mode
  :straight (:host github :repo "acowley/wgsl-ts-mode")
  :hook (wgsl-ts-mode . lsp-deferred)
  :mode "\\.wgsl\\'")

;; Support for WGSL grammar
(defvar jpalmer/wgsl-treesit-auto-recipe
  (make-treesit-auto-recipe
   :lang 'wgsl
   :ts-mode 'wgsl-ts-mode
   :remap '(wgsl-mode)
   :url "https://github.com/szebniok/tree-sitter-wgsl"
   :revision "master"
   :source-dir "src"
   :ext "\\.wgsl\\'"))
(add-to-list 'treesit-auto-recipe-list jpalmer/wgsl-treesit-auto-recipe)

;; Try to fix lsp mode's support for wgsl-ts-mode
(add-to-list 'lsp-language-id-configuration '(wgsl-ts-mode . "wgsl"))

(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.svelte\\'"
  :hook (web-mode . lsp-deferred)
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;;
;; These two packages don't really seem necessary, so I'm taking them out for now
;;

;; Start the server with `httpd-start`
;; Use `impatient-mode` in any buffer
;; (use-package impatient-mode)

;; (use-package skewer-mode)

(use-package compile
  :custom
  (compilation-scroll-output t))

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

(use-package org
  ;; :ensure org-contrib
  ;; :pin gnu
  ;; :straight (:type built-in)
  :straight (:type git :host github :repo "emacs-straight/org-mode" :branch "bugfix")
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
                                 "* %:annotation\n%U\n%:i"))
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
        org-startup-folded 'show2levels
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "TODAY(y)" "IN_PROGRESS(i)" "WAITING(w@/!)" "|" "DONE(d!/!)")
                            (sequence "PROJECT(p)" "ACTIVE(a)" "|" "FINISHED(f!)" "CANCELLED(c@)")
                            (sequence "SOMEDAY(S!)" "MAYBE(m!)"))
        org-todo-keyword-faces '(("TODO" . (:foreground "DodgerBlue3"))
                                 ("NEXT" . (:foreground "DodgerBlue2"))
                                 ("TODAY" . (:foreground "lime green"))
                                 ("IN_PROGRESS" . (:foreground "lime green"))
                                 ("DONE" . (:foreground "forest green"))
                                 ("PROJECT" . (:foreground "cornflower blue"))
                                 ("ACTIVE" . (:foreground "deep sky blue"))
                                 ("FINISHED" . (:foreground "forest green"))
                                 ("CANCELLED" . (:foreground "goldenrod"))
                                 ("WAITING" . (:foreground "tomato"))
                                 ("SOMEDAY" . (:foreground "purple"))
                                 ("MAYBE" . (:foreground "purple")))
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

(require 'org-protocol)

(with-eval-after-load 'org-faces
  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font jpalmer/variable-font :weight 'regular :height 1.3)
  (dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font jpalmer/variable-font :weight 'regular :height (cdr face)))

  ;; Make sure org-indent face is available
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ; (set-face-attribute 'org-link nil   :weight 'regular :inherit 'variable-pitch)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background 'unspecified)
  (set-face-attribute 'org-column-title nil :background 'unspecified))

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

(use-package org-ql
  :straight (:host github
             :repo "alphapapa/org-ql"
             :files (:defaults (:exclude "helm-org-ql.el")))
  :bind ("M-g o" . org-ql-find-in-agenda))

;; Now add support for org-file searching using org-ql-find into consult

(use-package org-chef)

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

(use-package auctex)

(use-package writeroom-mode)

(use-package emacs
  :custom
  (text-mode-ispell-word-completion t))

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

(defun gjg/gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup using Pandoc.
   Remove the property drawers Pandoc insists on inserting for org output."
      (interactive)
      (let* ((org-prefix (alist-get 'org-mode gptel-prompt-prefix-alist))
             (shift-indent (progn (string-match "^\\(\\*+\\)" org-prefix) (length (match-string 1 org-prefix))))
             (lua-filter (when (file-readable-p "~/.config/pandoc/gfm_code_to_org_block.lua")
                           (concat "--lua-filter=" (expand-file-name "~/.config/pandoc/gfm_code_to_org_block.lua"))))
             (temp-name (make-temp-name "gptel-convert-" ))
             (sentence-end "\\([.?!
      ]\\)"))
        (with-current-buffer (get-buffer-create (concat "*" temp-name "*"))
          (insert str)
          (write-region (point-min) (point-max) (concat "/tmp/" temp-name ".md" ))
          (shell-command-on-region (point-min) (point-max)
                                   (format "pandoc -f gfm -t org --shift-heading-level-by=%d %s|sed '/:PROPERTIES:/,/:END:/d'" shift-indent lua-filter)
                                   nil ;; use current buffer
                                   t   ;; replace the buffer contents
                                   "*gptel-convert-error*")
          (goto-char (point-max))
          (buffer-string))))

(defun gjg/gptel-convert-org-with-pandoc (content buffer)
        "Transform CONTENT acoording to required major-mode using `pandoc'.
         Currenly only `org-mode' is supported
         This depends on the `pandoc' binary only, not on the  Emacs Lisp `pandoc' package."
        (pcase (buffer-local-value 'major-mode buffer)
          ('org-mode (gjg/gptel--convert-markdown->org content))
          (_ content)))

(use-package gptel
   :custom
   (gptel-backend (gptel-make-openai "koboldcpp"
                      :stream t
                      :protocol "http"
                      :host "10.0.1.145:5000"
                      :models '("local-llm")))
   (gptel-default-mode 'org-mode)
   (gptel-model "local-llm")
   (gptel-post-stream-hook 'gptel-auto-scroll)
   (gptel-post-response-hook 'gptel-end-of-response)
   ;; Disable this for now
   ; (gptel-response-filter-functions '(gjg/gptel-convert-org-with-pandoc))
   :config
   (setq gptel-expert-commands t))

(setq gc-cons-threshold (* 2 1024 1024))
