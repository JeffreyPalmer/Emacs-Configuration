;;; A quick & ugly PATH solution to Emacs on Mac OSX
(when (string-equal "darwin" (symbol-name system-type))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings scpaste
                      clojure-mode clojure-test-mode
                      nrepl ac-nrepl
                      markdown-mode
                      yaml-mode
                      puppet-mode
                      marmalade scpaste
                      color-theme color-theme-solarized))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; use true OS X fullscreen
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;; Set some window defaults
(setq default-frame-alist
      '((width . 100) (height . 56)))
(menu-bar-mode)

;; Use the solarized theme
(color-theme-solarized-dark)

;; I *hate* this keybinding outside of the command line
(when window-system
  (if (eq (key-binding "\C-x\C-z") 'suspend-frame)
      (global-unset-key "\C-x\C-z"))
  (if (eq (key-binding "\C-z") 'suspend-frame)
      (global-unset-key "\C-z")))

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq delete-old-versions t)

;; ido mode support
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode)

;; don't EVER put tabs in indents
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; Allow auto-fill-mode in all text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; clojure support
;; nrepl
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'paredit-mode)

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
    echo area. Has no effect if the character before point is not of
    the syntax class ')'."
  (interactive)
  (let ((matching-text nil))
    ;; Only call `blink-matching-open' if the character before point
    ;; is a close parentheses type character. Otherwise, there's not
    ;; really any point, and `blink-matching-open' would just echo
    ;; "Mismatched parentheses", which gets really annoying.
    (if (char-equal (char-syntax (char-before (point))) ?\))
        (setq matching-text (blink-matching-open)))
    (if (not (null matching-text))
        (message matching-text))))

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-completing-map "\M-/" 'ac-stop) ; use M-/ to stop completion
;; ac-nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
