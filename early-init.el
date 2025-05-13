;;
;; DO NOT EDIT!
;;
;; This file was auto-generated from Emacs.org
;; Please edit that file and tangle it to generate both init.el and early-init.el
;;

(setq package-enable-at-startup nil)

;; Manually set the path to avoid an issue with emacs-plus on Apple Silicon
;; This bug should be fixed soon and this could then be removed
(setenv "PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/jeff/.qlot/bin")
(setenv "CPATH" "/opt/homebrew/include")
(setenv "LIBRARY_PATH" "/opt/homebrew/lib")
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Set the directory to look for native files

;; Set the eln-cache dir
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

(add-to-list 'default-frame-alist '(undecorated-round . t))

(setenv "LSP_USE_PLISTS" "true")
