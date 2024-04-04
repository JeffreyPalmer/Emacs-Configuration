;;
;; DO NOT EDIT!
;;
;; This file was auto-generated from Emacs.org
;; Please edit that file and tangle it to generate both init.el and early-init.el
;;

(setq package-enable-at-startup nil)

;; Set the directory to look for native files

;; Set the eln-cache dir
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

(setenv "LSP_USE_PLISTS" "true")
