(setq package-enable-at-startup nil)

;; Set the directory to look for native files

;; Set the eln-cache dir
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))
