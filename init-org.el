;; org mode configuration is here because of its length
(use-package org
  :ensure org-plus-contrib
  :pin org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         ("<f12>" . org-agenda))
  :hook
  ((org-mode . (lambda () (variable-pitch-mode t)))
   (org-mode . visual-line-mode))
  :config
  (setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org"
        org-agenda-files (list org-directory)
        org-default-notes-file (concat org-directory "/inbox.org")
        org-clock-persist 'history
        org-enforce-todo-dependencies t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-insert-heading-respect-content t
        org-catch-invisible-edits 'show-and-error
        org-use-speed-commands t
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
                                 "* [[%:link][%:description]]\n%?"))
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
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "TODAY(y)" "WAITING(w@/!)" "|" "DONE(d!/!)")
                            (sequence "PROJECT(p)" "ACTIVE(a)" "|" "FINISHED(f!)" "CANCELLED(c@)")
                            (sequence "SOMEDAY(S!)" "MAYBE(m!)"))
        org-todo-keyword-faces '(("TODO" :foreground "red4")
                                 ("NEXT" :foreground "red3")
                                 ("TODAY" :foreground "red1")
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
            (tags-todo "/TODAY"
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
            (tags-todo "+active|+project/TODO"
                       ((org-agenda-overriding-header "Other Project Tasks")
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
  (org-clock-persistence-insinuate)
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; undefine C-c [ and C-c ]
               (org-defkey org-mode-map (kbd "C-c [") 'undefined)
               (org-defkey org-mode-map (kbd "C-c ]") 'undefined)
               ;; make sure that org-reveal is bound
               (org-defkey org-mode-map (kbd "C-c r") 'org-reveal))))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :custom (org-bullets-bullet-list '("◉" "○" "►" "•" "▸")))

(use-package org-checklist
  :ensure org-plus-contrib)

(use-package org-habit
  :ensure org-plus-contrib
  :config
  (setq org-habit-graph-column 65))

(use-package org-indent
  :ensure nil
  :diminish)

(use-package org-pomodoro
  :bind
  ("C-c p" . org-pomodoro)
  :config
  (setq alert-user-configuration '((((:category . "org-pomodoro")) osx-notifier nil))
        org-pomodoro-format "🍅~%s"))

;; track when projects are reviewed and set next review dates
(use-package org-review
  :bind
  (("C-c v" . org-review-insert-last-review)))

;; hide empty blocks in the agenda view
(defun org-agenda-delete-empty-blocks ()
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

(add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)
;; org mode font configuration
(custom-theme-set-faces
 'user
 '(variable-pitch            ((t (:family "Avenir Next" :slant normal :weight light :height 1.1 :width normal))))
 '(fixed-pitch               ((t (:family "Fira Code" :slant normal :weight normal :height 1.0 :width normal))))
 '(mode-line                 ((t (:box nil :weight normal :height 1.0 :family "Avenir Next"))))
 '(org-block                 ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link                  ((t (:inherit default))))
 '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value        ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table                 ((t (:inherit fixed-pitch))))
 '(org-tag                   ((t (:inherit shadow  :height 0.8))))
 '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
 `(org-level-8               ((t (:inherit default))))
 `(org-level-7               ((t (:inherit default))))
 `(org-level-6               ((t (:inherit default))))
 `(org-level-5               ((t (:inherit default))))
 `(org-level-4               ((t (:inherit default))))
 `(org-level-3               ((t (:inherit default :height 1.1))))
 `(org-level-2               ((t (:inherit default :weight normal :height 1.15))))
 `(org-level-1               ((t (:inherit default :weight normal :height 1.2))))
 `(org-document-title        ((t (:inherit default :weight normal :height 2.0 :underline nil)))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
