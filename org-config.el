;; org mode configuration is here because of its length
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         ("<f12>" . org-agenda))
  :config
  (setq org-directory "~/Dropbox/org"
        org-agenda-files (list org-directory)
        org-default-notes-file (concat org-directory "/inbox.org")
        org-clock-persist 'history
        org-enforce-todo-dependencies t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-insert-heading-respect-content t
        org-catch-invisible-edits 'smart
        ;; don't show scheduled TODO items
        org-agenda-todo-ignore-scheduled 'future
        ;; logging work
        org-log-done 'time
        org-log-into-drawer "LOGBOOK"
        org-log-redeadline 'time
        org-log-refile 'time
        org-log-reschedule 'time
        org-habit-graph-column 65
        ;; capture settings
        org-capture-templates '(("t" "To Do" entry (file "")
                                 "* TODO %?\n" :clock-in t :clock-resume t)
                                ("g" "Generic" entry (file "")
                                 "* %?\n" :clock-in t :clock-resume t)
                                ("j" "Journal Entry"
                                 entry (file+olp+datetree "journal.org")
                                 "* %?")
                                ("l" "A link, for reading later." entry (file "")
                                 "* [[%:link][%:description]]\n%u\n%?"))
        ;; refile settings
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-log-note-headings '((done        . "CLOSING NOTE %t")
                                (state       . "State %-12s from %-12S %t")
                                (note        . "Note taken on %t")
                                (reschedule  . "Rescheduled from %S on %t")
                                (delschedule . "Not scheduled, was %S on %t")
                                (redeadline  . "New deadline from %S on %t")
                                (deldeadline . "Removed deadline, was %S on %t")
                                (refile      . "Refiled from %s to %S on %t"))
        org-startup-indented t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "TODAY(y!)" "|" "DONE(d!/!)")
                            (sequence "PROJECT(p)" "ACTIVE(a!)" "|" "FINISHED(f!)" "CANCELLED(c@)")
                            (sequence "SOMEDAY(S!)" "MAYBE(m!)")
                            (sequence "WAITING(w@/!)"))
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
        org-stuck-projects (quote ("" nil nil ""))
        org-agenda-compact-blocks nil
        org-agenda-block-separator ?\-
        org-agenda-custom-commands
        '(
          ;; a view that supports:
          ;; - most important task of the day
          ;; - secondary tasks
          ;; - other tasks if i have time
          ("d" "Daily View"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "/TODAY"
                  ((org-agenda-overriding-header "Most Important Tasks for Today")))
            (tags-todo "active/!TODO|NEXT"
                       ((org-agenda-overriding-header "Active Project Tasks")
                        (org-agenda-sorting-strategy '(todo-state-down category-keep))))
            (tags-todo "-active+project/!NEXT|TODO"
                       ((org-agenda-overriding-header "Other Project Tasks")
                        (org-agenda-sorting-strategy '(todo-state-down category-keep))))
            (tags-todo "/WAITING"
                  ((org-agenda-overriding-header "Waiting")))))
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
                   (org-agenda-skip-function '(org-agenda-skip-subtree-if 'nottodo '("NEXT" "TODAY")))))))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          (" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-CANCELLED/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-HOLD-CANCELLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'bh/skip-non-projects)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED/!NEXT"
                       ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header (concat "Project Subtasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-non-project-tasks)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-project-tasks)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED+WAITING|HOLD/!"
                       ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-non-tasks)
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))
           nil)))
  (org-clock-persistence-insinuate)
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; undefine C-c [ and C-c ]
               (org-defkey org-mode-map (kbd "C-c [") 'undefined)
               (org-defkey org-mode-map (kbd "C-c ]") 'undefined))))

(use-package org-bullets
  :hook ((org-mode . (lambda () (org-bullets-mode 1)))))

(use-package org-checklist
  :ensure org-plus-contrib)

(use-package org-habit
  :ensure org-plus-contrib)

(use-package org-indent
  :ensure nil
  :diminish)

(use-package org-pomodoro
  :commands (org-pomodoro)
  :bind
  ("C-c p" . org-pomodoro)
  :config
  (setq alert-user-configuration '((((:category . "org-pomodoro")) osx-notifier nil))
        org-pomodoro-format "🍅~%s"))

;;
;; org-mode support functions
;;
(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(defun bh/is-project-p ()
  "Any task with a todo keyword and subtasks"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

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


;; completely hide drawers
;; (defun org-cycle-hide-drawers (state)
;;   "Re-hide all drawers after a visibility state change."
;;   (when (and (derived-mode-p 'org-mode)
;;              (not (memq state '(overview folded contents))))
;;     (save-excursion
;;       (let* ((globalp (memq state '(contents all)))
;;              (beg (if globalp
;;                     (point-min)
;;                     (point)))
;;              (end (if globalp
;;                     (point-max)
;;                     (if (eq state 'children)
;;                       (save-excursion
;;                         (outline-next-heading)
;;                         (point))
;;                       (org-end-of-subtree t)))))
;;         (goto-char beg)
;;         (while (re-search-forward org-drawer-regexp end t)
;;           (save-excursion
;;             (beginning-of-line 1)
;;             (when (looking-at org-drawer-regexp)
;;               (let* ((start (1- (match-beginning 0)))
;;                      (limit
;;                        (save-excursion
;;                          (outline-next-heading)
;;                            (point)))
;;                      (msg (format
;;                             (concat
;;                               "org-cycle-hide-drawers:  "
;;                               "`:END:`"
;;                               " line missing at position %s")
;;                             (1+ start))))
;;                 (if (re-search-forward "^[ \t]*:END:" limit t)
;;                   (outline-flag-region start (point-at-eol) t)
;;                   (user-error msg))))))))))
