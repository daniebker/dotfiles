;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Daniel baker"
      user-mail-address "bakerdude@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula) ;; << This line enables the theme


;;;;;;;;
;; ORG
;;;;;;;;


(use-package! org-archive
  :after org
  :config
  (setq org-archive-location (concat org-directory "/archive/archive.org::")))

(use-package! org
  :config
  (setq
   org-log-into-drawer t
   org-refile-targets '((org-buffer-list :maxlevel . 3 ) (org-agenda-files :maxlevel . 3))
   org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(e)"  ; The next thing to do
           "GOAL(g)"  ; A goal, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w@)"  ; Something external is holding up this task
           "DELG(D@)"  ; Task was delegated to someone else.
           "|"
           "DONE(d!)"  ; Task successfully completed
           "KILL(k@)") ; Task was cancelled, aborted or is no longer applicable
          )
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("GOAL" . +org-todo-project)
          ("KILL" . +org-todo-cancel))))

(add-to-list 'org-modules 'org-checklist)
(setq initial-frame-alist '((top . 1) (left . 1) (width . 143) (height . 55)))

(add-to-list 'org-modules 'org-habit t)

(setq display-line-numbers-type 'relative)

(set-frame-parameter (selected-frame) 'alpha '(85))

(add-to-list 'default-frame-alist '(alpha 85))

(defun my-agenda-prefix ()
  (format "%s" (my-agenda-indent-string (org-current-level))))

(defun my-agenda-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "\t"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str " ")))
      (concat str "\\_"))))

(defun my-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(use-package! org-gtd
  :after org
  :demand t
  :config
  (setq
   org-agenda-files `(,org-gtd-directory)
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-habit-show-all-today t
   org-agenda-custom-commands '(
                                ("g" "Scheduled today and all NEXT items"  ;; a useful view to see what can be accomplished today
                                 ((agenda "" (
                                        (org-agenda-span 1)
                                        (org-agenda-start-day "-0d")
                                        (org-agenda-start-on-weekday nil)
                                        (org-deadline-warning-days 7)))
                                  (todo "NEXT")
                                  (todo "STRT")
                                  (todo "WAIT")
                                  (todo "GOAL")))
                                ("w" "Daily work plan"
                                ((agenda "" (
                                        (org-agenda-span 1)
                                        (org-agenda-start-day "-0d")
                                        (org-agenda-start-on-weekday nil)
                                        (org-agenda-sorting-strategy
                                         (quote ((agenda time-up priority-down))))
                                        (org-deadline-warning-days 3)))
                                 (todo "STRT"
                                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
                                 (todo "TODO|GOAL|WAIT|NEXT"
                                       ((org-agenda-prefix-format " %e %(my-agenda-prefix) ")
                                        (org-tags-match-list-sublevels t)))

                                 (todo "DONE")
                                 )
                                ((org-agenda-tag-filter-preset '("+work"))))
                                ("p" "Daily personal plan"((agenda "" (
                                        (org-agenda-span 2)
                                        (org-agenda-start-day "-0d")
                                        (org-agenda-start-on-weekday nil)
                                        (org-agenda-sorting-strategy
                                         (quote ((agenda time-up priority-down))))
                                        (org-deadline-warning-days 3)))
                                 (todo "STRT"
                                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
                                 (todo "NEXT|GOAL"
                                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                                       (org-agenda-prefix-format " %e %(my-agenda-prefix) ")
                                        (org-tags-match-list-sublevels t)))
                                 (todo "TODO|GOAL|WAIT"
                                       ((org-agenda-prefix-format " %e %(my-agenda-prefix) ")
                                        (org-agenda-skip-function '(my-org-skip-subtree-if-habit))
                                        (org-tags-match-list-sublevels t)))

                                 (todo "DONE|GOAL|STRT"
                                       ((org-agenda-prefix-format " %e %(my-agenda-prefix) ")
                                        (org-tags-match-list-sublevels t)))
                                 )
                                ((org-agenda-tag-filter-preset '("+personal"))))
                                )
                                
   org-capture-templates
      `(("i" "Inbox"
         entry (file ,(org-gtd-inbox-path))
         "* %?\n%U\n\n  %i"
         :kill-buffer t)
        ("l" "Todo with link"
         entry (file ,(org-gtd-inbox-path))
         "* %?\n%U\n\n  %i\n  %a"
         :kill-buffer t))))

;; this allows you use `(,org-gtd-directory) for your agenda files
(use-package org-agenda
  :ensure nil
  :after org-gtd)

;; this allows you to use (org-gtd-inbox-path) for your capture destinations
(use-package org-capture
  :ensure nil
  :after org-gtd)

(map! :after org
      :map org-mode-map
      :localleader
      (:prefix ("G" . "gtd")
        :desc "Capture" "c" #'org-gtd-capture
        :desc "Agenda list" "a" #'org-agenda-list
        :desc "Process inbox" "p" #'org-gtd-process-inbox
        :desc "All next actions" "n" #'org-gtd-show-all-next
        :desc "Stuck projects" "s" #'org-gtd-show-stuck-projects
        :desc "Clarify and finalise" "f" #'org-gtd-clarify-finalize))


(setq
  scroll-margin 10)                      ;; When to start moving the buffer up or down

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq
   org-pomodoro-length 50
   org-pomodoro-short-break-length 10
   ))

(use-package org-gcal
  :config
  (setq
   org-log-reschedule nil))

(defun my-org-gcal-set-scheduled (_calendar-id event _update-mode)
  "Set SCHEDULED property based on EVENT if it's not an recurring event with old start."
  (let*
      ((stime (plist-get (plist-get event :start)
                         :dateTime))
       (etime (plist-get (plist-get event :end)
                         :dateTime))
       (sday  (plist-get (plist-get event :start)
                         :date))
       (eday  (plist-get (plist-get event :end)
                         :date))
       (start (if stime (org-gcal--convert-time-to-local-timezone stime org-gcal-local-timezone) sday))
       (end   (if etime (org-gcal--convert-time-to-local-timezone etime org-gcal-local-timezone) eday))
       (old-time-desc (org-gcal--get-time-and-desc))
       (old-start (plist-get old-time-desc :start))
       (old-end (plist-get old-time-desc :start))
       (recurrence (plist-get event :recurrence))
       (timestamp
        (if (or (string= start end) (org-gcal--alldayp start end))
            (org-gcal--format-iso2org start)
          (if (and
               (= (plist-get (org-gcal--parse-date start) :year)
                  (plist-get (org-gcal--parse-date end)   :year))
               (= (plist-get (org-gcal--parse-date start) :mon)
                  (plist-get (org-gcal--parse-date end)   :mon))
               (= (plist-get (org-gcal--parse-date start) :day)
                  (plist-get (org-gcal--parse-date end)   :day)))
              (format "<%s-%s>"
                      (org-gcal--format-date start "%Y-%m-%d %a %H:%M")
                      (org-gcal--format-date end "%H:%M"))
            (format "%s--%s"
                    (org-gcal--format-iso2org start)
                    (org-gcal--format-iso2org
                     (if (< 11 (length end))
                         end
                       (org-gcal--iso-previous-day end))))))))
    (unless (and recurrence old-start) (org-schedule nil timestamp))))
(add-hook 'org-gcal-after-update-entry-functions #'my-org-gcal-set-scheduled)

(use-package org-edna
  :ensure t
  :commands (org-edna-mode))

(org-edna-mode)

 (setenv "NODE_PATH"
      (concat
       (getenv "HOME") "/gtd/node_modules"  ":"
       (getenv "NODE_PATH")
     )
    )

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((js . t)))


;;; ARCHIVING


(require 'org-archive)

; Set the function to use for org-archive-default  (C-c C-x C-a)
;; ()
; (setq org-archive-location "archive/archived_%s::")
;; (setq org-archive-location "::* ARCHIVED")

; unmap org-archive-subtree
;; (define-key org-mode-map (kbd "C-c C-x C-s") nil)

; select command to execute via org-archive-subtree-default (C-c C-x C-a)
(setq org-archive-default-command 'org-archive-subtree-hierarchical)

(defun line-content-as-string ()
  "Returns the content of the current line as a string"
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun org-child-list (&optional top-level)
  "This function returns all children of a heading as a list. "
  (interactive)
  (save-excursion
    ;; this only works with org-version > 8.0, since in previous
    ;; org-mode versions the function (org-outline-level) returns
    ;; gargabe when the point is not on a heading.
    (unless top-level
        (if (= (org-outline-level) 0)
            (outline-next-visible-heading 1)
        (org-goto-first-child)))
    (let ((child-list (list (line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (line-content-as-string) child-list)))
      child-list)))

(defun fa/org-struct-subtree ()
  "This function returns the tree structure in which a subtree belongs as a list."
  (interactive)
  (let ((archive-tree nil))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))

(defun org-archive-subtree-hierarchical ()
  "This function archives a subtree hierarchical"
  (interactive)
  (let ((org-tree (fa/org-struct-subtree))
        (source-buffer (current-buffer))
        (file (abbreviate-file-name
                   (or (buffer-file-name (buffer-base-buffer))
                       (error "No file associated to buffer")))))
    (save-excursion
      (setq location (org-archive--compute-location
                (or (org-entry-get nil "ARCHIVE" 'inherit)
                    org-archive-location))
            afile (car location)
            heading (cdr location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))
      (if (not (equal heading ""))
          (progn
            (setq org-tree (cons heading
                               (mapcar (lambda (s) (concat "*" s)) org-tree)))
            (org-demote-subtree)))
      (if (> (length afile) 0)
        (progn
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                target-buffer (or visiting (find-file-noselect afile))))
        (progn
          (setq target-buffer (current-buffer))))
      (unless target-buffer
        (error "Cannot access file \"%s\"" afile))
      (org-cut-subtree)
      (set-buffer target-buffer)
      (setq ind-target-buffer (clone-indirect-buffer nil nil))
      (set-buffer ind-target-buffer)
      (org-mode)
      (goto-char (point-min))

      ; simplified version of org-complex-heading-regexp-format
	  (setq my-org-complex-heading-regexp-format
	      (concat "^"
		      "\\(%s\\)"
		      "\\(?: *\\[[0-9%%/]+\\]\\)*"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?"
		      "[ \t]*$"))
      (setq top-level-p t)
      (while (not (equal org-tree nil))
        (let ((child-list (org-child-list top-level-p))
              (re (format my-org-complex-heading-regexp-format (regexp-quote (car org-tree))))
             )
          (if (member "______FOUND_MATCH" (mapcar (lambda (s) (replace-regexp-in-string re "______FOUND_MATCH" s)) child-list))
              (progn
                (re-search-forward re nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (if (not top-level-p) (newline))
              (org-insert-struct org-tree)
              (setq org-tree nil))))
        (setq top-level-p nil))
      (newline)
      (org-yank)
      ;; Kill the indirect buffer, returning the current buffer to the direct target buffer
      (kill-buffer ind-target-buffer)
      ;; Save and kill the target buffer, if it is not the source buffer.
      (when (not (eq source-buffer target-buffer))
            (save-buffer target-buffer)
            (kill-buffer target-buffer))
      ;; ensure font-lock and indentation are normal
      (set-buffer source-buffer)
      (org-restart-font-lock)
      (org-indent-mode t)
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))

(defun org-insert-struct (struct)
  "TODO"
  (interactive)
  (when struct
    (insert (car struct))
    (if  (not (equal (length struct) 1))
        (newline))
    (org-insert-struct (cdr struct))))
