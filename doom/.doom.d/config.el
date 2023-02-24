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

(setq +zen-text-scale 0.8 )
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

(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename "~/gtd/roam")))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "Daniel Baker")

(use-package! org-archive
  :after org
  :config
  (setq org-archive-location (concat org-directory "/archive/archive.org::")))

(use-package! org
  :config
  (setq
   org-log-into-drawer t
   org-log-note-clock-out t
   org-todo-repeat-to-state "TODO"
   org-refile-targets '((org-buffer-list :maxlevel . 3 ) (org-agenda-files :maxlevel . 7))
   org-todo-keywords
        '((sequence
           "PROJ(p)"  ; A goal, which usually contains other tasks
           "NEXT(e)"  ; The next thing to do
           "STRT(s)"  ; A task that is in progress
           "WAIT(w@)"  ; Something external is holding up this task
           "TODO(t)"  ; A task that needs doing & is ready to do
           "|"
           "DONE(d!)"  ; Task successfully completed
           "KILL(k@)") ; Task was cancelled, aborted or is no longer applicable
          )
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("KILL" . +org-todo-cancel))))

(add-to-list 'org-modules 'org-checklist)
(setq initial-frame-alist '((top . 1) (left . 1) (width . 143) (height . 55)))

(add-to-list 'org-modules 'org-habit t)

(setq display-line-numbers-type 'relative)

;; TRANSPARENCY
(set-frame-parameter (selected-frame) 'alpha '(85))
(add-to-list 'default-frame-alist '(alpha 85))

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

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
  (defun dlb/count-done ()
    (interactive)
    (save-excursion
        ;; we need to end up *before* the start of the drawer in order
        ;; to parse it correctly, so we back up one line from where org-log-beginning tells us.
      (goto-char (org-log-beginning))
      (forward-line -1)
      (let ((contents (cadr (org-element-drawer-parser nil nil))))
        (count-lines (plist-get contents :contents-begin)
                     (plist-get contents :contents-end)))))

  (defun dlb/count (count)
    (let* ((reset-count-prop (org-entry-get (point) "reset-count"))
           (reset-count (or (and reset-count-prop (string-to-number reset-count-prop))
                            10)))
      (% count reset-count)))

  (defun dlb/put-count ()
    (interactive)
    (let ((count (ndk/count-done)))
      (org-entry-put (point) "done-count" (format "%d" (ndk/count count)))))
  (setq
   org-agenda-files '("~/gtd/plan.org")
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-time-grid '(
    (daily today require-timed remove-match)
    (0800 0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100)
    ": "
    "------FREE------")
   org-agenda-prefix-format '(
    ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
    ;; (agenda  . " %i %-12:c% s") ;; file name + org-agenda-entry-type
    (agenda  . " • %t %-:c ")
    (timeline  . "  % s")
    (todo  . " %i %-12:c")
    (tags  . " %i %-12:c")
    (search . " %i %-12:c"))
   org-agenda-custom-commands '(
                                ("g" "Scheduled today and all NEXT items"  ;; a useful view to see what can be accomplished today
                                 ((agenda "" (
                                        (org-agenda-time-grid nil)
                                        (org-agenda-span 1)
                                        (org-agenda-start-day "-0d")
                                        (org-agenda-start-on-weekday nil)
                                        (org-agenda-sorting-strategy '(category-up priority-down))
                                        (org-deadline-warning-days 7)
                                        (org-agenda-overriding-header "Today's plan ")))
                                  (todo "NEXT")
                                  (todo "STRT")
                                  (todo "WAIT")
                                  (todo "PROJ"))
                                 nil

                                ("~/gtd/todo.txt"))
                                ("R" "Weekly Review"
                                 ((agenda "" (
                                        (org-agenda-span 7)
                                        (org-agenda-start-day "-6d")
                                        (org-agenda-start-with-log-mode t)
                                        (org-agenda-use-time-grid nil)
                                        ))
                                  ))
                                ("b" agenda "Today's Deadlines"
                                 ((org-agenda-span 'day)
                                  (org-agenda-skip-function '(org-agenda-skip-deadline-if-not-today))
                                  (org-agenda-entry-types '(:deadline))
                                                  (org-agenda-overriding-header "Today's Deadlines ")))
                                ("c" "List TODOs grouped by category" alltodo "" (
                                        (org-agenda-span 1)
                                        (org-agenda-skip-scheduled-if-done nil)
                                        (org-agenda-start-day "-0d")
                                        (org-agenda-sorting-strategy '(category-up priority-down))
                                ))
                                ("p" "Daily plan"
                                ((agenda "" (
                                        (org-agenda-span 1)
                                        (org-agenda-skip-scheduled-if-done nil)
                                        (org-agenda-start-day "-0d")
                                        (org-agenda-start-with-log-mode t)
                                        (org-agenda-log-mode-items '(state clock))
                                        (org-agenda-start-on-weekday nil)
                                        (org-agenda-use-time-grid t)
                                        (org-agenda-sorting-strategy
                                         (quote ((agenda time-up priority-down))))
                                        (org-deadline-warning-days 14)))
                                 ))
                                ("w" "Weekly plan"
                                ((agenda "" (
                                        (org-agenda-span 7)
                                        (org-agenda-skip-scheduled-if-done t)
                                        (org-agenda-start-day "-0d")
                                        (org-agenda-start-on-weekday nil)
                                        (org-agenda-use-time-grid t)
                                        (org-agenda-sorting-strategy
                                         (quote ((agenda time-up priority-down))))
                                        (org-deadline-warning-days 14)))
                                 )
                                nil
                                ("~/gtd/calendar.ics" "~/gtd/calendar.html" "~/gtd/calendar.txt" "~/gtd/calendar.epub"))
                                )
                                
   org-capture-templates
      `(("i" "Inbox"
         entry (file ,(org-gtd-inbox-path))
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END: \n\n%i"
         :kill-buffer t)
        ("l" "Todo with link"
         entry (file ,(org-gtd-inbox-path))
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n  %a"
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


(defun my/clocktable-formatter-group-by-prop (ipos tables params)
  (let* ((formatter (or org-clock-clocktable-formatter
                        'org-clocktable-write-default))
         (ht (make-hash-table :test 'equal))
         (total 0)
         (grouped
          (dolist (tt tables (sort (hash-table-keys ht)
                                   #'(lambda (x y) (string< x y))))
            (setq total (+ total (nth 1 tt)))
            (dolist (record (nth 2 tt))
              (let* ((lasttwo (last record 2))
                     (time (pop lasttwo))
                     (prop (cdr (car (car lasttwo))))
                     (prev (gethash prop ht 0)))
                (puthash prop (+ prev time) ht))
              ))
          )
         (newtable (mapcar (lambda (arg) (list 1 arg nil nil (gethash arg ht) nil)) grouped))
         (new-params (org-plist-delete params :properties)))
    (funcall formatter ipos (list (list nil total newtable)) new-params)))

(defun my/org-sum-tally-in-subtree ()
  "Add up all the TALLY properties of headings underneath the current one
The total is written to the TALLY_SUM property of this heading"
  (interactive)
  (org-entry-put (point) "SCORE_SUM"
                 (number-to-string
                  (let ((total 0)(count 0))
                    (save-excursion
                      (org-map-tree
                       (lambda ()
                         (let ((n (org-entry-get (point) "SCORE")))
                           (when (stringp n)
                             (setq total (+ total (string-to-number n))
                                   count (+ 1 count)))))))
                    (/ total count)))))

(defun my/roam-dailies ()
  "Returns the org roam dailies dir"
  (concat org-roam-directory "/" org-roam-dailies-directory "*.org"))

(setq
  scroll-margin 10)                      ;; When to start moving the buffer up or down

(use-package! org-ql
  :after org)
(require 'org-ql-search)

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq
   org-pomodoro-length 50
   org-pomodoro-short-break-length 10
   ))

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

;; Auto refresh buffers on disk content change
(global-auto-revert-mode t)

;; run an emacs server
(server-start)
(setq-hook! 'web-mode-hook +format-with 'prettier)

;; ;; Hooks
;; (defun sync-to-cloud ()
;;   "Sync org file to Raspberry Pi with external script."
;;   (when (eq major-mode 'org-mode)
;;     (shell-command-to-string "rclone sync ~/gtd OneDrive:gtd")))

;; (add-hook 'after-save-hook #'sync-to-cloud)
(setq-default org-download-image-dir "~/gtd/.attach")
