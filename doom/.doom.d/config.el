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
  (setq org-archive-location "archive.org::datetree/"))

(use-package! org
  :config
  (setq
   org-log-into-drawer t
   org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(e)"  ; The next thing to do
           "GOAL(g)"  ; A goal, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "DELG(D)"  ; Task was delegated to someone else.
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)" ; Task was cancelled, aborted or is no longer applicable
           "BACK(b)") ; Task was scheduled in the backlog
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[>](P)"   ; Task is planned
           "[X](D)")  ; Task was completed
          (sequence
           "?(q)"
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("GOAL" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))

(add-to-list 'org-modules 'org-checklist)
(setq initial-frame-alist '((top . 1) (left . 1) (width . 143) (height . 55)))

(use-package! org-journal
  :after org
  :config

  (setq
   org-journal-date-prefix "#+TITLE: "
   org-journal-time-prefix "* "
   org-journal-date-format "%a, %Y-%m-%d"
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-enable-agenda-integration t
   org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
  (add-to-list 'org-agenda-files org-journal-dir))

(add-to-list 'org-modules 'org-habit t)

(use-package! org-roam
  :after org
  :config
  (setq
   org-roam-directory "~/org-roam"              ;; Set the org-roam directory
   org-roam-dailies-directory "daily/"          ;; Required to use org roam daily notes
   org-roam-dailies-capture-templates           ;; Sets the capture templte for org roam dailies
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-autosync-mode))

(setq display-line-numbers-type 'relative)

(set-frame-parameter (selected-frame) 'alpha '(95))

(add-to-list 'default-frame-alist '(alpha 95))


(use-package! org-gtd
  :after org
  :demand t
  :config
  (setq
   org-agenda-files `(,org-gtd-directory)
   org-agenda-custom-commands '(
                                ("g" "Scheduled today and all NEXT items"  ;; a useful view to see what can be accomplished today
                                 ((agenda "" ((org-agenda-span 1)))
                                  (todo "NEXT")
                                  (todo "STRT")
                                  (todo "WAIT")
                                  (todo "GOAL")))
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

(map! :leader
      (:prefix-map ("e" . "org")
       (:prefix ("g" . "gtd ")
        :desc "Capture" "c" #'org-gtd-capture
        :desc "Agenda list" "a" #'org-agenda-list
        :desc "Process inbox" "p" #'org-gtd-process-inbox
        :desc "All next actions" "n" #'org-gtd-show-all-next
        :desc "Stuck projects" "s" #'org-gtd-show-stuck-projects
        :desc "Clarify and finalise" "f" #'org-gtd-clarify-finalize)))
