;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     emacs-lisp
     org
     markdown
     deft
     syntax-checking
     ess
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(org-cliplink transpose-frame rainbow-mode helm-org-rifle org-download)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         spacemacs-light
                         spacemacs-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ";"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/user-config ()

  ;; backup settings
  ;;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq backup-by-copying t)
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  ;;; disable creation of lock files
  (setq create-lockfiles nil)

  ;; consider *.org.txt files as org files
  (add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

  ;; kill-buffer with y-or-n-p instead of yes-or-no-p
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; requirement for org-cliplink
  (load "~/emacs-request/request.el")

  ;; save whenever emacs is out of focus
  (defun save-all ()
    (interactive)
    (save-some-buffers t))
  (add-hook 'focus-out-hook 'save-all)

  ;; deft settings
  (setq deft-directory "~/Dropbox/org")
  (setq deft-extensions '("txt" "org"))

  ;; browser settings
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")

  ;; spacemacs requires the org settings to defined this way
  (with-eval-after-load 'org

    ;; helm-org-rifle settings
    (require 'helm-org-rifle)
    (setq helm-org-rifle-show-path t)

    ;; org-download settings
    (require 'org-download)
    (setq-default org-download-image-dir "~/Dropbox/org/pics")

    ;; Do not truncate lines and enable word wrap
    (set-default 'truncate-lines nil)
    (set-default 'word-wrap t)
    (setq helm-buffers-truncate-lines nil)
    (setq org-startup-truncated nil)

    ;; Enable the compact layout in agenda
    (setq org-agenda-compact-blocks t)

    ;; default appointment duration
    (setq org-agenda-default-appointment-duration 15)

    ;; redefining tab in org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda () (local-set-key [tab] 'org-agenda-tree-to-indirect-buffer)))

    ;; org files directory
    (setq org-agenda-files '("~/Dropbox/org" "~/Dropbox/org/pocket-to-org.org.txt" "~/Dropbox/org/zapier-to-org.org.txt"))

    ;; restore layout after exit from agenda view
    (setq org-agenda-restore-windows-after-quit t)

    ;; Don't show tasks in agenda if they are done
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)

    ;; Max number of days to show in agenda
    (setq org-agenda-span 21)

    ;; Warn about a deadline
    (setq org-deadline-warning-days 60)

    ;; org agenda starts on the current day
    (setq org-agenda-start-on-weekday nil)

    ;; dont do auto align tags
    (setq org-auto-align-tags nil)

    ;; org bullets config
    (setq org-bullets-bullet-list (quote ("◉" "◆" "✚" "☀" "○")))

    ;; count all checkboxes, not just the ones directly below
    (setq org-checkbox-hierarchical-statistics nil)

    ;; log the clocks into this drawer
    (setq org-log-into-drawer "LOGBOOK")

    ;; remember to clock out the clock on exit
    (setq org-remember-clock-out-on-exit t)

    ;; display clock time both in mode line and frame title
    (setq org-clock-clocked-in-display (quote both))

    ;; lists are also collapsed by default, not just headings
    (setq org-cycle-include-plain-lists 'integrate)

    ;; checkbox cant be checked unless all the children are not done
    (setq org-enforce-todo-checkbox-dependencies t)

    ;; todo cant be done unless all the children tasks are done
    (setq org-enforce-todo-dependencies t)

    ;; export formats
    (setq org-export-backends (quote (html icalendar md)))

    ;; calendar export settings
    (setq org-icalendar-exclude-tags (quote ("noexport")))
    (setq org-icalendar-include-todo t)
    (setq org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo)))
    (setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo)))

    ;; show all images with fixed width
    (setq org-image-actual-width 500)

    ;; org modules to load
    (setq org-modules (quote (org-crypt org-habit org-mouse)))

    ;; org refile settings
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
    (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

    ;; When you run an agenda command, Org visits agenda files that are not yet visited. When finding a file for the first time, Org checks the startup options and apply them to the buffer: those options are either globally set through the org-startup-* variables or on a per-file basis through the #+STARTUP keyword. Especially, Org will honor the startup visibility status, as set by org-startup-folded or #+STARTUP: folded. This may slow down the operation of visiting a file very much, and the process of selecting agenda entries consequently. To prevent agenda commands to honor startup options when visiting an agenda file for the first time, do this
    (setq org-agenda-inhibit-startup t)

    ;; on startup, the headings should be folded
    (setq org-startup-folded t)

    ;; org todo keywords
    (setq org-todo-keywords
          (quote
           ((sequence "TODO" "PROG" "PAUS" "|" "DONE" "CANC"))))

    ;; colors for todo states
    (setq org-todo-keyword-faces
          '(("PROG" . "orange") ("PAUS" . "magenta") ("CANC" . "red") ("DONE" . "green")))

    ;; org priority settings 7, 0, 9
    (setq org-default-priority 55)
    (setq org-highest-priority 48)
    (setq org-lowest-priority 57)

    ;; Org Capture settings
    (global-set-key (kbd "<f6>") 'org-capture)
    (setq org-capture-templates
          (quote (
                  ("w"         ; hotkey
                   "Work Todo" ; name
                   entry       ; type
                   (file+headline "~/Dropbox/org/work.org" "Tasks") ;target
                   "* TODO %^{Description}\n:PROPERTIES:\n:Added: %U\n:END:" ; template
                   )
                  ("t"
                   "Task Diary"
                   entry
                   (file+datetree "~/Dropbox/org/tasks.org")
                   "* TODO %^{Description}\n:PROPERTIES:\n:Added: %U\n:END:")
                  ("j"
                   "Journal"
                   entry
                   (file+datetree "~/Dropbox/org/journal.org")
                   "** %U - %^{Activity}")
                  )))

    ;; ess settings
    (setq ess-ask-for-ess-directory nil)
    (setq ansi-color-for-comint-mode 'filter)
    (setq comint-scroll-to-bottom-on-input t)
    (setq comint-scroll-to-bottom-on-output t)
    (setq comint-move-point-for-output t)
    (setq ess-eval-visibly-p nil)
    (require 'ess-site)

    ;; org babel settings
    (require 'ob-R)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (emacs-lisp . t)
       (python . t)))
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-export-babel-evaluate nil)
    (setq org-confirm-babel-evaluate nil)
    (add-to-list 'org-babel-default-header-args:R
                 '(:session . "*org-R*"))

    ;; sorting strategy for org agenda
    (setq org-agenda-sorting-strategy
          (quote
           ((agenda time-up deadline-up)
            (todo priority-down todo-state-down)
            (tags priority-down todo-state-down))))

    ;; text format for org agenda
    (setq org-agenda-prefix-format
          (quote
           ((agenda . "%s %?-12t %e")
            (timeline . "  %s")
            (todo . " %i %e")
            (tags . " %i %e")
            (search . " %i %e"))))

    ;; default format for columns view
    (setq org-columns-default-format
          "%75ITEM %TODO %PRIORITY %SCHEDULED %DEADLINE %CLOSED %ALLTAGS")

    ;; from http://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda
    ;; being used in a org agenda custom command below
    (defun cmp-date-property (prop)
      "Compare two `org-mode' agenda entries, `A' and `B', by some date property. If a is before b, return -1. If a is after b, return 1. If they are equal return t."
      (lexical-let ((prop prop))
        #'(lambda (a b)

            (let* ((a-pos (get-text-property 0 'org-marker a))
                   (b-pos (get-text-property 0 'org-marker b))
                   (a-date (or (org-entry-get a-pos prop)
                               (format "<%s>" (org-read-date t nil "now"))))
                   (b-date (or (org-entry-get b-pos prop)
                               (format "<%s>" (org-read-date t nil "now"))))
                   (cmp (compare-strings a-date nil nil b-date nil nil))
                   )
              (if (eq cmp t) nil (signum cmp))
              ))))

    ;; from http://emacs.stackexchange.com/questions/18710/display-count-of-tasks-in-agenda-instead-of-tasks-based-on-tag
    (load "~/org-agenda-count.el")

    (setq org-agenda-custom-commands
          (quote
           (("x" "Tasks done"
             ((tags "CLOSED>=\"<-4w>\"" nil))
             ((org-agenda-view-columns-initially t)
              (org-agenda-overriding-header "Tasks Done in the last week")
              (org-agenda-cmp-user-defined
               (cmp-date-property "CLOSED"))
              (org-agenda-sorting-strategy (quote (user-defined-down)))
              (org-agenda-window-setup
               (quote
                (only-window)))))
            ("z" "work separated"
             ((agenda "" ((org-agenda-span 5)))
              (tags "+PROJECT&-DONE" ((org-agenda-overriding-header "Projects")
                                      (org-agenda-hide-tags-regexp "PROJECT")))
              (tags-todo "-work"
                         ((org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                          (org-agenda-overriding-header "Non-Work Tasks")
                          ))
              (tags-todo "+work"
                         ((org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote deadline) (quote scheduled))))
                          (org-agenda-overriding-header "Work Tasks")
                          ))
              (tags "GOAL" (
                            (org-agenda-overriding-header "Goals")
                            ))
              (tags "CLOSED>=\"<-4w>\"" (
                                         (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
                                         (org-agenda-sorting-strategy '(user-defined-down))
                                         (org-agenda-overriding-header "Tasks Done in the last week")
                                         )))
             nil)
            ("Q" "closed tasks"
             ((tags "CLOSED>=\"<-4w>\"" (
                                         (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
                                         (org-agenda-sorting-strategy '(user-defined-down))
                                         (org-agenda-overriding-header (format "Tasks done in the last week (%s)" (org-agenda-count "CLOSED")))
                                         )))
             nil)
            ("W" "work todos"
             ((tags-todo "+work"
                         ((org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote deadline) (quote scheduled))))
                          (org-agenda-overriding-header (format "Work Tasks (%s)" (org-agenda-count "work")))
                          (org-agenda-hide-tags-regexp "work")
                          )))
             nil)
            ("E" "non-work todos"
             ((tags-todo "-work"
                         ((org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                          (org-agenda-overriding-header (format "Non-Work Tasks (%s)" (org-agenda-count "-work")))
                          )))
             nil)
            ("G" "Goals"
             ((tags "GOAL" ((org-agenda-overriding-header (format "Goals (%s)" (org-agenda-count "GOAL")))
                            (org-agenda-hide-tags-regexp "GOAL"))))
             nil)
            ("P" "Projects"
             ((tags "+PROJECT&-DONE" ((org-agenda-overriding-header (format "Projects (%s)" (org-agenda-count "+PROJECT&-DONE")))
                                      (org-agenda-hide-tags-regexp "PROJECT"))))
             nil)
            ("K" "Wishlist"
             ((tags "wishlist" ((org-agenda-overriding-header (format "Wishlist (%s)" (org-agenda-count "wishlist")))
                                (org-agenda-hide-tags-regexp "wishlist"))))
             nil)
            )))

    ;; Collapse everything except current tab.
    (defun org-show-current-heading-tidily ()
      (interactive)
      "Show next entry, keeping other entries closed."
      (if (save-excursion (end-of-line) (outline-invisible-p))
          (progn (org-show-entry) (show-children))
        (outline-back-to-heading)
        (unless (and (bolp) (org-on-heading-p))
          (org-up-heading-safe)
          (hide-subtree)
          (error "Boundary reached"))
        (org-overview)
        (org-reveal t)
        (org-show-entry)
        (show-children)))

    ;; Place tags close to the right-hand side of the window
    (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
    (defun place-agenda-tags ()
      "Put the agenda tags by the right border of the agenda window."
      (setq org-agenda-tags-column (- 4 (window-width)))
      (org-agenda-align-tags))

    ;; any items below the headings with these tags dont inherit that tag
    (setq org-tags-exclude-from-inheritance (quote ("PROJECT" "crypt")))

    ;; by default, agenda will reorganize buffers
    (setq org-agenda-window-setup 'reorganize-frame)

    ;; By default, Org maintains only a single agenda buffer and rebuilds it each time you change the view, to make sure everything is always up to date. If you often switch between agenda views and the build time bothers you, you can turn on sticky agenda buffers or make this the default by customizing the variable org-agenda-sticky. With sticky agendas, the agenda dispatcher will not recreate agenda views from scratch, it will only switch to the selected one, and you need to update the agenda by hand with r or g when needed. You can toggle sticky agenda view any time with org-toggle-sticky-agenda.
    (setq org-agenda-sticky nil)

    ;; custom format for timestamp
    (setq org-time-stamp-custom-formats (quote ("<%m/%d/%y %a>" . "<%Y-%m-%dT%H:%M:%S%z>")))

    ;; org config ends
    )

  ;; load any changes from disk
  (setq global-auto-revert-mode t)

  ;; commands and settings for dashboard
  (defun refresh-dashboard ()
    "Run some commands in sequence."
    (interactive)
    (message "%s" "i started")
    (message nil)
    (cl-loop repeat 6 do (execute-kbd-macro (kbd "r")) (other-window 1))
    (message "%s" "i ran")
    (message nil)
    )
  (defun org-dashboard ()
    "Dashboard-like setting in org"
    (interactive)
    (setq org-agenda-sticky t)
    (setq org-agenda-window-setup 'current-window)
    (setq-default mode-line-format nil)
    (split-window-right)
    (split-window-below)
    (split-window-below)
    (org-agenda nil "P")
    (other-window 1)
    (org-agenda nil "W")
    (other-window 1)
    (org-agenda nil "E")
    (other-window 1)
    (split-window-below)
    (split-window-right)
    (org-agenda nil "a")
    (other-window 1)
    (org-agenda nil "Q")
    (other-window 1)
    (split-window-below)
    (org-agenda nil "G")
    (other-window 1)
    (org-agenda nil "K")
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 1)
    (run-with-timer 0 (* 5 60) 'refresh-dashboard)
    )
  (global-set-key (kbd "<f7>") 'org-dashboard)

  ;; close dashboard
  (require 'cl)
  (defun bk-kill-buffers (regexp)
    "Kill buffers matching REGEXP without asking for confirmation."
    (interactive "sKill buffers matching this regular expression: ")
    (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
      (kill-matching-buffers regexp)))
  (defun close-dashboard ()
    "Dashboard-like setting in org"
    (interactive)
    (bk-kill-buffers ".*Org.*Agenda.*")
    (delete-other-windows)
    )

  ;; default file to open
  (find-file "~/Dropbox/org/main.org")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(line-spacing 0.25)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-link ((t (:foreground "#b58900" :underline nil)))))
