;; Config

;; [[file:~/emacs-spacemacs-config/my-general-config.org::*Config][Config:1]]
(with-eval-after-load 'helm
  (setq helm-display-function 'helm-default-display-buffer)) ;; temp work around

(require 'epa-file)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; (require 'python) ; if not done elsewhere
;; (require 'eval-in-repl-python)
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))

;; disabling arrows when word wrap is enabled
(setq-default visual-line-fringe-indicators nil)
(setq-default fringe-indicator-alist '(
                                       (truncation left-arrow right-arrow)
                                       (continuation nil nil) ;; left-curly-arrow
                                       (overlay-arrow . right-triangle)
                                       (up . up-arrow)
                                       (down . down-arrow)
                                       (top top-left-angle top-right-angle)
                                       (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                                       (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                                       (empty-line . empty-line)
                                       (unknown . question-mark)))

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
(load "~/emacs-spacemacs-config/emacs-request/request.el")

;; save whenever emacs is out of focus
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; default browser settings
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(require 'helm-bookmark)

;; spacemacs requires the org settings to defined this way
(with-eval-after-load 'org

  ;; crypt
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)

  ;; stylize exported html according to specified css
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-html5-fancy t
        org-html-doctype "html5")

  ;; org-publish
  (require 'ox-publish)
  (setq org-publish-project-alist
        '(
          ("org"
           :base-directory "~/Dropbox/org/"
           :publishing-directory "~/Dropbox/org/"
           :base-extension "---"
           :recursive nil
           :publishing-function org-html-publish-to-html
           :include ("bayesian.org" "classification.org" "clustering.org" "data_science_misc.org" "data_structs_algos.org" "deep_learning.org" "ds_tools.org" "machine_learning_misc.org" "nlp.org" "recommendations.org" "regression.org" "reinforcement-learning.org" "statistics.org" "supervised_learning.org" "time_series.org")
           )

          ))

  ;; org-indent
  (require 'org-indent)
  (setq org-startup-indented t)

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
  (setq org-agenda-default-appointment-duration 30)

  ;; redefining tab in org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (local-set-key [tab] 'org-agenda-tree-to-indirect-buffer)))

  ;; org files and directories
  (setq org-agenda-files '("~/Dropbox/org" "~/Dropbox/org/pocket-to-org.org.txt" "~/Dropbox/org/zapier-to-org.org.txt"))

  ;; restore layout after exit from agenda view
  (setq org-agenda-restore-windows-after-quit t)

  ;; Don't show tasks in agenda if they are done
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Max number of days to show in agenda
  (setq org-agenda-span 45)

  ;; Warn about a deadline
  (setq org-deadline-warning-days 90)

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

  ;; export formats
  (setq org-export-backends (quote (html icalendar md)))

  ;; change ... to
  ;; (setq org-ellipsis "⤵")

  ;; calendar export settings
  (setq org-icalendar-exclude-tags (quote ("noexport")))
  (setq org-icalendar-include-todo t)
  (setq org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo)))
  (setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo)))

  ;; set custom org-display-inline-images function
  (setq org-image-actual-width 1800)
  (load "~/emacs-spacemacs-config/org-display-inline-images-custom.el")

  ;; load org-pretty-tags
  (load "~/emacs-spacemacs-config/org-pretty-tags/org-pretty-tags.el")

  ;; Don't show tasks with "home" tag during day time
  (defun my/org-agenda-skip-home ()
    (let ((current-hour (string-to-number (format-time-string "%H"))))
      (when (and (< 10 current-hour 18)
                 (member "home" (org-get-tags-at)))
        (or (outline-next-heading)
            (goto-char (point-max))))))
  ;; (setq org-agenda-skip-function #'my/org-agenda-skip-home)


  ;; org modules to load
  (setq org-modules (quote (org-crypt org-habit org-mouse)))

  ;; org refile settings
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path (quote file))       ; Show full paths for refiling

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

  ;; org priority settings : default-H, highest-A, lowest-Z
  (setq org-default-priority 72)
  (setq org-highest-priority 65)
  (setq org-lowest-priority 90)

  ;; Org Capture settings
  (global-set-key (kbd "<f6>") 'org-capture)
  (setq org-capture-templates
        (quote (
                ("w"         ; hotkey
                 "Work Todo" ; name
                 entry       ; type
                 (file+headline "~/Dropbox/org/work.org" "Tasks") ;target
                 "* TODO [#A] %^{Task}" ; template
                 )
                ("t"
                 "Task Diary"
                 entry
                 (file+datetree "~/Dropbox/org/tasks.org")
                 "* TODO [#A] %^{Task}")
                ("j"
                 "Journal"
                 item
                 (file+datetree "~/Dropbox/org/journal.org")
                 "- %U - %^{Activity}")
                ("b"
                 "Add a book to read"
                 entry
                 (file+headline "~/Dropbox/org/notes.org" "Books to read")
                 "* TODO %^{Book name}\n%^{Why to read this book?}"
                 )
                ("s"
                 "Schedule an event or a task"
                 entry
                 (file+datetree "~/Dropbox/org/tasks.org")
                 "* %^{Event or Task}\nSCHEDULED: %^t"
                 )
                )))

  ;; sorting strategy for org agenda
  (setq org-agenda-sorting-strategy
        (quote
         ((agenda time-up deadline-up)
          (todo priority-down todo-state-down tag-up)
          (tags priority-down todo-state-down tag-up))))

  ;; text format for org agenda
  (setq org-agenda-prefix-format
        (quote
         ((agenda . "%s %?-12t %e ")
          (timeline . "  %s")
          (todo . " %i %e ")
          (tags . " %i %e ")
          (search . " %i %e "))))

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
  (load "~/emacs-spacemacs-config/org-agenda-count.el")

  (setq org-agenda-custom-commands
        (quote
         (
          ("Q" "Closed Tasks"
           ((tags "CLOSED>=\"<-4w>\"" (
                                       (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
                                       (org-agenda-sorting-strategy '(user-defined-down))
                                       (org-agenda-overriding-header (format "Tasks done in the last week (%s)" (org-agenda-count "CLOSED")))
                                       )))
           nil)
          ("H" "Z Tasks"
           ((tags-todo "+PRIORITY=\"Z\""
                       ((org-agenda-overriding-header (format "Z Tasks (%s)" (org-agenda-count ""))))))
           nil)
          ("W" "Work ToDos"
           ((tags-todo "+work"
                       ((org-agenda-overriding-header (format "Work Tasks (%s)" (org-agenda-count "")))
                        (org-agenda-hide-tags-regexp "work")
                        )))
           nil)
          ("E" "Non-Work ToDos"
           ((tags-todo "-work"
                       ((org-agenda-overriding-header (format "Non-Work Tasks (%s)" (org-agenda-count "")))
                        )))
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

  ;; org config ends
  )

;; load any changes from disk
(setq global-auto-revert-mode t)

;; commands and settings for dashboard
(defun refresh-dashboard ()
  "Run some commands in sequence."
  (interactive)
  ;; (message "%s" "i started")
  ;; (message nil)
  (cl-loop repeat 3 do (execute-kbd-macro (kbd "r")) (other-window 1))
  ;; (message "%s" "i ran")
  ;; (message nil)
  )
(defun org-dashboard ()
  "Dashboard-like setting in org"
  (interactive)
  (setq org-agenda-sticky t)
  (setq org-agenda-window-setup 'current-window)
  (setq-default mode-line-format nil)
  (split-window-right)
  ;; (split-window-below)
  ;; (org-agenda nil "W")
  ;; (other-window 1)
  (org-agenda nil "E")
  (other-window 1)
  (split-window-below)
  (org-agenda nil "a")
  (other-window 1)
  (org-agenda nil "Q")
  ;; (other-window 1)
  ;; (shrink-window-if-larger-than-buffer)
  ;; (other-window 2)
  ;; (shrink-window-horizontally 10)
  ;; (other-window 1)
  ;; (shrink-window 15)
  ;; (other-window 1)
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
  (cancel-function-timers 'refresh-dashboard)
  (bk-kill-buffers ".*Org.*Agenda.*")
  (delete-other-windows)
  )

;; default file to open
(find-file "~/Dropbox/org/main.org")
;; Config:1 ends here

;; Finalization
;; In the end, satisfy the Spacemacs loading mechanism.


;; [[file:~/emacs-spacemacs-config/my-general-config.org::*Finalization][Finalization:1]]
(provide 'my-general-config)
;; Finalization:1 ends here
