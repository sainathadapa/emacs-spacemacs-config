;; Initialize variables

;; [[file:org-config.org::*Initialize variables][Initialize variables:1]]
(setq org-directory "~/Dropbox/org")
(setq my-config-folder "~/emacs-spacemacs-config")
;; Initialize variables:1 ends here

;; Appearance
;; Indent headings and text

;; [[file:org-config.org::*Appearance][Appearance:1]]
(require 'org-indent)
(setq org-startup-indented t)
;; Appearance:1 ends here



;; Do not truncate lines and enable word wrap

;; [[file:org-config.org::*Appearance][Appearance:2]]
(set-default 'truncate-lines nil)
(set-default 'word-wrap t)
(setq helm-buffers-truncate-lines nil)
(setq org-startup-truncated nil)
;; Appearance:2 ends here



;; Do not align Tags automatically

;; [[file:org-config.org::*Appearance][Appearance:3]]
(setq org-auto-align-tags nil)
;; Appearance:3 ends here



;; Set the symbols for different heading levels

;; [[file:org-config.org::*Appearance][Appearance:4]]
(setq org-bullets-bullet-list (quote ("◉" "◆" "✚" "☀" "○")))
;; Appearance:4 ends here



;; On startup, content should be in folded state

;; [[file:org-config.org::*Appearance][Appearance:5]]
(setq org-startup-folded t)
;; Appearance:5 ends here



;; Count all checkboxes, not just the ones directly below

;; [[file:org-config.org::*Appearance][Appearance:6]]
(setq org-checkbox-hierarchical-statistics nil)
;; Appearance:6 ends here



;; Custom ~org-display-inline-images~ function that displays the images according to the logic:
;; 1. Always conserve the aspect ratio
;; 2. Image shouldn't exceed the current window's width (minus 50 pixels)
;; 3. Image shouldn't exceed half of the current window's height
;; 4. Resize only if the actual dimensions do not conform to the above two points

;; [[file:org-config.org::*Appearance][Appearance:7]]
(setq org-image-actual-width 1800)
(load (concat my-config-folder "/org-display-inline-images-custom.el"))
;; Appearance:7 ends here



;; Smooth scrolling with mouse:
;; from https://emacs.stackexchange.com/questions/10354/smooth-mouse-scroll-for-inline-images

;; [[file:org-config.org::*Appearance][Appearance:8]]
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast
;; Appearance:8 ends here

;; To-Do states and related
;; Keywords

;; [[file:org-config.org::*To-Do states and related][To-Do states and related:1]]
(setq org-todo-keywords
      (quote
       ((sequence "TODO" "PROG" "PAUS" "|" "DONE" "CANC"))))
;; To-Do states and related:1 ends here



;; Colors for todo states

;; [[file:org-config.org::*To-Do states and related][To-Do states and related:2]]
(setq org-todo-keyword-faces
      '(("PROG" . "orange") ("PAUS" . "magenta") ("CANC" . "red") ("DONE" . "green")))
;; To-Do states and related:2 ends here



;; Priority settings : default is H, highest is A, and lowest is Z

;; [[file:org-config.org::*To-Do states and related][To-Do states and related:3]]
(setq org-default-priority 72)
(setq org-highest-priority 65)
(setq org-lowest-priority 90)
;; To-Do states and related:3 ends here

;; Capture
;; Hotkey

;; [[file:org-config.org::*Capture][Capture:1]]
(global-set-key (kbd "<f6>") 'org-capture)
;; Capture:1 ends here



;; Templates

;; [[file:org-config.org::*Capture][Capture:2]]
(setq org-capture-templates
      '(
        ("w"         ; hotkey
         "Work Todo" ; name
         entry       ; type
         (file+headline (lambda () (concat org-directory "/work.org")) "Tasks") ;target
         "* TODO [#A] %^{Task}" ; template
         )
        ("t"
         "Task Diary"
         entry
         (file+datetree (lambda () (concat org-directory "/tasks.org")))
         "* TODO [#A] %^{Task}")
        ("p"
         "Journal"
         item
         (file+datetree (lambda () (concat org-directory "/journal.org")))
         "- %U - %^{Activity}")
        ("j"
         "Work log"
         item
         (file+olp+datetree (lambda () (concat org-directory "/work.org")) "Log")
         "- %U - %^{Activity}")
        ("b"
         "Add a book to read"
         entry
         (file+headline (lambda () (concat org-directory "/notes.org")) "Books to read")
         "* TODO %^{Book name}\n%^{Why to read this book?}"
         )
        ("s"
         "Schedule an event or a task"
         entry
         (file+datetree (lambda () (concat org-directory "/tasks.org")))
         "* %^{Event or Task}\nSCHEDULED: %^t"
         )
        ))
;; Capture:2 ends here

;; Agenda
;; Enable the compact layout in agenda

;; [[file:org-config.org::*Agenda][Agenda:1]]
(setq org-agenda-compact-blocks t)
;; Agenda:1 ends here



;; Restore layout after exit from agenda view

;; [[file:org-config.org::*Agenda][Agenda:2]]
(setq org-agenda-restore-windows-after-quit t)
;; Agenda:2 ends here



;; Default appointment duration

;; [[file:org-config.org::*Agenda][Agenda:3]]
(setq org-agenda-default-appointment-duration 30)
;; Agenda:3 ends here



;; Pressing ~Tab~ while the cursor is on a task will expand that task in a separate buffer

;; [[file:org-config.org::*Agenda][Agenda:4]]
(add-hook 'org-agenda-mode-hook
          (lambda () (local-set-key [tab] 'org-agenda-tree-to-indirect-buffer)))
;; Agenda:4 ends here



;; Include these files and directories when creating the agenda

;; [[file:org-config.org::*Agenda][Agenda:5]]
;; (setq org-agenda-files '(org-directory))
(setq org-agenda-files (append
                        (directory-files-recursively org-directory "\\.org$")
                        (directory-files-recursively org-directory "\\.org.txt$")))
;; Agenda:5 ends here



;; Don't show tasks in agenda if they are done

;; [[file:org-config.org::*Agenda][Agenda:6]]
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
;; Agenda:6 ends here



;; Max number of days to show in agenda

;; [[file:org-config.org::*Agenda][Agenda:7]]
(setq org-agenda-span 90)
;; Agenda:7 ends here



;; Warn about a deadline

;; [[file:org-config.org::*Agenda][Agenda:8]]
(setq org-deadline-warning-days 90)
;; Agenda:8 ends here



;; Agenda starts on the current day

;; [[file:org-config.org::*Agenda][Agenda:9]]
(setq org-agenda-start-on-weekday nil)
;; Agenda:9 ends here



;; Sorting strategy

;; [[file:org-config.org::*Agenda][Agenda:10]]
(setq org-agenda-sorting-strategy
      (quote
       ((agenda priority-down alpha-up)
        (todo priority-down alpha-up)
        (tags priority-down alpha-up))))
;; Agenda:10 ends here



;; Display format

;; [[file:org-config.org::*Agenda][Agenda:11]]
(setq org-agenda-prefix-format
      (quote
       ((agenda . "%s %?-12t %e ")
        (timeline . "  %s")
        (todo . " %i %e ")
        (tags . " %i %e ")
        (search . " %i %e "))))
;; Agenda:11 ends here



;; Default format for columns view

;; [[file:org-config.org::*Agenda][Agenda:12]]
(setq org-columns-default-format
      "%75ITEM %TODO %PRIORITY %SCHEDULED %DEADLINE %CLOSED %ALLTAGS")
;; Agenda:12 ends here



;; Place tags close to the right-hand side of the window. From http://lists.gnu.org/archive/html/emacs-orgmode//2010-12/msg00410.html

;; [[file:org-config.org::*Agenda][Agenda:13]]
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))
;; Agenda:13 ends here



;; By default, agenda will reorganize frames/splits

;; [[file:org-config.org::*Agenda][Agenda:14]]
(setq org-agenda-window-setup 'reorganize-frame)
;; Agenda:14 ends here



;; By default, Org maintains only a single agenda buffer and rebuilds it each time you change the view, to make sure everything is always up to date. If you often switch between agenda views and the build time bothers you, you can turn on sticky agenda buffers or make this the default by customizing the variable org-agenda-sticky. With sticky agendas, the agenda dispatcher will not recreate agenda views from scratch, it will only switch to the selected one, and you need to update the agenda by hand with r or g when needed. You can toggle sticky agenda view any time with org-toggle-sticky-agenda.

;; [[file:org-config.org::*Agenda][Agenda:15]]
(setq org-agenda-sticky nil)
;; Agenda:15 ends here



;; When you run an agenda command, Org visits agenda files that are not yet visited. When finding a file for the first time, Org checks the startup options and apply them to the buffer: those options are either globally set through the org-startup-* variables or on a per-file basis through the #+STARTUP keyword. Especially, Org will honor the startup visibility status, as set by org-startup-folded or #+STARTUP: folded. This may slow down the operation of visiting a file very much, and the process of selecting agenda entries consequently. To prevent agenda commands to honor startup options when visiting an agenda file for the first time, do this

;; [[file:org-config.org::*Agenda][Agenda:16]]
(setq org-agenda-inhibit-startup t)
;; Agenda:16 ends here

;; Helper functions
;; Extract the date of completion, and use it for comparison. From http://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda

;; [[file:org-config.org::*Helper functions][Helper functions:1]]
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
;; Helper functions:1 ends here



;; Display the total number of tasks in Agenda. From http://emacs.stackexchange.com/questions/18710/display-count-of-tasks-in-agenda-instead-of-tasks-based-on-tag

;; [[file:org-config.org::*Helper functions][Helper functions:2]]
(load (concat my-config-folder "/org-agenda-count.el"))
;; Helper functions:2 ends here



;; Sort agenda items by link's text and not link's URL

;; [[file:org-config.org::*Helper functions][Helper functions:3]]
(defun remove-priority (str)
  (replace-regexp-in-string "\\[#[^\\[]*\\] " "" str))

(defun extract-link-text (str)
  (replace-regexp-in-string "\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]" "\\3" str))

(defun org-cmp-alpha-2 (a b)
  "Compare the headlines, alphabetically. (after extract link texts if any links present)"
  (let* ((pla (text-property-any 0 (length a) 'org-heading t a))
         (plb (text-property-any 0 (length b) 'org-heading t b))
         (ta (and pla (substring a pla)))
         (tb (and plb (substring b plb)))
         (case-fold-search nil))
    (when pla
      (when (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp a) "")
                                  "\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *") ta)
        (setq ta (substring ta (match-end 0))))
      (setq ta (downcase ta)))
    (when plb
      (when (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp b) "")
                                  "\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *") tb)
        (setq tb (substring tb (match-end 0))))
      (setq tb (downcase tb)))
    (setq ta (extract-link-text ta))
    (setq tb (extract-link-text tb))
    (cond ((not (or ta tb)) nil)
          ((not ta) +1)
          ((not tb) -1)
          ((string-lessp ta tb) -1)
          ((string-lessp tb ta) +1))))
;; Helper functions:3 ends here

;; Views

;; [[file:org-config.org::*Views][Views:1]]
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
         ((tags-todo "-work-paper" (
                              (org-agenda-overriding-header (format "Non-Work Tasks (%s)" (org-agenda-count "")))
                              (org-agenda-cmp-user-defined 'org-cmp-alpha-2)
                              (org-agenda-sorting-strategy '(user-defined-up))
                              )))
         nil)
        )))
;; Views:1 ends here

;; Export
;; Stylize exported html according to specified CSS

;; [[file:org-config.org::*Export][Export:1]]
(setq org-html-htmlize-output-type 'css)
(setq org-html-html5-fancy t
      org-html-doctype "html5")
;; Export:1 ends here



;; Backends to enable

;; [[file:org-config.org::*Export][Export:2]]
(setq org-export-backends (quote (html icalendar md)))
;; Export:2 ends here



;; Do not use babel on export

;; [[file:org-config.org::*Export][Export:3]]
(setq org-export-use-babel nil)
;; Export:3 ends here

;; Refile

;; [[file:org-config.org::*Refile][Refile:1]]
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path (quote file))       ; Show full paths for refiling
;; Refile:1 ends here

;; Clocking
;; Log the clocks into this drawer

;; [[file:org-config.org::*Clocking][Clocking:1]]
(setq org-log-into-drawer "LOGBOOK")
;; Clocking:1 ends here



;; Remember to clock out the clock on exit

;; [[file:org-config.org::*Clocking][Clocking:2]]
(setq org-remember-clock-out-on-exit t)
;; Clocking:2 ends here



;; Display clock time both in mode line and frame title

;; [[file:org-config.org::*Clocking][Clocking:3]]
(setq org-clock-clocked-in-display (quote both))
;; Clocking:3 ends here

;; Miscellaneous
;; Modules to load

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:1]]
(setq org-modules (quote (org-crypt org-habit org-mouse)))
;; Miscellaneous:1 ends here



;; Prevent editing in the invisible area

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:2]]
(setq org-catch-invisible-edits (quote show-and-error))
;; Miscellaneous:2 ends here



;; Do not show empty lines between subtrees, when collapsed

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:3]]
(setq org-cycle-separator-lines 0)
;; Miscellaneous:3 ends here



;; Collapse everything except current tab. From https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:4]]
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
;; Miscellaneous:4 ends here



;; ~helm-org-rifle~ settings

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:5]]
(require 'helm-org-rifle)
(setq helm-org-rifle-show-path t)
;; Miscellaneous:5 ends here



;; ~org-download~ settings

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:6]]
(require 'org-download)
(setq-default org-download-image-dir (concat org-directory "/pics"))
;; Miscellaneous:6 ends here



;; Load a requirement for ~org-cliplink~

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:7]]
(load (concat my-config-folder "/emacs-request/request.el"))
;; Miscellaneous:7 ends here



;; Alerts:
;; - https://github.com/akhramov/org-wild-notifier.el
;; - https://github.com/spegoraro/org-alert

;; [[file:org-config.org::*Miscellaneous][Miscellaneous:8]]
(require 'org-alert)
(setq alert-default-style 'libnotify)
;; Miscellaneous:8 ends here

;; Dashboard
;; Create a dashboard with multiple Agenda views

;; [[file:org-config.org::*Dashboard][Dashboard:1]]
(defun org-dashboard ()
  "Dashboard-like setting in org"
  (interactive)
  (setq org-agenda-sticky t)
  (setq org-agenda-window-setup 'current-window)
  (setq-default mode-line-format nil)
  (split-window-right)
  (split-window-below)
  (org-agenda nil "W")
  (other-window 1)
  (org-agenda nil "E")
  (shrink-window 50)
  (other-window 1)
  ;; (split-window-below)
  (org-agenda nil "a")
  (other-window 1)
  (shrink-window 15)
  ;; (org-agenda nil "Q")
  ;; (other-window 1)
  ;; (shrink-window-if-larger-than-buffer)
  ;; (other-window 2)
  ;; (shrink-window-horizontally 10)
  ;; (other-window 1)
  ;; (other-window 1)
  (run-with-timer 0 (* 5 60) 'refresh-dashboard)
  )

(defun refresh-dashboard ()
  "Run some commands in sequence."
  (interactive)
  ;; (message "%s" "i started")
  ;; (message nil)
  (cl-loop repeat 3 do (execute-kbd-macro (kbd "r")) (other-window 1))
  ;; (message "%s" "i ran")
  ;; (message nil)
  )

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
;; Dashboard:1 ends here

;; Disabled

;; [[file:org-config.org::*Disabled][Disabled:1]]
;; any items below the headings with these tags dont inherit that tag
;; (setq org-tags-exclude-from-inheritance (quote ("PROJECT" "crypt")))

;; crypt
;; (require 'org-crypt)
;; (org-crypt-use-before-save-magic)
;; (setq org-tags-exclude-from-inheritance (quote ("crypt")))

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
;; (setq org-crypt-key nil)

;; org-publish
;; (require 'ox-publish)
;; (setq org-publish-project-alist
;;       '(
;;         ("org"
;;          :base-directory "~/Dropbox/org/"
;;          :publishing-directory "~/Dropbox/org/"
;;          :base-extension "---"
;;          :recursive nil
;;          :publishing-function org-html-publish-to-html
;;          :include ("bayesian.org" "classification.org" "clustering.org" "data_science_misc.org" "data_structs_algos.org" "deep_learning.org" "ds_tools.org" "machine_learning_misc.org" "nlp.org" "recommendations.org" "regression.org" "reinforcement-learning.org" "statistics.org" "supervised_learning.org" "time_series.org")
;;          )))

;; change ... to
;; (setq org-ellipsis "⤵")

;; calendar export settings
;; (setq org-icalendar-exclude-tags (quote ("noexport")))
;; (setq org-icalendar-include-todo t)
;; (setq org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo)))
;; (setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo)))

;; lists are also collapsed by default, not just headings
;; (setq org-cycle-include-plain-lists 'integrate)

;; Don't show tasks with "home" tag during day time
;; (defun my/org-agenda-skip-home ()
;;   (let ((current-hour (string-to-number (format-time-string "%H"))))
;;     (when (and (< 10 current-hour 18)
;;                (member "home" (org-get-tags-at)))
;;       (or (outline-next-heading)
;;           (goto-char (point-max))))))
;; (setq org-agenda-skip-function #'my/org-agenda-skip-home)
;; Disabled:1 ends here

;; Final
;; Let the Spacemacs use this configuration.

;; [[file:org-config.org::*Final][Final:1]]
(provide 'org-config)
;; Final:1 ends here
