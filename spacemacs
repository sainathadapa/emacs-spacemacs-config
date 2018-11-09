;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     emacs-lisp
     org
     markdown
     html
     spell-checking
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(org-cliplink transpose-frame rainbow-mode helm-org-rifle org-download eval-in-repl)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(org-projectile)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ";"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()

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
    (setq org-agenda-span 31)

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

    ;; calendar export settings
    (setq org-icalendar-exclude-tags (quote ("noexport")))
    (setq org-icalendar-include-todo t)
    (setq org-icalendar-use-deadline (quote (event-if-not-todo event-if-todo)))
    (setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo)))

    ;; set custom org-display-inline-images function
    (setq org-image-actual-width 1800)
    (load "~/emacs-spacemacs-config/org-display-inline-images-custom.el")

    ;; Don't show tasks with "home" tag during day time
    (defun my/org-agenda-skip-home ()
      (let ((current-hour (string-to-number (format-time-string "%H"))))
        (when (and (< 10 current-hour 18)
                   (member "home" (org-get-tags-at)))
          (or (outline-next-heading)
              (goto-char (point-max))))))
    (setq org-agenda-skip-function #'my/org-agenda-skip-home)


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
    (cl-loop repeat 4 do (execute-kbd-macro (kbd "r")) (other-window 1))
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
    (split-window-below)
    (org-agenda nil "W")
    (other-window 1)
    (org-agenda nil "E")
    (other-window 1)
    (split-window-below)
    (org-agenda nil "a")
    (other-window 1)
    (org-agenda nil "Q")
    (other-window 1)
    (shrink-window-if-larger-than-buffer)
    (other-window 2)
    (shrink-window-horizontally 10)
    (other-window 1)
    (shrink-window 15)
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
    (cancel-function-timers 'refresh-dashboard)
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
 '(epg-gpg-program "/usr/bin/gpg1")
 '(line-spacing 5)
 '(org-modules (quote (org-crypt org-habit org-mouse)))
 '(package-selected-packages
   (quote
    (transpose-frame tagedit slim-mode sass-mode rainbow-mode pug-mode org-present org-pomodoro log4e gntp org-download org-cliplink mmm-mode markdown-toc less-css-mode htmlize helm-css-scss helm-c-yasnippet haml-mode gnuplot gh-md fuzzy flyspell-correct-helm flyspell-correct eval-in-repl paredit emmet-mode company-web web-completion-data company-statistics auto-yasnippet auto-dictionary ac-ispell auto-complete web-mode scss-mode helm-org-rifle helm-company company yasnippet markdown-mode alert org-mime ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:foreground "#b58900" :underline nil)))))
