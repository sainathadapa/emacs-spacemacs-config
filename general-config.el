;; Appearance
;; Disabling bent arrows (↩)

;; [[file:general-config.org::*Appearance][Appearance:1]]
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
;; Appearance:1 ends here

;; Backup and other file settings
;; Store all backup and autosave files in the ~/tmp~ directory

;; [[file:general-config.org::*Backup and other file settings][Backup and other file settings:1]]
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; Backup and other file settings:1 ends here



;; Backup by copying instead of renaming. See https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup-Copying.html

;; [[file:general-config.org::*Backup and other file settings][Backup and other file settings:2]]
(setq backup-by-copying t)
;; Backup and other file settings:2 ends here



;; Automatic deletion of backups. See https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup-Deletion.html

;; [[file:general-config.org::*Backup and other file settings][Backup and other file settings:3]]
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;; Backup and other file settings:3 ends here



;; Do not create "lock" files.

;; [[file:general-config.org::*Backup and other file settings][Backup and other file settings:4]]
(setq create-lockfiles nil)
;; Backup and other file settings:4 ends here



;; Save files when emacs goes out of focus

;; [[file:general-config.org::*Backup and other file settings][Backup and other file settings:5]]
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)
;; Backup and other file settings:5 ends here



;; Load any changes from disk automatically

;; [[file:general-config.org::*Backup and other file settings][Backup and other file settings:6]]
(setq global-auto-revert-mode t)
;; Backup and other file settings:6 ends here

;; Miscellaneous
;; Answer with y/n instead of having to write the full words

;; [[file:general-config.org::*Miscellaneous][Miscellaneous:1]]
(fset 'yes-or-no-p 'y-or-n-p)
;; Miscellaneous:1 ends here



;; Default browser to use

;; [[file:general-config.org::*Miscellaneous][Miscellaneous:2]]
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open")
;; Miscellaneous:2 ends here

;; [[file:general-config.org::*Miscellaneous][Miscellaneous:3]]
(with-eval-after-load 'helm
  (setq helm-display-function 'helm-default-display-buffer)) ;; temp work around
;; Miscellaneous:3 ends here

;; [[file:general-config.org::*Miscellaneous][Miscellaneous:4]]
(setq history-delete-duplicates t)
;; Miscellaneous:4 ends here

;; Org config
;; Spacemacs requires the org-mode settings to be defined after the org-mode is activated.

;; [[file:general-config.org::*Org config][Org config:1]]
(with-eval-after-load 'org
  (require 'org-config)
)
;; Org config:1 ends here



;; Default file to open

;; [[file:general-config.org::*Org config][Org config:2]]
(find-file "~/Dropbox/org/main.org")
;; Org config:2 ends here

;; Disabled

;; [[file:general-config.org::*Disabled][Disabled:1]]
;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; (require 'python) ; if not done elsewhere
;; (require 'eval-in-repl-python)
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))

;; consider *.org.txt files as org files
;; (add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

;; (require 'helm-bookmark)
;; Disabled:1 ends here

;; Final
;; Let the Spacemacs use this configuration.

;; [[file:general-config.org::*Final][Final:1]]
(provide 'general-config)
;; Final:1 ends here
