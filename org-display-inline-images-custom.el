(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.  BEG and END default to the buffer
boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let* ((case-fold-search t)
            (file-extension-re (image-file-name-regexp))
            (link-abbrevs (mapcar #'car
                                  (append org-link-abbrev-alist-local
                                          org-link-abbrev-alist)))
            ;; Check absolute, relative file names and explicit
            ;; "file:" links.  Also check link abbreviations since
            ;; some might expand to "file" links.
            (file-types-re (format "[][]\\[\\(?:file\\|[./~]%s\\)"
                                   (if (not link-abbrevs) ""
                                     (format "\\|\\(?:%s:\\)"
                                             (regexp-opt link-abbrevs))))))
       (while (re-search-forward file-types-re end t)
         (let ((link (save-match-data (org-element-context))))
           ;; Check if we're at an inline image, i.e., an image file
           ;; link without a description (unless INCLUDE-LINKED is
           ;; non-nil).
           (when (and (equal "file" (org-element-property :type link))
                      (or include-linked
                          (null (org-element-contents link)))
                      (string-match-p file-extension-re
                                      (org-element-property :path link)))
             (let ((file (expand-file-name
                          (org-link-unescape
                           (org-element-property :path link)))))
               (when (file-exists-p file)
                 (let ((width
                        ;; Apply `org-image-actual-width' specifications.
                        (cond

                         ;; return nil if image type is not available
                         ((not (image-type-available-p 'imagemagick)) nil)

                         ;; if org-image-actual-width is true, return nil
                         ((eq org-image-actual-width t) nil)

                         ((listp org-image-actual-width)
                          (or
                           ;; First try to find a width among
                           ;; attributes associated to the paragraph
                           ;; containing link.
                           (let ((paragraph
                                  (let ((e link))
                                    (while (and (setq e (org-element-property
                                                         :parent e))
                                                (not (eq (org-element-type e)
                                                         'paragraph))))
                                    e)))
                             (when paragraph
                               (save-excursion
                                 (goto-char (org-element-property :begin paragraph))
                                 (when
                                     (re-search-forward
                                      "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                                      (org-element-property
                                       :post-affiliated paragraph)
                                      t)
                                   (string-to-number (match-string 1))))))

                           (car org-image-actual-width)))

                         ((numberp org-image-actual-width) (let* (
                                                                  ;; get the image dimensions
                                                                  (this-image-dimensions (image-size (create-image file) t nil))
                                                                  (actual-width (car this-image-dimensions))
                                                                  (actual-height (cdr this-image-dimensions))
                                                                  ;; calculate the aspect ratio
                                                                  (this-aspect-ratio (/ (float actual-width) (float actual-height)))
                                                                  ;; get the window width
                                                                  (this-window-width (- (window-pixel-width) 50))
                                                                  ;; get the window height
                                                                  (this-window-height (* (window-pixel-height) 0.5))
                                                                  ;; minimum of image width and window width
                                                                  (min-width (min actual-width this-window-width))
                                                                  ;; minimum of image height and window height
                                                                  (min-height (min actual-height this-window-height))
                                                                  ;; conserve aspect ratio
                                                                  (min-width-via-height (truncate (* (float min-height) this-aspect-ratio)))
                                                                  )
                                                             (max (min min-width min-width-via-height org-image-actual-width) 10)))))

                       (old (get-char-property-and-overlay
                             (org-element-property :begin link)
                             'org-image-overlay)))

                   (if (and (car-safe old) refresh)
                       (image-refresh (overlay-get (cdr old) 'display))
                     (let ((image (create-image file
                                                (and width 'imagemagick)
                                                nil
                                                :width width)))
                       (when image
                         (let ((ov (make-overlay
                                    (org-element-property :begin link)
                                    (progn
                                      (goto-char
                                       (org-element-property :end link))
                                      (skip-chars-backward " \t")
                                      (point)))))
                           (overlay-put ov 'display image)
                           (overlay-put ov 'face 'default)
                           (overlay-put ov 'org-image-overlay t)
                           (overlay-put
                            ov 'modification-hooks
                            (list 'org-display-inline-remove-overlay))
                           (push ov org-inline-image-overlays)))))))))))))))
