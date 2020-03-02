(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
        (let* ((case-fold-search t)
               (file-extension-re (image-file-name-regexp))
               (link-abbrevs (mapcar #'car
                                     (append org-link-abbrev-alist-local
                                             org-link-abbrev-alist)))
               ;; Check absolute, relative file names and explicit
               ;; "file:" links.  Also check link abbreviations since
               ;; some might expand to "file" links.
               (file-types-re
                (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
                        (if (not link-abbrevs) ""
                          (concat "\\|" (regexp-opt link-abbrevs))))))
          (while (re-search-forward file-types-re end t)
            (let* ((link (org-element-lineage
                          (save-match-data (org-element-context))
                          '(link) t))
                   (linktype (org-element-property :type link))
                   (inner-start (match-beginning 1))
                   (path
                    (cond
                     ;; No link at point; no inline image.
                     ((not link) nil)
                     ;; File link without a description.  Also handle
                     ;; INCLUDE-LINKED here since it should have
                     ;; precedence over the next case.  I.e., if link
                     ;; contains filenames in both the path and the
                     ;; description, prioritize the path only when
                     ;; INCLUDE-LINKED is non-nil.
                     ((or (not (org-element-property :contents-begin link))
                          include-linked)
                      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
                           (org-element-property :path link)))
                     ;; Link with a description.  Check if description
                     ;; is a filename.  Even if Org doesn't have syntax
                     ;; for those -- clickable image -- constructs, fake
                     ;; them, as in `org-export-insert-image-links'.
                     ((not inner-start) nil)
                     (t
                      (org-with-point-at inner-start
                        (and (looking-at
                              (if (char-equal ?< (char-after inner-start))
                                  org-link-angle-re
                                org-link-plain-re))
                             ;; File name must fill the whole
                             ;; description.
                             (= (org-element-property :contents-end link)
                                (match-end 0))
                             (match-string 2)))))))
              (when (and path (string-match-p file-extension-re path))
                (let ((file (if (equal "attachment" linktype)
                                (progn
                                  (require 'org-attach)
                                  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
                  (when (and file (file-exists-p file))
                    (let ((width
                           ;; Apply `org-image-actual-width' specifications.
                           (cond

                            ;; if org-image-actual-width is true, return nil
                            ((eq org-image-actual-width t) nil)

                            ((listp org-image-actual-width)
                             (or
                              ;; First try to find a width among
                              ;; attributes associated to the paragraph
                              ;; containing link.
                              (pcase (org-element-lineage link '(paragraph))
                                (`nil nil)
                                (p
                                 (let* ((case-fold-search t)
                                        (end (org-element-property :post-affiliated p))
                                        (re "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"))
                                   (when (org-with-point-at
                                             (org-element-property :begin p)
                                           (re-search-forward re end t))
                                     (string-to-number (match-string 1))))))
                              ;; Otherwise, fall-back to provided number.
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
                                                                (max (min min-width min-width-via-height org-image-actual-width) 10)))

			                      (t nil)))

			                    (old (get-char-property-and-overlay
				                        (org-element-property :begin link)
				                        'org-image-overlay)))
		                  (if (and (car-safe old) refresh)
			                    (image-refresh (overlay-get (cdr old) 'display))
			                  (let ((image (create-image file
						                                       (and (image-type-available-p 'imagemagick)
							                                          width 'imagemagick)
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
			                        (when (<= 26 emacs-major-version)
				                        (cl-assert (boundp 'image-map))
				                        (overlay-put ov 'keymap image-map))
			                        (push ov org-inline-image-overlays))))))))))))))))
