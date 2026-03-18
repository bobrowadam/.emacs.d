;;; magit-diff-review.el --- Track reviewed files in Magit diff buffers -*- lexical-binding: t; -*-

(require 'magit-diff)

(defface bob/magit-review-heading
  '((t :inherit magit-diff-file-heading :foreground "gray50" :weight normal))
  "Face for reviewed file headings in Magit diff.")

(defvar bob/magit-review--state (make-hash-table :test 'equal)
  "Hash-table of review-key -> hash-table of filepath -> t.")

(defconst bob/magit-review--checkmark
  (propertize " ✓" 'face '(:foreground "#50a050" :weight bold)))

(defun bob/magit-review--key ()
  "Derive the review state key for the current buffer."
  (format "%s:%s"
          (or (magit-toplevel) default-directory)
          (or magit-buffer-diff-range (buffer-name))))

(defun bob/magit-review--reviewed-set ()
  "Get or create the reviewed file set for the current buffer."
  (let ((key (bob/magit-review--key)))
    (or (gethash key bob/magit-review--state)
        (puthash key (make-hash-table :test 'equal) bob/magit-review--state))))

(defun bob/magit-review--file-at-point ()
  "Return the file path from the magit file section at point.
Walks up from hunk to parent file section if needed."
  (when-let* ((section (magit-current-section)))
    (cond
     ((magit-file-section-p section)
      (oref section value))
     ((magit-hunk-section-p section)
      (when-let* ((parent (oref section parent)))
        (when (magit-file-section-p parent)
          (oref parent value)))))))

(defun bob/magit-review--file-section (filepath)
  "Find the magit file section for FILEPATH in the current buffer."
  (when (bound-and-true-p magit-root-section)
    (seq-find (lambda (s)
                (and (magit-file-section-p s)
                     (equal (oref s value) filepath)))
              (oref magit-root-section children))))

(defun bob/magit-review--apply-overlay (section)
  "Apply a reviewed overlay to SECTION's heading line."
  (let* ((start (oref section start))
         (content (or (oref section content) (oref section end)))
         (ovl (make-overlay start content)))
    (overlay-put ovl 'magit-review t)
    (overlay-put ovl 'face 'bob/magit-review-heading)
    (overlay-put ovl 'after-string bob/magit-review--checkmark)
    ovl))

(defun bob/magit-review--remove-overlays ()
  "Remove all review overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'magit-review t))

(defun bob/magit-review--apply-overlays ()
  "Re-apply review overlays for all reviewed files in the current buffer."
  (when (derived-mode-p 'magit-diff-mode)
    (bob/magit-review--remove-overlays)
    (let ((reviewed (bob/magit-review--reviewed-set)))
      (when (bound-and-true-p magit-root-section)
        (dolist (section (oref magit-root-section children))
          (when (and (magit-file-section-p section)
                     (gethash (oref section value) reviewed))
            (bob/magit-review--apply-overlay section)))))
    (bob/magit-review--update-header-line)))

(defun bob/magit-review--file-count ()
  "Return (reviewed . total) file count for the current buffer."
  (let ((reviewed (bob/magit-review--reviewed-set))
        (total 0))
    (when (bound-and-true-p magit-root-section)
      (dolist (section (oref magit-root-section children))
        (when (magit-file-section-p section)
          (cl-incf total))))
    (cons (hash-table-count reviewed) total)))

(defun bob/magit-review--update-header-line ()
  "Append review progress to the header-line."
  (when (derived-mode-p 'magit-diff-mode)
    (let* ((counts (bob/magit-review--file-count))
           (reviewed (car counts))
           (total (cdr counts)))
      (when (> reviewed 0)
        (setq header-line-format
              (list (if (stringp header-line-format)
                        header-line-format
                      (or (car-safe header-line-format) ""))
                    (propertize (format "  [%d/%d reviewed]" reviewed total)
                                'face (if (= reviewed total)
                                          '(:foreground "#50a040" :weight bold)
                                        '(:foreground "#b0a030")))))))))

(defun bob/magit-review-toggle ()
  "Toggle reviewed mark on the file at point, then advance to next file."
  (interactive)
  (if-let* ((filepath (bob/magit-review--file-at-point)))
      (let ((reviewed (bob/magit-review--reviewed-set)))
        (if (gethash filepath reviewed)
            (remhash filepath reviewed)
          (puthash filepath t reviewed))
        (bob/magit-review--apply-overlays)
        ;; Advance to next file section
        (when (gethash filepath reviewed)
          (when-let* ((section (bob/magit-review--file-section filepath)))
            (goto-char (oref section end))
            (when-let* ((next (magit-current-section)))
              (when (or (magit-file-section-p next)
                        (magit-hunk-section-p next))
                (goto-char (oref (if (magit-file-section-p next)
                                     next
                                   (oref next parent))
                                 start)))))))
    (user-error "No file at point")))

(defun bob/magit-review-clear ()
  "Clear all review marks for the current diff."
  (interactive)
  (let ((key (bob/magit-review--key)))
    (remhash key bob/magit-review--state))
  (bob/magit-review--remove-overlays)
  (bob/magit-review--update-header-line)
  (setq header-line-format (magit-set-header-line-format
                            (magit-buffer-diff-range)))
  (message "Review marks cleared"))

(keymap-set magit-file-section-map "v" #'bob/magit-review-toggle)
(keymap-set magit-hunk-section-map "v" #'bob/magit-review-toggle)
(keymap-set magit-diff-mode-map "C-c C-v" #'bob/magit-review-clear)

(add-hook 'magit-post-refresh-hook #'bob/magit-review--apply-overlays)

(provide 'magit-diff-review)
;;; magit-diff-review.el ends here
