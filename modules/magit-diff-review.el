;;; magit-diff-review.el --- Track reviewed files in Magit diff buffers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'magit-diff)
(require 'sqlite)

(defgroup bob/magit-review nil
  "Track reviewed files in Magit diff buffers."
  :group 'magit-diff)

(defcustom bob/magit-review-database-file
  (expand-file-name "magit-diff-review.sqlite" user-emacs-directory)
  "SQLite file used to persist Magit diff review state."
  :type 'file
  :group 'bob/magit-review)

(defface bob/magit-review-heading
  '((t :inherit magit-diff-file-heading :foreground "gray50" :weight normal))
  "Face for reviewed file headings in Magit diff."
  :group 'bob/magit-review)

(defvar bob/magit-review--state (make-hash-table :test 'equal)
  "Hash-table of review-key -> hash-table of filepath -> t.")

(defvar bob/magit-review--db nil
  "SQLite handle for persisted Magit diff review state.")

(defconst bob/magit-review--checkmark
  (propertize " ✓" 'face '(:foreground "#50a050" :weight bold)))

(defun bob/magit-review--normalize-files (files)
  "Return a deterministic representation of FILES."
  (when files
    (sort (copy-sequence files) #'string<)))

(defun bob/magit-review--diff-kind ()
  "Return a keyword describing the current diff kind."
  (cond
   (magit-buffer-diff-range-oids :range)
   (magit-buffer-diff-range :range)
   ((equal magit-buffer-diff-typearg "--cached") :staged)
   (t :unstaged)))

(defun bob/magit-review--key-data ()
  "Return the canonical review identity for the current buffer."
  (list :repo (or (magit-toplevel) default-directory)
        :kind (bob/magit-review--diff-kind)
        :typearg magit-buffer-diff-typearg
        :oids (and magit-buffer-diff-range-oids
                   (copy-sequence magit-buffer-diff-range-oids))
        :range (unless magit-buffer-diff-range-oids
                 magit-buffer-diff-range)
        :files (bob/magit-review--normalize-files magit-buffer-diff-files)))

(defun bob/magit-review--key ()
  "Derive the review state key for the current buffer."
  (prin1-to-string (bob/magit-review--key-data)))

(defun bob/magit-review--db ()
  "Return the SQLite connection for review state, creating it if needed."
  (unless (sqlite-available-p)
    (user-error "SQLite support is not available in this Emacs build"))
  (unless bob/magit-review--db
    (make-directory (file-name-directory bob/magit-review-database-file) t)
    (setq bob/magit-review--db
          (sqlite-open bob/magit-review-database-file nil))
    (sqlite-execute
     bob/magit-review--db
     "CREATE TABLE IF NOT EXISTS review_entries (
        review_key TEXT NOT NULL,
        file_path TEXT NOT NULL,
        reviewed_at INTEGER NOT NULL,
        PRIMARY KEY (review_key, file_path)
      )"))
  bob/magit-review--db)

(defun bob/magit-review--close-db ()
  "Close the review state database if it is open."
  (when (sqlitep bob/magit-review--db)
    (sqlite-close bob/magit-review--db))
  (setq bob/magit-review--db nil))

(defun bob/magit-review--load-reviewed-set (key)
  "Load reviewed files for KEY from persistent storage."
  (let ((reviewed (make-hash-table :test 'equal)))
    (dolist (row (sqlite-select
                  (bob/magit-review--db)
                  "SELECT file_path FROM review_entries WHERE review_key = ?"
                  (vector key)))
      (puthash (car row) t reviewed))
    reviewed))

(defun bob/magit-review--persist-file-state (key filepath reviewed-p)
  "Persist REVIEWED-P for FILEPATH under KEY."
  (if reviewed-p
      (sqlite-execute
       (bob/magit-review--db)
       "INSERT INTO review_entries (review_key, file_path, reviewed_at)
        VALUES (?, ?, strftime('%s', 'now'))
        ON CONFLICT(review_key, file_path)
        DO UPDATE SET reviewed_at = excluded.reviewed_at"
       (vector key filepath))
    (sqlite-execute
     (bob/magit-review--db)
     "DELETE FROM review_entries WHERE review_key = ? AND file_path = ?"
     (vector key filepath))))

(defun bob/magit-review--clear-persisted-state (key)
  "Delete all persisted review state for KEY."
  (sqlite-execute
   (bob/magit-review--db)
   "DELETE FROM review_entries WHERE review_key = ?"
   (vector key)))

(defun bob/magit-review--reviewed-set ()
  "Get or create the reviewed file set for the current buffer."
  (let ((key (bob/magit-review--key)))
    (or (gethash key bob/magit-review--state)
        (puthash key (bob/magit-review--load-reviewed-set key)
                 bob/magit-review--state))))

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
      (let* ((key (bob/magit-review--key))
             (reviewed (bob/magit-review--reviewed-set))
             (reviewed-p (not (gethash filepath reviewed))))
        (if reviewed-p
            (puthash filepath t reviewed)
          (remhash filepath reviewed))
        (bob/magit-review--persist-file-state key filepath reviewed-p)
        (bob/magit-review--apply-overlays)
        ;; Collapse the current file diff and advance to next file section
        (when reviewed-p
          (when-let* ((section (bob/magit-review--file-section filepath)))
            (magit-section-hide section)
            ;; (goto-char (oref section end))
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
    (remhash key bob/magit-review--state)
    (bob/magit-review--clear-persisted-state key))
  (bob/magit-review--remove-overlays)
  (magit-refresh)
  (message "Review marks cleared"))

(defun bob/magit-review-difftastic-at-point ()
  "Show difftastic structural diff for the file at point."
  (interactive)
  (if-let* ((filepath (bob/magit-review--file-at-point)))
      (let* ((range (or magit-buffer-diff-range-oids
                        magit-buffer-diff-range))
             (args (when (equal magit-buffer-diff-typearg "--cached")
                     (list "--cached"))))
        (difftastic-git-diff-range range args (list filepath)))
    (user-error "No file at point")))

(keymap-set magit-file-section-map "v" #'bob/magit-review-toggle)
(keymap-set magit-hunk-section-map "v" #'bob/magit-review-toggle)
(keymap-set magit-file-section-map "M-d" #'bob/magit-review-difftastic-at-point)
(keymap-set magit-hunk-section-map "M-d" #'bob/magit-review-difftastic-at-point)
(keymap-set magit-diff-mode-map "C-c C-v" #'bob/magit-review-clear)

(add-hook 'magit-post-refresh-hook #'bob/magit-review--apply-overlays)
(add-hook 'kill-emacs-hook #'bob/magit-review--close-db)

(provide 'magit-diff-review)
;;; magit-diff-review.el ends here
