;;; project-text-edit.el --- Exact project text edits -*- lexical-binding: t; -*-

(require 'lisp-mode)
(require 'cl-lib)
(require 'subr-x)
(require 'mentat-elisp-library)

(defun mentat-text-edit--exact-range (text)
  "Return the sole exact match range for TEXT in the current buffer."
  (unless (and (stringp text) (not (string-empty-p text)))
    (user-error "Match text must be nonempty"))
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (user-error "Expected text was not found"))
  (let ((range (cons (- (point) (length text)) (point))))
    (goto-char (1+ (car range)))
    (when (search-forward text nil t)
      (user-error "Expected text occurs more than once"))
    range))

(defun mentat-text-edit--elisp-diagnostics (file)
  "Return post-write parenthesis diagnostics for Elisp FILE in this buffer."
  (when (string-equal (file-name-extension file) "el")
    (condition-case err
        (progn
          (delay-mode-hooks (emacs-lisp-mode))
          (goto-char (point-min))
          (check-parens)
          "Elisp parentheses check passed.")
      (error
       (format "Elisp parentheses check failed at line %d, column %d: %s"
               (line-number-at-pos) (current-column)
               (error-message-string err))))))

(defun mentat-text-edit--result (file status &optional count diagnostics)
  "Return an edit result for FILE with STATUS, COUNT, and DIAGNOSTICS."
  (let ((summary
         (if count
             (format "%s %s (%d replacements)." status file count)
           (format "%s %s." status file))))
    (if diagnostics
        (concat summary "\n" diagnostics)
      summary)))

(defun mentat-text-edit--write-result (absolute file status &optional count)
  "Write the current buffer to ABSOLUTE and report FILE, STATUS, and COUNT."
  (write-region (point-min) (point-max) absolute nil 'silent)
  (mentat-text-edit--result
   file status count (mentat-text-edit--elisp-diagnostics absolute)))

(mentat-defun mentat-text-edit-replace-once (file old new)
  "Replace the sole exact OLD occurrence in FILE with NEW.
Reject missing or ambiguous OLD text.  Do not write when OLD and NEW are equal.
Check balanced parentheses after writing Elisp files.
Run compilation and documentation checks separately when the change is complete."
  (:display "Replace Once")
  (unless (and (stringp new) (file-regular-p file))
    (user-error "FILE must be a regular file and NEW must be a string"))
  (let ((absolute (expand-file-name file)))
    (with-temp-buffer
      (insert-file-contents absolute)
      (pcase-let ((`(,start . ,end) (mentat-text-edit--exact-range old)))
        (if (equal old new)
            (mentat-text-edit--result file "unchanged")
          (delete-region start end)
          (goto-char start)
          (insert new)
          (mentat-text-edit--write-result absolute file "updated"))))))

(mentat-defun mentat-text-edit-replace-many (file replacements)
  "Apply exact REPLACEMENTS to FILE atomically.
REPLACEMENTS is a nonempty JSON array of `[old, new]' string pairs.
Every old value must be nonempty and occur exactly once in the original file.
Reject overlap and write only after every replacement validates.
Check balanced parentheses after writing Elisp files.
Run compilation and documentation checks separately when the change is complete."
  (:display "Replace Many"
   :arguments
   ((file "File path")
    (replacements
     "Nonempty array of [old, new] string pairs; old must be nonempty")))
  (unless (file-regular-p file)
    (user-error "FILE must be a regular file"))
  (unless (consp replacements)
    (user-error
     "Invalid REPLACEMENTS. Try again with [[\"old text\", \"new text\"]]"))
  (let ((absolute (expand-file-name file)))
    (with-temp-buffer
      (insert-file-contents absolute)
      (let ((ranges
             (mapcar
              (lambda (replacement)
                (unless (and (listp replacement)
                             (= (length replacement) 2)
                             (stringp (car replacement))
                             (not (string-empty-p (car replacement)))
                             (stringp (cadr replacement)))
                  (user-error
                   "Invalid replacement. Try again with [[\"old text\", \"new text\"]]"))
                (let ((old (car replacement))
                      (new (cadr replacement)))
                  (pcase-let ((`(,start . ,end)
                               (mentat-text-edit--exact-range old)))
                    (list start end old new))))
              replacements)))
        (let ((ordered (sort (copy-sequence ranges)
                             (lambda (left right) (< (car left) (car right)))))
              previous)
          (dolist (range ordered)
            (when (and previous (< (car range) (cadr previous)))
              (user-error "Replacement ranges overlap"))
            (setq previous range)))
        (let ((changed
               (cl-remove-if
                (lambda (range) (equal (nth 2 range) (nth 3 range)))
                ranges)))
          (if (null changed)
              (mentat-text-edit--result file "Unchanged")
            (dolist (range
                     (sort changed (lambda (left right)
                                     (> (car left) (car right)))))
              (delete-region (car range) (cadr range))
              (goto-char (car range))
              (insert (nth 3 range)))
            (mentat-text-edit--write-result
             absolute file "Updated" (length changed))))))))

(mentat-defun mentat-text-edit-insert-after-once (file anchor text)
  "Insert TEXT after the sole exact ANCHOR occurrence in FILE.
Reject missing or ambiguous anchors.  Return unchanged when TEXT is already
present immediately after ANCHOR.  Check balanced parentheses after writing
Elisp files.  Run compilation and documentation checks separately when the
change is complete."
  (:display "Insert After Once")
  (unless (and (stringp text) (file-regular-p file))
    (user-error "FILE must be a regular file and TEXT must be a string"))
  (let ((absolute (expand-file-name file)))
    (with-temp-buffer
      (insert-file-contents absolute)
      (pcase-let ((`(,_start . ,end) (mentat-text-edit--exact-range anchor)))
        (goto-char end)
        (if (or (string-empty-p text)
                (and (<= (+ (point) (length text)) (point-max))
                     (equal text
                            (buffer-substring-no-properties
                             (point) (+ (point) (length text))))))
            (mentat-text-edit--result file "unchanged")
          (insert text)
          (mentat-text-edit--write-result absolute file "updated"))))))

(provide 'project-text-edit)
;;; project-text-edit.el ends here
