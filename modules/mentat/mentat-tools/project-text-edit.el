;;; project-text-edit.el --- Exact project text edits -*- lexical-binding: t; -*-

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

(defun mentat-text-edit--result (file status)
  "Return a structured edit result for FILE with STATUS."
  `((file . ,file) (status . ,status)))

(mentat-defun mentat-text-edit-replace-once (file old new)
  "Replace the sole exact OLD occurrence in FILE with NEW.
Reject missing or ambiguous OLD text.  Do not write when OLD and NEW are equal."
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
          (write-region (point-min) (point-max) absolute nil 'silent)
          (mentat-text-edit--result file "updated"))))))

(mentat-defun mentat-text-edit-insert-after-once (file anchor text)
  "Insert TEXT after the sole exact ANCHOR occurrence in FILE.
Reject missing or ambiguous anchors.  Return unchanged when TEXT is already
present immediately after ANCHOR."
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
          (write-region (point-min) (point-max) absolute nil 'silent)
          (mentat-text-edit--result file "updated"))))))

(provide 'project-text-edit)
;;; project-text-edit.el ends here
