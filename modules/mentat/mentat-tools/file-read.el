;;; file-read.el --- Bounded Emacs-native file reading -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'mentat-elisp-library)

(mentat-defun mentat-read-file
    (file &key (start-line 1) end-line (max-chars 50000))
  "Read bounded text from FILE and return content with location metadata.
FILE is resolved against `default-directory'.  START-LINE and END-LINE are
one-based and inclusive.  END-LINE defaults to the end of the file.  MAX-CHARS
is capped at 200000; when the selected text exceeds it, CONTENT is truncated
and TRUNCATED is non-nil."
  (:execution sync :display "Read File")
  (let* ((absolute (expand-file-name file))
         (start (max 1 start-line))
         (limit (min 200000 (max 1 max-chars))))
    (unless (file-regular-p absolute)
      (user-error "Not a regular file: %s" absolute))
    (unless (file-readable-p absolute)
      (user-error "File is not readable: %s" absolute))
    (when (and end-line (< end-line start))
      (user-error "END-LINE must be at least START-LINE"))
    (with-temp-buffer
      (insert-file-contents absolute)
      (when (save-excursion (goto-char (point-min)) (search-forward "\0" nil t))
        (user-error "Refusing to read binary file: %s" absolute))
      (let* ((total-lines (count-lines (point-min) (point-max)))
             (selected-end (and (> total-lines 0)
                                (min (or end-line total-lines) total-lines)))
             (begin (progn
                      (goto-char (point-min))
                      (forward-line (1- start))
                      (point)))
             (finish (if selected-end
                         (progn
                           (goto-char (point-min))
                           (forward-line selected-end)
                           (point))
                       (point-min)))
             (text (if (or (> start total-lines) (= total-lines 0))
                       ""
                     (buffer-substring-no-properties begin finish)))
             (truncated (> (length text) limit)))
        `((file . ,absolute)
          (start_line . ,start)
          (end_line . ,(and (not (string-empty-p text)) selected-end))
          (total_lines . ,total-lines)
          (truncated . ,truncated)
          (content . ,(if truncated (substring text 0 limit) text)))))))

(provide 'file-read)
;;; file-read.el ends here
