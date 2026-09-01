;;; file-read.el --- Bounded Emacs-native file reading -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'mentat-elisp-library)

(mentat-defun mentat-read-file
    (file &key (start-line 1) end-line (max-chars 50000))
  "Read bounded file text with a compact continuation notice.
FILE is resolved against `default-directory'.  START-LINE and END-LINE are
one-based and inclusive.  END-LINE defaults to the end of the file.  MAX-CHARS
is capped at 200000."
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
             (truncated (> (length text) limit))
             (content (if truncated (substring text 0 limit) text))
             (next-line
              (cond
               (truncated (+ start (cl-count ?\n content)))
               ((and selected-end (< selected-end total-lines))
                (1+ selected-end))))
             (notice
              (cond
               (truncated
                (format
                 "[Output truncated at %d characters. Continue with start-line=%d.]"
                 limit next-line))
               (next-line
                (format "[%d more lines. Continue with start-line=%d.]"
                        (- total-lines selected-end) next-line)))))
        (if notice
            (concat (string-remove-suffix "\n" content) "\n\n" notice)
          content)))))

(provide 'file-read)
;;; file-read.el ends here
