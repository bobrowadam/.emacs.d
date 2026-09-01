;;; buffer-read.el --- Bounded Emacs buffer reading -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'mentat-elisp-library)

(mentat-defun mentat-read-buffer
    (buffer &key (offset 1) (max-chars 50000))
  "Read bounded text from BUFFER starting at one-based character OFFSET.
BUFFER must name a live Emacs buffer.  MAX-CHARS is capped at 50000 so the
result fits the registered-call transport cap.  The result includes NEXT_OFFSET
when more text remains, allowing bounded sequential
reads without returning the complete buffer through the tool transport."
  (:execution sync :display "Read Buffer")
  (unless (and (stringp buffer) (not (string-empty-p buffer)))
    (user-error "BUFFER must be a non-empty buffer name"))
  (unless (and (integerp offset) (> offset 0))
    (user-error "OFFSET must be a positive integer"))
  (unless (and (integerp max-chars) (> max-chars 0))
    (user-error "MAX-CHARS must be a positive integer"))
  (let ((target (get-buffer buffer))
        (limit (min 50000 max-chars)))
    (unless target
      (user-error "No Emacs buffer named %s" buffer))
    (with-current-buffer target
      (save-restriction
        (widen)
        (let* ((total (buffer-size))
               (start (+ (point-min) (1- offset)))
               (available (<= start (point-max)))
               (finish (if available
                           (min (point-max) (+ start limit))
                         (point-max)))
               (content (if available
                            (buffer-substring-no-properties start finish)
                          ""))
               (truncated (and available (< finish (point-max)))))
          `((buffer . ,(buffer-name target))
            (offset . ,offset)
            (end_offset . ,(and (not (string-empty-p content))
                                (+ offset (length content) -1)))
            (next_offset . ,(and truncated (+ offset (length content))))
            (total_chars . ,total)
            (truncated . ,truncated)
            (content . ,content)))))))

(mentat-defun mentat-read-buffer-lines
    (buffer &key (start-line 1) end-line (max-chars 50000))
  "Read a bounded line range from a live Emacs BUFFER.
START-LINE and END-LINE are one-based and inclusive.  When END-LINE is nil,
read from START-LINE until MAX-CHARS or the end of the buffer.  MAX-CHARS is
capped at 50000 for the registered-call transport limit."
  (:execution sync :display "Read Buffer Lines")
  (unless (and (stringp buffer) (not (string-empty-p buffer)))
    (user-error "BUFFER must be a non-empty buffer name"))
  (unless (and (integerp start-line) (> start-line 0))
    (user-error "START-LINE must be a positive integer"))
  (when (and end-line
             (not (and (integerp end-line) (>= end-line start-line))))
    (user-error "END-LINE must be an integer greater than or equal to START-LINE"))
  (unless (and (integerp max-chars) (> max-chars 0))
    (user-error "MAX-CHARS must be a positive integer"))
  (let ((target (get-buffer buffer))
        (limit (min 50000 max-chars)))
    (unless target
      (user-error "No Emacs buffer named %s" buffer))
    (with-current-buffer target
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let* ((start (point))
                 (actual-start-line (line-number-at-pos start t))
                 (requested-finish
                  (if end-line
                      (save-excursion
                        (goto-char start)
                        (forward-line (1+ (- end-line start-line)))
                        (point))
                    (point-max)))
                 (finish (min requested-finish (+ start limit)))
                 (content (buffer-substring-no-properties start finish))
                 (truncated (< finish requested-finish)))
            `((buffer . ,(buffer-name target))
              (start_line . ,actual-start-line)
              (end_line . ,(and (not (string-empty-p content))
                                (line-number-at-pos (max start (1- finish)) t)))
              (truncated . ,truncated)
              (content . ,content))))))))

(provide 'buffer-read)
;;; buffer-read.el ends here
