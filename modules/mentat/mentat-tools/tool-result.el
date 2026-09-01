;;; tool-result.el --- Shared handling for large tool results -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defconst mentat-tool-result-default-max-bytes 50000
  "Default maximum UTF-8 byte size of inline tool-result text.")

(defconst mentat-tool-result-default-max-lines 2000
  "Default maximum number of inline tool-result lines.")

(defun mentat-tool-result-decode-utf8 (text)
  "Decode unibyte UTF-8 TEXT and leave decoded multibyte text unchanged."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (if (multibyte-string-p text)
      text
    (decode-coding-string text 'utf-8 t)))

(defun mentat-tool-result--utf8-bytes (text)
  "Return the number of bytes needed to encode TEXT as UTF-8."
  (string-bytes (encode-coding-string text 'utf-8)))

(defun mentat-tool-result--line-count (text)
  "Return the number of logical lines in TEXT."
  (with-temp-buffer
    (insert text)
    (count-lines (point-min) (point-max))))

(defun mentat-tool-result--byte-prefix-length (text max-bytes)
  "Return the largest character prefix of TEXT within MAX-BYTES UTF-8 bytes."
  (if (<= (mentat-tool-result--utf8-bytes text) max-bytes)
      (length text)
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (mentat-tool-result--utf8-bytes
                   (substring text 0 middle))
                  max-bytes)
              (setq low middle)
            (setq high (1- middle)))))
      low)))

(defun mentat-tool-result--line-prefix-length (text max-lines)
  "Return the character length of the first MAX-LINES lines of TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (forward-line max-lines)
    (- (point) (point-min))))

(defun mentat-tool-result-persist-text (text &optional prefix)
  "Save complete TEXT in a private temporary file and return its path.
PREFIX identifies the producing tool in the temporary directory name."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (let* ((label (replace-regexp-in-string
                 "[^[:alnum:]-]+" "-" (or prefix "tool-result")))
         (directory (make-temp-file (concat "mentat-" label "-") t))
         (file (expand-file-name "output.txt" directory))
         (coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)
    (set-file-modes file #o600)
    file))

(cl-defun mentat-tool-result-limit-text
    (text &key
          (max-bytes mentat-tool-result-default-max-bytes)
          (max-lines mentat-tool-result-default-max-lines)
          (prefix "tool-result"))
  "Return bounded TEXT metadata, persisting the full text when truncated.
MAX-BYTES counts the UTF-8 representation.  MAX-LINES limits logical lines.
PREFIX identifies the producer in the temporary output path."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (unless (and (integerp max-bytes) (> max-bytes 0))
    (user-error "Tool-result max-bytes must be a positive integer"))
  (unless (and (integerp max-lines) (> max-lines 0))
    (user-error "Tool-result max-lines must be a positive integer"))
  (let* ((total-characters (length text))
         (total-bytes (mentat-tool-result--utf8-bytes text))
         (total-lines (mentat-tool-result--line-count text))
         (byte-length (mentat-tool-result--byte-prefix-length text max-bytes))
         (line-length (mentat-tool-result--line-prefix-length text max-lines))
         (visible-length (min byte-length line-length))
         (truncated (< visible-length total-characters))
         (visible (if truncated (substring text 0 visible-length) text)))
    `((content . ,visible)
      (truncated . ,(if truncated t :json-false))
      (total-characters . ,total-characters)
      (total-bytes . ,total-bytes)
      (total-lines . ,total-lines)
      ,@(when truncated
          `((omitted-characters . ,(- total-characters visible-length))
            (omitted-bytes . ,(- total-bytes
                                 (mentat-tool-result--utf8-bytes visible)))
            (omitted-lines . ,(max 0 (- total-lines
                                         (mentat-tool-result--line-count visible))))
            (full-output-file . ,(mentat-tool-result-persist-text
                                  text prefix)))))))

(provide 'tool-result)
;;; tool-result.el ends here
