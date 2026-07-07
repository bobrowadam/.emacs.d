;;; pi-emacs.el --- Emacs helpers for Pi -*- lexical-binding: t; -*-

;;; Commentary:
;; Elisp helpers used by the pi-coding-agent Emacs extension.
;; Provides context extraction, diagnostics, and file navigation.

;;; Code:

(require 'project)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'help-fns)
(require 'find-func)
(require 'info-look)

(declare-function magit-commit-create "magit-commit" (&optional args))
(declare-function magit-process-error-summary "magit-process" (process-buf section))
(declare-function magit-refresh-all "magit-mode" ())

(defun pi/encode-result (text)
  "Encode TEXT as base64 UTF-8 for transport."
  (base64-encode-string (encode-coding-string text 'utf-8) t))

(defun pi/--backtrace-string ()
  "Return the current backtrace as a string."
  (with-output-to-string
    (backtrace)))

(defun pi/--blocked-interactive-prompt (&rest _args)
  "Fail instead of blocking Emacs on an interactive prompt."
  (user-error "Pi Emacs tool blocked an interactive prompt"))

(defun pi/--format-tool-error (err backtrace messages)
  "Format ERR with BACKTRACE and captured MESSAGES."
  (let ((sections (list (format "Error: %s" (error-message-string err)))))
    (when messages
      (setq sections
            (append sections
                    (list (concat "Messages/Warnings:\n"
                                  (string-join
                                   (delete-dups (nreverse messages))
                                   "\n"))))))
    (setq sections (append sections (list (concat "Backtrace:\n" backtrace))))
    (string-join sections "\n\n")))

(defmacro pi/with-tool-safety (&rest body)
  "Run BODY without interactive prompts and return errors with backtraces."
  (declare (indent 0) (debug t))
  `(let ((debug-on-error nil)
         (debug-on-quit nil)
         (pi/--captured-backtrace nil)
         (pi/--tool-messages nil)
         (pi/--original-signal (symbol-function 'signal))
         (pi/--original-message (symbol-function 'message))
         (pi/--original-display-warning (symbol-function 'display-warning)))
     (cl-letf (((symbol-function 'signal)
                (lambda (error-symbol data)
                  (setq pi/--captured-backtrace (pi/--backtrace-string))
                  (funcall pi/--original-signal error-symbol data)))
               ((symbol-function 'message)
                (lambda (format-string &rest args)
                  (when format-string
                    (let ((text (apply #'format-message format-string args)))
                      (unless (string-empty-p text)
                        (push (format "message: %s" text) pi/--tool-messages))))
                  (apply pi/--original-message format-string args)))
               ((symbol-function 'display-warning)
                (lambda (type warning-message &optional level buffer-name)
                  (push (format "warning[%s]: %s" type warning-message)
                        pi/--tool-messages)
                  (funcall pi/--original-display-warning
                           type warning-message level buffer-name)))
               ((symbol-function 'yes-or-no-p) #'pi/--blocked-interactive-prompt)
               ((symbol-function 'y-or-n-p) #'pi/--blocked-interactive-prompt)
               ((symbol-function 'read-answer) #'pi/--blocked-interactive-prompt)
               ((symbol-function 'read-from-minibuffer) #'pi/--blocked-interactive-prompt)
               ((symbol-function 'read-string) #'pi/--blocked-interactive-prompt)
               ((symbol-function 'read-buffer) #'pi/--blocked-interactive-prompt)
               ((symbol-function 'read-file-name) #'pi/--blocked-interactive-prompt))
       (condition-case err
           (progn ,@body)
         ((error quit)
          (pi/encode-result
           (pi/--format-tool-error
            err
            (or pi/--captured-backtrace (pi/--backtrace-string))
            pi/--tool-messages)))))))

;; --- Pulse region (non-invasive visual feedback after pi edits) ---

(defvar pulse-delay)
(defvar pulse-iterations)

(defun pi/pulse-region (file start-line end-line)
  "Pulse-highlight lines START-LINE to END-LINE in FILE.
Called by pi after it edits a region, to give visual feedback.

Non-invasive by design: only pulses if FILE is already visible in
some window on some frame.  Does NOT visit the file, steal focus,
raise frames, rearrange windows, or move point.  If the buffer is
not currently displayed, this silently does nothing and returns
a JSON note explaining why."
  (require 'json)
  (require 'pulse)
  (let* ((buf (find-buffer-visiting file))
         (win (and buf (get-buffer-window buf t))))
    (cond
     ((not buf)
      (json-encode '(("pulsed" . :false) ("reason" . "file not visited"))))
     ((not (window-live-p win))
      (json-encode '(("pulsed" . :false) ("reason" . "buffer not visible"))))
     (t
      (save-selected-window
        (with-current-buffer buf
          (save-excursion
            (let* ((pulse-delay 0.02)
                   (pulse-iterations 60)
                   (beg (progn (goto-char (point-min))
                               (forward-line (1- start-line))
                               (point)))
                   (end (progn (goto-char (point-min))
                               (forward-line (1- end-line))
                               (end-of-line)
                               (point)))
                   (text (buffer-substring-no-properties beg end)))
              (pulse-momentary-highlight-region beg end)
              (json-encode `(("pulsed" . t) ("pulsed text" . ,text)))))))))))

;; --- Clipboard image paste into pi's input buffer ---

(defvar pi/--image-counter 0
  "Counter used to generate unique temp image filenames.")

(defun pi/paste-image ()
  "Paste the image on the macOS clipboard into the pi input buffer.
Saves it to a temp PNG via pngpaste and inserts `@path ' at point so
pi picks it up as a file reference."
  (interactive)
  (unless (executable-find "pngpaste")
    (user-error "pngpaste not found — install with: brew install pngpaste"))
  (let* ((dir (temporary-file-directory))
         (name (format "pi-image-%s-%d.png"
                       (format-time-string "%Y%m%d-%H%M%S")
                       (cl-incf pi/--image-counter)))
         (path (expand-file-name name dir)))
    (unless (zerop (call-process "pngpaste" nil nil nil path))
      (user-error "No image on clipboard"))
    (insert "@" path " ")))

(with-eval-after-load 'pi-coding-agent-input
  (define-key pi-coding-agent-input-mode-map (kbd "C-c i") #'pi/paste-image))

(defun pi/open-file (path line)
  "Open PATH and jump to LINE."
  (let ((buffer (find-file-noselect path)))
    (select-frame-set-input-focus (selected-frame))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (set-window-point (selected-window) (point))
      (recenter))))

(defun pi/open-new-buffer-with-content (text &optional name mode)
  "Open a new buffer and insert TEXT.

Optional NAME controls the buffer base name (defaults to `*pi-copy*`).
Optional MODE is a major mode function (symbol) to enable in the buffer."
  (let* ((base-name (or name "*pi-copy*"))
         (buffer (generate-new-buffer base-name)))
    (select-frame-set-input-focus (selected-frame))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (when (and mode (fboundp mode))
        (funcall mode))
      (set-buffer-modified-p nil))
    (buffer-name buffer)))

;;; --- Agent-facing Elisp inspection helpers ---

(defvar pi/--named-elisp-scripts (make-hash-table :test 'equal)
  "Named Elisp snippets kept in the live Emacs server runtime.")

(defun pi/--eval-elisp-code (code)
  "Evaluate Elisp CODE and return the last form's raw result."
  (let (form result)
    (with-temp-buffer
      (insert code)
      (goto-char (point-min))
      (while (progn
               (skip-chars-forward " \t\n\r")
               (not (eobp)))
        (setq form (read (current-buffer)))
        (setq result (eval form t))))
    result))

(defun pi/--check-elisp-code (name code)
  "Byte-compile check named Elisp CODE for NAME and return warning text."
  (let* ((raw-safe-name (replace-regexp-in-string "[^[:alnum:]_.-]" "-" name))
         (safe-name (if (> (length raw-safe-name) 48)
                        (substring raw-safe-name 0 48)
                      raw-safe-name))
         (source-file (make-temp-file (concat "pi-named-elisp-" safe-name "-") nil ".el"))
         (compiled-file (concat source-file "c"))
         (log-name (generate-new-buffer-name " *pi-named-elisp-check*"))
         (byte-compile-log-buffer log-name)
         (byte-compile-dest-file-function (lambda (_source) compiled-file))
         (byte-compile-error-on-warn nil)
         (byte-compile-warnings t))
    (unwind-protect
        (condition-case err
            (progn
              (with-temp-file source-file
                (insert ";;; pi named elisp check -*- lexical-binding: t; -*-\n")
                (insert code)
                (unless (string-suffix-p "\n" code)
                  (insert "\n")))
              (unless (byte-compile-file source-file)
                (user-error "byte-compile-file returned nil"))
              (let ((log-buffer (get-buffer log-name))
                    (warning-lines nil))
                (when (buffer-live-p log-buffer)
                  (with-current-buffer log-buffer
                    (dolist (line (split-string (buffer-string) "\n"))
                      (let ((trimmed-line (string-trim line)))
                        (unless (or (string-empty-p trimmed-line)
                                    (string= trimmed-line "")
                                    (string-prefix-p "Compiling file " trimmed-line)
                                    (string-prefix-p "Entering directory " trimmed-line)
                                    (string-prefix-p "Leaving directory " trimmed-line))
                          (push line warning-lines))))))
                (string-trim (string-join (nreverse warning-lines) "\n"))))
          (error
           (user-error "Named Elisp check failed: %s" (error-message-string err))))
      (when (file-exists-p source-file)
        (delete-file source-file))
      (when (file-exists-p compiled-file)
        (delete-file compiled-file))
      (let ((log-buffer (get-buffer log-name)))
        (when (buffer-live-p log-buffer)
          (kill-buffer log-buffer))))))

(defun pi/eval-elisp (code)
  "Evaluate Elisp CODE in the running Emacs and return base64 result text."
  (pi/with-tool-safety
    (unless (stringp code)
      (user-error "Invalid Elisp code"))
    (pi/encode-result
     (prin1-to-string (pi/--eval-elisp-code code)))))

(defun pi/eval-named-elisp (name code)
  "Define or rerun named Elisp snippet NAME and return base64 result text.
When CODE is non-empty, store it under NAME before executing it.
When CODE is empty, rerun the existing snippet stored under NAME."
  (pi/with-tool-safety
    (unless (stringp name)
      (user-error "Invalid named Elisp name"))
    (unless (stringp code)
      (user-error "Invalid named Elisp code"))
    (let* ((trimmed-name (string-trim name))
           (has-code (not (string-empty-p (string-trim code)))))
      (when (string-empty-p trimmed-name)
        (user-error "Named Elisp name cannot be empty"))
      (let ((check-warnings ""))
        (when has-code
          (setq check-warnings (pi/--check-elisp-code trimmed-name code))
          (puthash trimmed-name code pi/--named-elisp-scripts))
        (let* ((stored-code (gethash trimmed-name pi/--named-elisp-scripts))
               (result-text nil))
          (unless stored-code
            (user-error "No named Elisp snippet stored as %s" trimmed-name))
          (setq result-text
                (format "%s %s%s => %s"
                        (if has-code "Defined and executed" "Executed")
                        trimmed-name
                        (if has-code " (checked)" "")
                        (prin1-to-string (pi/--eval-elisp-code stored-code))))
          (pi/encode-result
           (if (and has-code (not (string-empty-p check-warnings)))
               (format "%s\n\nElisp check warnings:\n%s" result-text check-warnings)
             result-text)))))))

(defun pi/eval-elisp-file (path)
  "Load Elisp file PATH in the running Emacs and return base64 result text."
  (pi/with-tool-safety
    (unless (and (stringp path) (file-readable-p path))
      (user-error "Elisp file is not readable: %s" path))
    (pi/encode-result
     (format "Loaded %s => %S" path (load-file path)))))

(defun pi/--json-bool (value)
  "Return VALUE as an Emacs JSON boolean."
  (if value t :json-false))

(defun pi/--validate-symbol-name (name kind &optional intern-p)
  "Validate NAME as a symbol name for KIND.
When INTERN-P is non-nil, return the interned symbol."
  (unless (stringp name)
    (user-error "Invalid %s name" kind))
  (when (string-empty-p name)
    (user-error "Empty %s name" kind))
  (when intern-p
    (intern name)))

(defun pi/elisp-describe-function (function)
  "Return base64-encoded documentation for Emacs Lisp FUNCTION."
  (pi/with-tool-safety
    (let ((sym (pi/--validate-symbol-name function "function" t)))
      (unless (fboundp sym)
        (user-error "Function %s is void" function))
      (pi/encode-result
       (with-temp-buffer
         (let ((standard-output (current-buffer)))
           (describe-function-1 sym)
           (buffer-string)))))))

(defun pi/--function-definition-bounds ()
  "Return source bounds around the function definition at point."
  (save-excursion
    (unless (looking-at-p "(\\s-*\\(?:cl-\\)?def")
      (beginning-of-defun))
    (let ((start (point)))
      (save-excursion
        (end-of-defun)
        (cons start (point))))))

(defun pi/elisp-get-function-definition (function)
  "Return base64-encoded JSON source location for Emacs Lisp FUNCTION."
  (pi/with-tool-safety
    (let ((sym (pi/--validate-symbol-name function "function" t)))
      (unless (fboundp sym)
        (user-error "Function %s is not found" function))
      (condition-case err
          (pcase-let* ((`(,buffer . ,pos) (find-function-noselect sym))
                       (file (or (buffer-file-name buffer) "<interactively defined>")))
            (with-current-buffer buffer
              (save-excursion
                (goto-char pos)
                (pcase-let* ((`(,start . ,end) (pi/--function-definition-bounds))
                             (source (buffer-substring-no-properties start end)))
                  (pi/encode-result
                   (json-encode
                    `((source . ,source)
                      (file-path . ,file)
                      (start-line . ,(line-number-at-pos start))
                      (end-line . ,(line-number-at-pos end)))))))))
        (error
         (pi/encode-result
          (json-encode
           `((found . :json-false)
             (function . ,function)
             (message . ,(error-message-string err))
             (backtrace . ,(pi/--backtrace-string))))))))))

(defun pi/elisp-describe-variable (variable)
  "Return base64-encoded JSON metadata for Emacs Lisp VARIABLE.
Does not expose the variable's value."
  (pi/with-tool-safety
    (let* ((sym (pi/--validate-symbol-name variable "variable" t))
           (doc (documentation-property sym 'variable-documentation t))
           (file (find-lisp-object-file-name sym 'defvar))
           (custom-p (custom-variable-p sym))
           (custom-group (get sym 'custom-group))
           (obsolete (get sym 'byte-obsolete-variable)))
      (unless (or (boundp sym) doc file custom-p obsolete)
        (user-error "Variable %s is not defined" variable))
      (pi/encode-result
       (json-encode
        `((name . ,variable)
          (bound . ,(pi/--json-bool (boundp sym)))
          (value-type . ,(when (boundp sym) (symbol-name (type-of (symbol-value sym)))))
          (documentation . ,doc)
          (source-file . ,(or file "<interactively defined>"))
          (is-custom . ,(pi/--json-bool custom-p))
          (custom-group . ,(when custom-group (symbol-name custom-group)))
          (custom-type . ,(when custom-p (format "%S" (get sym 'custom-type))))
          (is-obsolete . ,(pi/--json-bool obsolete))
          (obsolete-info . ,(when obsolete (format "%S" obsolete)))))))))

(defun pi/--clean-info-content (content)
  "Clean Info navigation markup from CONTENT while preserving prose."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (re-search-forward "\\*[Nn]ote[ \n][^:]*::" nil t)
      (replace-match "[See: \\&]"))
    (buffer-string)))

(defun pi/--extract-info-node-content ()
  "Extract the current Info node content without its header."
  (let (start end)
    (goto-char (point-min))
    (when (re-search-forward "^File: [^,]+,  Node: [^,\n]+.*\n" nil t)
      (setq start (point)))
    (when start
      (goto-char start)
      (setq end (if (re-search-forward "^\^_" nil t)
                    (match-beginning 0)
                  (point-max))))
    (when (and start end)
      (pi/--clean-info-content
       (buffer-substring-no-properties start end)))))

(defun pi/elisp-info-lookup-symbol (symbol)
  "Return base64-encoded JSON Info documentation lookup for SYMBOL."
  (pi/with-tool-safety
    (pi/--validate-symbol-name symbol "symbol")
    (let (result)
      (condition-case nil
          (with-temp-buffer
            (let ((mode 'emacs-lisp-mode)
                  info-buf node manual content)
              (emacs-lisp-mode)
              (save-window-excursion
                (info-lookup-symbol symbol mode)
                (setq info-buf (get-buffer "*info*"))
                (when info-buf
                  (with-current-buffer info-buf
                    (goto-char (point-min))
                    (when (re-search-forward "^File: \\([^,]+\\),  Node: \\([^,\n]+\\)" nil t)
                      (setq manual (match-string 1))
                      (setq node (match-string 2))
                      (when (string-match "\\.info\\'" manual)
                        (setq manual (substring manual 0 (match-beginning 0)))))
                    (setq content (pi/--extract-info-node-content)))))
              (when (and node content)
                (setq result
                      `((found . t)
                        (symbol . ,symbol)
                        (node . ,node)
                        (manual . ,manual)
                        (content . ,content)
                        (info-ref . ,(format "(%s)%s" manual node)))))))
        (error nil))
      (pi/encode-result
       (json-encode
        (or result
            `((found . :json-false)
              (symbol . ,symbol)
              (message . ,(format "Symbol '%s' not found in Elisp Info documentation" symbol)))))))))

(defun pi/get-context ()
  "Return base64-encoded context for the current buffer."
  (with-current-buffer (window-buffer (selected-window))
    (let* ((buffer-name (buffer-name))
           (file (buffer-file-name))
           (mode (symbol-name major-mode))
           (line (line-number-at-pos))
           (text
            (if (use-region-p)
                (let* ((beg (region-beginning))
                       (end (region-end))
                       (start-line (line-number-at-pos beg))
                       (end-line (line-number-at-pos end))
                       (body (buffer-substring-no-properties beg end)))
                  (format
                   "buffer-name:%s\n%smode:%s\nline:%d\nselection-start-line:%d\nselection-end-line:%d\n---\n%s"
                   buffer-name
                   (if file (format "file:%s\n" file) "")
                   mode
                   line
                   start-line
                   end-line
                   body))
              (let* ((total (count-lines (point-min) (point-max)))
                     (start (max 1 (- line 15)))
                     (end (min total (+ line 15)))
                     (body (save-excursion
                             (goto-char (point-min))
                             (forward-line (1- start))
                             (let ((beg (point)))
                               (forward-line (- end start -1))
                               (buffer-substring-no-properties beg (point))))))
                (format
                 "buffer-name:%s\n%smode:%s\nline:%d\ntotal-lines:%d\nexcerpt-start-line:%d\n---\n%s"
                 buffer-name
                 (if file (format "file:%s\n" file) "")
                 mode
                 line
                 total
                 start
                 body)))))
      (pi/encode-result text))))

(defun pi/get-diagnostics ()
  "Return base64-encoded diagnostics for the current buffer."
  (with-current-buffer (window-buffer (selected-window))
    (let* ((file (or (buffer-file-name) "(no file)"))
           (text
            (cond
             ((and (boundp 'eglot--managed-mode) eglot--managed-mode)
              (let ((diags (flymake-diagnostics)))
                (if diags
                    (mapconcat
                     (lambda (d)
                       (format "%s:%d:%d [%s] %s"
                               file
                               (line-number-at-pos (flymake-diagnostic-beg d))
                               0
                               (flymake-diagnostic-type d)
                               (flymake-diagnostic-text d)))
                     diags
                     "\n")
                  (format "no diagnostics for %s" file))))
             ((and (boundp 'flymake-mode) flymake-mode)
              (let ((diags (flymake-diagnostics)))
                (if diags
                    (mapconcat
                     (lambda (d)
                       (format "%s:%d:%d [%s] %s"
                               file
                               (line-number-at-pos (flymake-diagnostic-beg d))
                               0
                               (flymake-diagnostic-type d)
                               (flymake-diagnostic-text d)))
                     diags
                     "\n")
                  (format "no diagnostics for %s" file))))
             ((and (boundp 'flycheck-mode) flycheck-mode (boundp 'flycheck-current-errors))
              (let ((errs flycheck-current-errors))
                (if errs
                    (mapconcat
                     (lambda (e)
                       (format "%s:%d:%d [%s] %s"
                               (or (flycheck-error-filename e) file)
                               (flycheck-error-line e)
                               (or (flycheck-error-column e) 0)
                               (flycheck-error-level e)
                               (flycheck-error-message e)))
                     errs
                     "\n")
                  (format "no diagnostics for %s" file))))
             (t "no diagnostics backend active in this buffer (eglot, flymake, or flycheck)"))))
      (pi/encode-result text))))

(defun pi/screenshot-target ()
  "Return metadata for screenshotting the selected Emacs frame."
  (require 'json)
  (let* ((frame (selected-frame))
         (window (frame-selected-window frame))
         (buffer (window-buffer window))
         (file (buffer-file-name buffer))
         (project-root (when (fboundp 'project-current)
                         (when-let* ((project (project-current nil (buffer-local-value 'default-directory buffer))))
                           (expand-file-name (project-root project))))))
    (pi/encode-result
     (json-encode
      `((frameName . ,(or (frame-parameter frame 'name) ""))
        (bufferName . ,(buffer-name buffer))
        (majorMode . ,(symbol-name (buffer-local-value 'major-mode buffer)))
        (file . ,file)
        (project . ,project-root))))))

(defun pi/get-snapshot ()
  "Return base64-encoded lightweight snapshot of the current buffer."
  (with-current-buffer (window-buffer (selected-window))
    (let* ((buffer-name (buffer-name))
           (file (buffer-file-name))
           (project (project-current nil))
           (project-root (when project
                           (expand-file-name (project-root project))))
           (relative-file (when (and project-root (buffer-file-name))
                            (file-relative-name (buffer-file-name) project-root)))
           (mode (symbol-name major-mode))
           (line (line-number-at-pos))
           (window-start-line (line-number-at-pos (window-start)))
           (window-end-line (line-number-at-pos (window-end (selected-window) t)))
           (diags (cond
                   ((or (and (boundp 'eglot--managed-mode) eglot--managed-mode)
                        (and (boundp 'flymake-mode) flymake-mode))
                    (flymake-diagnostics))
                   ((and (boundp 'flycheck-mode) flycheck-mode (boundp 'flycheck-current-errors))
                    flycheck-current-errors)
                   (t nil)))
           (diagnostic-count (length diags))
           (error-count 0)
           (warning-count 0)
           (note-count 0))
      (dolist (diag diags)
        (cond
         ((memq (if (fboundp 'flymake-diagnostic-type)
                    (flymake-diagnostic-type diag)
                  (when (fboundp 'flycheck-error-level)
                    (flycheck-error-level diag)))
                '(:error error eglot-error))
          (setq error-count (1+ error-count)))
         ((memq (if (fboundp 'flymake-diagnostic-type)
                    (flymake-diagnostic-type diag)
                  (when (fboundp 'flycheck-error-level)
                    (flycheck-error-level diag)))
                '(:warning warning eglot-warning))
          (setq warning-count (1+ warning-count)))
         ((memq (if (fboundp 'flymake-diagnostic-type)
                    (flymake-diagnostic-type diag)
                  (when (fboundp 'flycheck-error-level)
                    (flycheck-error-level diag)))
                '(:note note :info info eglot-note))
          (setq note-count (1+ note-count)))))
      (pi/encode-result
       (string-join
        (delq nil
              (list
               (format "buffer-name:%s" buffer-name)
               (when file (format "file:%s" file))
               (when relative-file (format "relative-file:%s" relative-file))
               (when project-root (format "project-root:%s" project-root))
               (format "mode:%s" mode)
               (format "line:%d" line)
               (when (use-region-p)
                 (format "selection-start-line:%d"
                         (line-number-at-pos (region-beginning))))
               (when (use-region-p)
                 (format "selection-end-line:%d"
                         (line-number-at-pos (region-end))))
               (format "window-start-line:%d" window-start-line)
               (format "window-end-line:%d" window-end-line)
               (format "diagnostic-count:%d" diagnostic-count)
               (format "error-count:%d" error-count)
               (format "warning-count:%d" warning-count)
               (format "note-count:%d" note-count)))
        "\n")))))
(defconst pi/--response-buffer " *pi-response*"
  "Buffer name used for the pi response posframe.")

(defun pi/--response-visible-p ()
  "Return non-nil if the pi response posframe is visible."
  (and (get-buffer pi/--response-buffer)
       (get-buffer-window pi/--response-buffer t)))

(defun pi/show-response (file line text)
  "Show TEXT in a posframe near LINE in FILE.
Press q or Escape to dismiss."
  (require 'posframe)
  (let* ((buf (or (find-buffer-visiting file)
                  (find-file-noselect file)))
         (win (or (get-buffer-window buf t)
                  (display-buffer buf)))
         (content (concat "Pi:\n" text)))
    (with-selected-window win
      (with-current-buffer buf
        (goto-char (point-min))
        (forward-line (1- line))
        (posframe-show pi/--response-buffer
                       :string content
                       :position (point)
                       :poshandler #'posframe-poshandler-point-top-left-corner
                       :border-width 1
                       :border-color (face-foreground 'shadow nil t)
                       :background-color (face-background 'tooltip nil t)
                       :foreground-color (face-foreground 'default nil t)
                       :internal-border-width 8
                       :max-width (max 40 (/ (window-width win) 2))
                       :min-width 20)
        (add-hook 'post-command-hook #'pi/--auto-dismiss-response nil t)))))

(defun pi/--auto-dismiss-response ()
  "Dismiss posframe after the next command."
  (posframe-delete pi/--response-buffer)
  (remove-hook 'post-command-hook #'pi/--auto-dismiss-response t))

(defun pi/dismiss-response ()
  "Dismiss the pi response posframe."
  (interactive)
  (posframe-delete pi/--response-buffer))

(defun pi/dismiss-all-responses ()
  "Dismiss all pi response posframes."
  (interactive)
  (posframe-delete pi/--response-buffer))



;;; --- Unix socket client for Emacs → Pi communication ---

(defconst pi/socket-name-basename-length 24
  "Maximum basename prefix length used in Pi socket filenames.")

(defconst pi/socket-name-hash-length 16
  "Hex digest length used in Pi socket filenames.")

(defun pi/encode-cwd (cwd)
  "Encode CWD to match Pi's hashed session socket naming."
  (let* ((dir (directory-file-name (expand-file-name cwd)))
         (base (file-name-nondirectory dir))
         (safe-base (replace-regexp-in-string
                     "^-+\\|-+$" ""
                     (replace-regexp-in-string "[^[:alnum:]._-]+" "-" base)))
         (safe-base (if (string-empty-p safe-base) "cwd" safe-base))
         (safe-base (if (> (length safe-base) pi/socket-name-basename-length)
                        (substring safe-base 0 pi/socket-name-basename-length)
                      safe-base))
         (hash (substring (secure-hash 'sha256 dir) 0 pi/socket-name-hash-length)))
    (format "%s-%s" safe-base hash)))

(defun pi/legacy-encode-cwd (cwd)
  "Encode CWD using the legacy full-path socket naming scheme."
  (let ((s (replace-regexp-in-string "^[/\\]" "" cwd)))
    (concat "--" (replace-regexp-in-string "[/\\:]" "-" s) "--")))

(defun pi/socket-dir ()
  "Return the directory containing Pi Unix sockets."
  (expand-file-name "sockets" "~/.pi"))

(defun pi/worktree-socket-dir (root)
  "Return the worktree-index socket directory for ROOT."
  (expand-file-name
   (concat "by-worktree/" (pi/encode-cwd root))
   (pi/socket-dir)))

(defun pi/worktree-socket-candidates (root)
  "Return newest worktree-index socket symlinks for ROOT."
  (let ((dir (pi/worktree-socket-dir root)))
    (when (file-directory-p dir)
      (mapcar
       #'car
       (sort
        (seq-filter
         (lambda (entry)
           (and (string-suffix-p ".sock" (car entry))
                (file-exists-p (car entry))))
         (directory-files-and-attributes dir t "\\.sock\\'" nil 'integer))
        (lambda (a b)
          (time-less-p (file-attribute-modification-time (cdr b))
                       (file-attribute-modification-time (cdr a)))))))))

(defun pi/socket-root (&optional cwd)
  "Return the root directory used for Pi socket discovery."
  (directory-file-name
   (expand-file-name
    (or cwd
        (when-let* ((project (project-current nil))
                    (root (project-root project)))
          root)
        (and (fboundp 'bob/monorepo-root)
             (bob/monorepo-root))
        default-directory))))

(defun pi/socket-path (&optional cwd)
  "Return the primary Unix socket path for CWD."
  (let* ((dir (pi/socket-root cwd))
         (encoded (pi/encode-cwd dir)))
    (expand-file-name (concat encoded ".sock")
                      (pi/socket-dir))))

(defun pi/socket-paths-for-dir (dir)
  "Return hashed and legacy socket paths for DIR."
  (mapcar (lambda (encoded)
            (expand-file-name (concat encoded ".sock") (pi/socket-dir)))
          (list (pi/encode-cwd dir) (pi/legacy-encode-cwd dir))))

(defun pi/socket-ancestor-dirs (&optional cwd)
  "Return CWD's ancestor dirs from closest to Git root/project root."
  (let* ((start (directory-file-name
                 (expand-file-name (or cwd default-directory))))
         (project-root (pi/socket-root cwd))
         (git-root (when-let* ((dir (locate-dominating-file start ".git")))
                     (directory-file-name (expand-file-name dir))))
         (stop (or git-root project-root))
         dirs
         parent)
    (while (and start (not (member start dirs)))
      (setq dirs (append dirs (list start)))
      (setq parent (directory-file-name (file-name-directory start)))
      (setq start (unless (or (string= start stop)
                              (string= parent start))
                    parent)))
    (delete-dups (append dirs (delq nil (list project-root git-root))))))

(defun pi/socket-path-candidates (&optional cwd)
  "Return candidate Unix socket paths for CWD.
Prefer the closest parent Pi session, then farther parent sessions."
  (let ((dirs (pi/socket-ancestor-dirs cwd))
        direct-paths
        direct-basenames
        candidates)
    (dolist (dir dirs)
      (dolist (path (pi/socket-paths-for-dir dir))
        (unless (member path direct-paths)
          (setq direct-paths (append direct-paths (list path))))))
    (setq direct-basenames (mapcar #'file-name-nondirectory direct-paths))
    (dolist (path direct-paths)
      (unless (member path candidates)
        (setq candidates (append candidates (list path)))))
    (dolist (dir dirs)
      (dolist (path (pi/worktree-socket-candidates dir))
        (when (and (member (file-name-nondirectory path) direct-basenames)
                   (not (member path candidates)))
          (setq candidates (append candidates (list path))))))
    candidates))

(defun pi/send (message &optional cwd)
  "Send MESSAGE (a plist) as newline-delimited JSON to Pi's socket.
Optional CWD overrides the project root for socket discovery."
  (let* ((sock-paths (pi/socket-path-candidates cwd))
         (json (json-encode message))
         (payload (concat json "\n"))
         last-error sent)
    (dolist (sock-path sock-paths)
      (when (and (not sent) (file-exists-p sock-path))
        (condition-case err
            (let ((proc (make-network-process
                         :name "pi-send"
                         :family 'local
                         :service t
                         :remote sock-path
                         :noquery t)))
              (process-send-string proc payload)
              (delete-process proc)
              (setq sent t))
          (file-error
           (setq last-error err)
           (ignore-errors (delete-file sock-path))))))
    (unless sent
      (if last-error
          (user-error "Pi socket unavailable: %s" (error-message-string last-error))
        (user-error "Pi socket not found: %s" (string-join sock-paths ", "))))))

;;; --- Magit integration ---

(defconst pi/magit-commit-running-threshold-seconds 120
  "Seconds before Pi reports that a Magit commit is still running.")

(when (and (fboundp 'pi/magit--commit-process-finish-advice)
           (advice-member-p #'pi/magit--commit-process-finish-advice
                            'magit-process-finish))
  (advice-remove 'magit-process-finish
                 #'pi/magit--commit-process-finish-advice))

(defun pi/magit--buffer-tail (buffer &optional max-lines)
  "Return the tail of BUFFER, keeping at most MAX-LINES lines."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (forward-line (- (or max-lines 200)))
        (string-trim
         (buffer-substring-no-properties (point) (point-max)))))))

(defun pi/magit--strip-control-sequences (text)
  "Remove ANSI/control sequences from TEXT."
  (when text
    (let ((clean text))
      (setq clean (replace-regexp-in-string "\r" "\n" clean))
      (setq clean (replace-regexp-in-string "\x1b\[[0-9;?]*[ -/]*[@-~]" "" clean))
      (setq clean (replace-regexp-in-string "\x1b\][^\a]*\a" "" clean))
      (string-trim clean))))

(defun pi/magit--string-tail (text &optional max-lines)
  "Return the tail of TEXT, keeping at most MAX-LINES lines."
  (when (and text (not (string-empty-p text)))
    (with-temp-buffer
      (insert text)
      (pi/magit--buffer-tail (current-buffer) max-lines))))

(defun pi/magit--process-command-string (process)
  "Return PROCESS command as a shell-escaped string, or nil."
  (when-let* ((command (process-command process)))
    (string-join (mapcar #'shell-quote-argument command) " ")))

(defun pi/magit--process-duration (process)
  "Return elapsed seconds for PROCESS, or nil if unavailable."
  (when-let* ((start-time (process-get process 'pi-magit-start-time)))
    (max 0.0 (- (float-time) start-time))))

(defun pi/magit--process-error-summary (process)
  "Return Magit's structured error summary for PROCESS, if available."
  (let* ((process-buffer (process-buffer process))
         (section (process-get process 'section)))
    (and (buffer-live-p process-buffer)
         section
         (with-current-buffer process-buffer
           (magit-process-error-summary process-buffer section)))))

(defun pi/magit--detect-hook-hints (text)
  "Return a list of likely hook-related hints from TEXT."
  (let ((haystack (downcase (or text "")))
        hints)
    (when (string-match-p "lefthook" haystack)
      (push "Lefthook output detected" hints))
    (when (string-match-p "pre-commit" haystack)
      (push "A pre-commit hook appears to be running" hints))
    (when (or (string-match-p "npm warn exec" haystack)
              (string-match-p "will be installed" haystack)
              (string-match-p "\\bnpx\\b" haystack))
      (push "npm/npx warmup or package resolution may be delaying the hook" hints))
    (delete-dups (nreverse hints))))

(defun pi/magit--run-git-diagnostic (root &rest args)
  "Run `git ARGS' in ROOT and return a plist with command, exit, and output."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory (expand-file-name root)))
           (command (string-join
                     (cons "git" (mapcar #'shell-quote-argument args))
                     " "))
           (exit (apply #'process-file "git" nil (current-buffer) nil args))
           (output (pi/magit--strip-control-sequences (buffer-string))))
      (list :command command
            :exit exit
            :output output))))

(defun pi/magit--format-git-diagnostic (diagnostic &optional max-lines)
  "Format DIAGNOSTIC plist, keeping at most MAX-LINES of output."
  (let ((command (plist-get diagnostic :command))
        (exit (plist-get diagnostic :exit))
        (output (pi/magit--string-tail (plist-get diagnostic :output) max-lines)))
    (string-join
     (delq nil
           (list (and command (format "$ %s" command))
                 (format "exit %s" exit)
                 (and output (not (string-empty-p output)) output)))
     "\n")))

(defun pi/magit--process-report-body (process)
  "Return a plist with summary, tail, and hook hints for PROCESS."
  (let* ((summary (pi/magit--strip-control-sequences
                   (pi/magit--process-error-summary process)))
         (tail (pi/magit--strip-control-sequences
                (pi/magit--buffer-tail (process-buffer process) 200)))
         (combined (string-join (delq nil (list summary tail)) "\n\n"))
         (hints (pi/magit--detect-hook-hints combined)))
    (list :summary summary
          :tail tail
          :hints hints)))

(defun pi/magit--format-process-outcome (process event)
  "Return a human-friendly outcome string for PROCESS and sentinel EVENT."
  (let ((status (process-status process))
        (code (process-exit-status process))
        (event-text (string-trim (or event ""))))
    (cond
     ((eq status 'signal)
      (format "terminated by signal %s%s"
              code
              (if (string-empty-p event-text)
                  ""
                (format " (%s)" event-text))))
     (t
      (format "exit %s%s"
              code
              (if (string-empty-p event-text)
                  ""
                (format " (%s)" event-text)))))))

(defun pi/magit--notify-commit-running (process root callback-cwd)
  "Notify Pi that Magit commit PROCESS in ROOT is still running."
  (when (and (process-live-p process)
             (not (process-get process 'pi-magit-running-notified)))
    (process-put process 'pi-magit-running-notified t)
    (let* ((duration (or (pi/magit--process-duration process) 0.0))
           (command (pi/magit--process-command-string process))
           (report (pi/magit--process-report-body process))
           (tail (plist-get report :tail))
           (hints (plist-get report :hints))
           (sections
            (delq nil
                  (list
                   (format "Magit commit still running in %s" root)
                   (format "State: still running after %.1fs" duration)
                   (and command (format "Command: %s" command))
                   (and hints
                        (concat "Possible blockers:\n- "
                                (string-join hints "\n- ")))
                   (and tail
                        (not (string-empty-p tail))
                        (concat "Recent output tail:\n" tail))
                   "The commit may still finish in Emacs; first-run hook installs or npx warmups can take a while."))))
      (ignore-errors
        (pi/send (list :type "input"
                       :text (string-join sections "\n\n")
                       :submit t)
                 callback-cwd)))))

(defun pi/magit--notify-commit-failure (process root callback-cwd event)
  "Notify Pi if Magit commit PROCESS failed in ROOT.
EVENT is the process sentinel event string."
  (when (and (processp process)
             (memq (process-status process) '(exit signal))
             (not (= (process-exit-status process) 0)))
    (let* ((duration (pi/magit--process-duration process))
           (command (pi/magit--process-command-string process))
           (report (pi/magit--process-report-body process))
           (summary (plist-get report :summary))
           (tail (plist-get report :tail))
           (hints (plist-get report :hints))
           (status-diagnostic (pi/magit--run-git-diagnostic root "status" "--short"))
           (dry-run-diagnostic (pi/magit--run-git-diagnostic root "commit" "--dry-run" "--verbose"))
           (sections
            (delq nil
                  (list
                   (format "Magit commit failed in %s" root)
                   (format "Result: %s" (pi/magit--format-process-outcome process event))
                   (and duration (format "Duration: %.1fs" duration))
                   (and command (format "Command: %s" command))
                   (and hints
                        (concat "Likely blockers:\n- "
                                (string-join hints "\n- ")))
                   (and summary
                        (not (string-empty-p summary))
                        (concat "Magit summary:\n" summary))
                   (and tail
                        (not (string-empty-p tail))
                        (concat "Process output tail:\n" tail))
                   (concat "Safe diagnostics:\n"
                           (pi/magit--format-git-diagnostic status-diagnostic 80)
                           "\n\n"
                           (pi/magit--format-git-diagnostic dry-run-diagnostic 120))))))
      (ignore-errors
        (pi/send (list :type "input"
                       :text (string-join sections "\n\n")
                       :submit t)
                 callback-cwd)))))

(defun pi/magit--refresh-after-commit (process root)
  "Refresh Magit buffers for ROOT after successful commit PROCESS."
  (when (and (eq (process-status process) 'exit)
             (= (process-exit-status process) 0)
             (file-directory-p root))
    (let ((default-directory (file-name-as-directory (expand-file-name root))))
      (ignore-errors
        (magit-refresh-all)))))

(defun pi/magit--watch-commit-process (process root callback-cwd)
  "Attach Pi status reporting to Magit commit PROCESS."
  (when (processp process)
    (let ((old-sentinel (process-sentinel process))
          (timer (run-at-time
                  pi/magit-commit-running-threshold-seconds nil
                  (lambda ()
                    (pi/magit--notify-commit-running process root callback-cwd)))))
      (process-put process 'pi-magit-start-time (float-time))
      (process-put process 'pi-magit-watchdog timer)
      (set-process-sentinel
       process
       (lambda (proc event)
         (when-let* ((watchdog (process-get proc 'pi-magit-watchdog)))
           (cancel-timer watchdog)
           (process-put proc 'pi-magit-watchdog nil))
         (when old-sentinel
           (condition-case err
               (funcall old-sentinel proc event)
             (error (message "Magit process sentinel error: %s"
                             (error-message-string err)))))
         (pi/magit--refresh-after-commit proc root)
         (pi/magit--notify-commit-failure proc root callback-cwd event))))))

(defun pi/magit-commit (git-root callback-cwd &optional message-base64)
  "Open Magit's commit editor for GIT-ROOT and report failures to CALLBACK-CWD.
Optional MESSAGE-BASE64 is decoded and passed to git as the initial message."
  (let* ((default-directory (file-name-as-directory (expand-file-name git-root)))
         (pi-commit-root (directory-file-name (expand-file-name git-root)))
         (message (and message-base64
                       (decode-coding-string
                        (base64-decode-string message-base64)
                        'utf-8))))
    (select-frame-set-input-focus (selected-frame))
    (require 'magit)
    (require 'magit-commit)
    (require 'magit-process)
    (pi/magit--watch-commit-process
     (magit-commit-create
      (when message
        (list "--edit" (concat "--message=" message))))
     pi-commit-root
     callback-cwd)
    (delete-other-windows)))

(defun pi/--language-from-mode ()
  "Return a short language tag for the current buffer's major mode, or nil.
Strips `-ts-mode' and `-mode' suffixes so e.g. `typescript-ts-mode' and
`python-mode' both become \"typescript\" / \"python\".  Only returns a
tag for `prog-mode' descendants so non-code buffers fall back to a
bare code fence on the pi side."
  (when (derived-mode-p 'prog-mode)
    (let* ((name (symbol-name major-mode))
           (stripped (cond
                      ((string-suffix-p "-ts-mode" name)
                       (substring name 0 (- (length name) (length "-ts-mode"))))
                      ((string-suffix-p "-mode" name)
                       (substring name 0 (- (length name) (length "-mode"))))
                      (t name))))
      (unless (string-empty-p stripped) stripped))))

(defun pi/send-text (text)
  "Send TEXT to Pi's input field."
  (pi/send `(:type "input" :text ,text)))

(defun pi/send-region (start end &optional text)
  "Send the region from START to END with optional TEXT question."
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos start))
         (selection (buffer-substring-no-properties start end))
         (language (pi/--language-from-mode))
         (msg (list :type "input" :selection selection)))
    (when file (setq msg (plist-put msg :file file)))
    (when line (setq msg (plist-put msg :line line)))
    (when language (setq msg (plist-put msg :language language)))
    (when text (setq msg (plist-put msg :text text)))
    (pi/send msg)))

(defun pi/ask (&optional text)
  "Ask Pi about current context and show response inline.
Sends file, line, selection (if active), and TEXT as a question.
Response appears as an overlay at the current line."
  (interactive "sAsk Pi: ")
  (let ((msg (list :type "ask"
                   :requestId (format "%s" (random 100000))))
        (language (pi/--language-from-mode)))
    (when (buffer-file-name)
      (setq msg (plist-put msg :file (buffer-file-name)))
      (setq msg (plist-put msg :line (line-number-at-pos))))
    (when (use-region-p)
      (setq msg (plist-put msg :selection
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))))
      (when language (setq msg (plist-put msg :language language))))
    (when (and text (not (string-empty-p text)))
      (setq msg (plist-put msg :text text)))
    (pi/send msg)
    (message "Asked Pi.")))

(defun pi/send-buffer-context (&optional text)
  "Send current buffer context (file, line, selection if active) to Pi.
Optional TEXT is appended as a question/instruction.
Marks the message for terminal focus and submit on the receiver side."
  (interactive "sMessage for Pi: ")
  (pi/send
   (thread-last
     (list :type "input" :focus t :submit t)
     (pi/--add-buffer-file)
     (pi/--add-selection-and-language)
     (pi/--add-text text)
     (pi/add-flymake-diagnostics))))

(defun bob/pi-send-buffer-context (&optional text)
  "Send current buffer context to Pi."
  (interactive "sMessage for Pi: ")
  (pi/send-buffer-context text))

(defun pi/--add-buffer-file (msg)
  "Add file and line number to MSG if buffer is visiting a file."
  (if (buffer-file-name)
      (let ((msg (plist-put msg :file (buffer-file-name))))
        (plist-put msg :line (line-number-at-pos)))
    msg))

(defun pi/--add-selection-and-language (msg)
  "Add selection and language to MSG if region is active."
  (if (use-region-p)
      (let* ((language (pi/--language-from-mode))
             (msg (plist-put msg :selection
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))))
        (plist-put msg :language language))
    msg))

(defun pi/--add-text (text msg)
  "Add TEXT to MSG if TEXT is non-empty."
  (if (and text (not (string-empty-p text)))
      (plist-put msg :text text)
    msg))

(defun pi/add-flymake-diagnostics (msg)
  "Add flymake diagnostics to MSG.
When region is active, only diagnostics for that region.
Otherwise, all file diagnostics."
  (let* ((diags (flymake-diagnostics))
         (filtered-diags
          (if (use-region-p)
              (let ((reg-start (region-beginning))
                    (reg-end (region-end)))
                (seq-filter
                 (lambda (d)
                   (let ((d-pos (flymake-diagnostic-beg d)))
                     (and (>= d-pos reg-start) (<= d-pos reg-end))))
                 diags))
            diags)))
    (if filtered-diags
        (plist-put msg :diagnostics
                   (vconcat
                    (mapcar
                     (lambda (d)
                       (list :line (line-number-at-pos (flymake-diagnostic-beg d))
                             :column (save-excursion
                                       (goto-char (flymake-diagnostic-beg d))
                                       (current-column))
                             :type (symbol-name (flymake-diagnostic-type d))
                             :message (flymake-diagnostic-text d)))
                     filtered-diags)))
      msg)))

(provide 'pi-emacs)
;;; pi-emacs.el ends here
