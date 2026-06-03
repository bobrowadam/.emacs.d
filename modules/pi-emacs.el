;;; pi-emacs.el --- Emacs helpers for Pi -*- lexical-binding: t; -*-

;;; Commentary:
;; Elisp helpers used by the pi-coding-agent Emacs extension.
;; Provides context extraction, diagnostics, and file navigation.

;;; Code:

(require 'project)
(require 'json)
(require 'subr-x)

(defun pi/encode-result (text)
  "Encode TEXT as base64 UTF-8 for transport."
  (base64-encode-string (encode-coding-string text 'utf-8) t))

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
