;; -*- lexical-binding: t; -*-
;;
;; pi.el — Emacs integration for the Pi coding agent running in Kitty.
;;
;; Assumes the following helpers are defined in init.org:
;;   bob/monorepo-root, bob/kitten, bob/kitty-socket,
;;   bob/kitty-tab-exists-p, bob/kitty-focus-tab

(declare-function bob/monorepo-root "init")
(declare-function bob/kitten "init")
(declare-function bob/kitty-socket "init")
(declare-function bob/kitty-tab-exists-p "init")
(declare-function bob/kitty-focus-tab "init")

;;; Session tracking

(defvar bob/kitty-pi-tabs (make-hash-table :test 'equal)
  "Maps monorepo root dirs to Kitty window IDs for pi sessions.")

(defvar pulse-delay)
(defvar pulse-iterations)

;;; Kitty window ID lookup

(defun bob/kitty-pi-window-id (&optional dir)
  "Return the Kitty window ID for the pi session in DIR (or current project).
Searches the hash table first, then queries Kitty.  Returns a string or nil."
  (let* ((dir (expand-file-name (or dir (bob/monorepo-root))))
         (cached (gethash dir bob/kitty-pi-tabs)))
    (if (and cached (bob/kitty-tab-exists-p cached))
        cached
      (when-let* ((found (bob/kitty-find-pi-tab dir)))
        (puthash dir found bob/kitty-pi-tabs)
        found))))

;;; Open / focus

(defun bob/kitty-find-pi-tab (dir)
  "Find a Kitty tab running pi in DIR, return its active window ID
as string, or nil.  Prefers the focused window when multiple match."
  (condition-case nil
      (let ((os-windows (json-parse-string (bob/kitten "ls") :array-type 'list))
            (focused nil)
            (first-match nil))
        (dolist (os-win os-windows)
          (dolist (tab (gethash "tabs" os-win))
            (dolist (win (gethash "windows" tab))
              (when (cl-some (lambda (proc)
                               (and (string= (gethash "cwd" proc) (directory-file-name dir))
                                    (member "pi" (append (gethash "cmdline" proc) nil))))
                             (append (gethash "foreground_processes" win) nil))
                (let ((id (number-to-string (gethash "id" win))))
                  (unless first-match (setq first-match id))
                  (when (eq (gethash "is_focused" win) t)
                    (setq focused id)))))))
        (or focused first-match))
    (error nil)))

(defun bob/open-pi-in-kitty ()
  "Open or focus Pi coding agent in Kitty for the current project root."
  (interactive)
  (let* ((dir (expand-file-name (bob/monorepo-root)))
         (tab-id (gethash dir bob/kitty-pi-tabs)))
    ;; Try hash first, then discover existing tab by cwd+process, then launch new.
    (cond
     ((and tab-id (bob/kitty-tab-exists-p tab-id))
      (bob/kitty-focus-tab tab-id))
     ((when-let* ((found-id (bob/kitty-find-pi-tab dir)))
        (puthash dir found-id bob/kitty-pi-tabs)
        (bob/kitty-focus-tab found-id)
        t))
     (t
      (remhash dir bob/kitty-pi-tabs)
      (let* ((args (list "launch" "--type=tab"
                         "--cwd" dir
                         "/bin/zsh" "-li" "-c" "pi; exec /bin/zsh -li"))
             (out (apply #'bob/kitten args))
             (win-id (when (string-match "\\([0-9]+\\)" out)
                       (match-string 1 out)))
             (dark-p (string= "Dark"
                              (string-trim
                               (shell-command-to-string
                                "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))
             (theme-file (expand-file-name
                          (if dark-p "kanagawa.conf" "kanagawa-light.conf")
                          "~/.config/kitty")))
        (when win-id
          (puthash dir win-id bob/kitty-pi-tabs)
          (bob/kitten "set-colors" "--match" (format "id:%s" win-id) theme-file))
        (call-process "open" nil nil nil "-a" "kitty"))))))

;;; Sending text

(defun bob/pi--send-raw (win-id text)
  "Send TEXT to the pi window WIN-ID using bracketed paste, then submit."
  (bob/kitten "send-text" "--match" (format "id:%s" win-id)
              (concat "\x1b[200~" text "\x1b[201~"))
  (bob/kitten "send-key" "--match" (format "id:%s" win-id) "Enter"))

(defun bob/pi--current-win-id ()
  "Return the Kitty window ID for the pi session in the current project.
Opens a new tab if one doesn't exist yet."
  (let* ((dir (expand-file-name (bob/monorepo-root)))
         (tab-id (gethash dir bob/kitty-pi-tabs)))
    (unless (and tab-id (bob/kitty-tab-exists-p tab-id))
      (bob/open-pi-in-kitty))
    (gethash dir bob/kitty-pi-tabs)))

(defun bob/pi--format-region (text)
  "Wrap TEXT with file and line context for sending to pi."
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (rel-file (if (buffer-file-name)
                       (file-relative-name (buffer-file-name) (bob/monorepo-root))
                     (buffer-name)))
         (line (line-number-at-pos (if (use-region-p) (region-beginning) (point)))))
    (format "```%s\n# %s (line %d)\n%s\n```"
            (or (and (derived-mode-p 'prog-mode)
                     (string-trim-right (symbol-name major-mode) "-mode\\|-ts-mode"))
                "")
            rel-file line text)))

;;;###autoload
(defun bob/pi-show-message (text)
  "Show TEXT from pi. Short messages go to minibuffer, longer ones to a buffer."
  (if (< (length text) 120)
      (message "%s" text)
    (with-current-buffer (get-buffer-create "*pi-response*")
      (erase-buffer)
      (insert text)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun bob/pi-send-dwim ()
  "Prompt for instruction, send region (or current defun) to pi. Stay in Emacs."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (mark-defun)
                     (prog1 (cons (region-beginning) (region-end))
                       (deactivate-mark)))))
         (beg (car bounds))
         (end (cdr bounds))
         (text (buffer-substring-no-properties beg end))
         (instruction (read-string "Pi: "))
         (payload (concat instruction "\n\n" (bob/pi--format-region text)))
         (win-id (bob/pi--current-win-id)))
    (bob/pi--send-raw win-id payload)
    (deactivate-mark)
    (message "Sent to pi.")))


;;;###autoload
(defun bob/pi-send-buffer ()
  "Send the entire buffer to pi with filename context."
  (interactive)
  (let* ((instruction (read-string "Pi: "))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (payload (concat instruction "\n\n" (bob/pi--format-region text)))
         (win-id (bob/pi--current-win-id)))
    (bob/pi--send-raw win-id payload)
    (message "Sent to pi.")))

;;;###autoload
(defun bob/pi-pulse-region (file start-line end-line)
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
      ;; Use save-selected-window + with-current-buffer instead of
      ;; with-selected-window so we never touch focus, and save-excursion
      ;; so point is not disturbed even in the target buffer.
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

;;; Clipboard image paste

(defvar pi-coding-agent--image-counter 0
  "Counter for unique temp image filenames.")

(defun pi-coding-agent-paste-image ()
  "Paste image from macOS clipboard into the input buffer as a file reference.
Saves clipboard image to a temp file via pngpaste and inserts @path at point."
  (interactive)
  (unless (executable-find "pngpaste")
    (user-error "pngpaste not found — install with: brew install pngpaste"))
  (let* ((dir (temporary-file-directory))
         (name (format "pi-image-%s-%d.png"
                       (format-time-string "%Y%m%d-%H%M%S")
                       (cl-incf pi-coding-agent--image-counter)))
         (path (expand-file-name name dir)))
    (unless (zerop (call-process "pngpaste" nil nil nil path))
      (user-error "No image on clipboard"))
    (insert "@" path " ")))

(with-eval-after-load 'pi-coding-agent-input
  (define-key pi-coding-agent-input-mode-map (kbd "C-c i") #'pi-coding-agent-paste-image))

;;; Keybindings

(global-set-key (kbd "C-c p p") #'bob/open-pi-in-kitty)
(global-set-key (kbd "C-c p s") #'pi/send-buffer-context)
(global-set-key (kbd "C-c p a") #'pi/ask)

(provide 'pi-helpers)
