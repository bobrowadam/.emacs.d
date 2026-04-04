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

;;; Open / focus

(defun bob/open-pi-in-kitty ()
  "Open or focus Pi coding agent in Kitty for the current project root."
  (interactive)
  (let* ((dir (expand-file-name (bob/monorepo-root)))
         (title (concat "pi:" (file-name-nondirectory (directory-file-name dir))))
         (tab-id (gethash dir bob/kitty-pi-tabs)))
    (if (and tab-id (bob/kitty-tab-exists-p tab-id))
        (bob/kitty-focus-tab tab-id)
      (remhash dir bob/kitty-pi-tabs)
      (let* ((args (list "launch" "--type=tab"
                         "--tab-title" title
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
        (call-process "open" nil nil nil "-a" "kitty")))))

;;; Sending text

(defun bob/pi--send-raw (win-id text)
  "Send TEXT to the pi window WIN-ID using bracketed paste, then submit."
  (bob/kitten "send-text" "--match" (format "id:%s" win-id)
              (concat "\x1b[200~" text "\x1b[201~\n")))

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
(defun bob/pi-send-region (beg end)
  "Send the active region to pi with file/line context, then focus Kitty."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (payload (bob/pi--format-region text))
         (win-id (bob/pi--current-win-id)))
    (bob/pi--send-raw win-id payload)
    (call-process "open" nil nil nil "-a" "kitty")))

;;;###autoload
(defun bob/pi-send-dwim ()
  "Send region if active, otherwise the current defun, to pi."
  (interactive)
  (if (use-region-p)
      (bob/pi-send-region (region-beginning) (region-end))
    (save-excursion
      (mark-defun)
      (bob/pi-send-region (region-beginning) (region-end))
      (deactivate-mark))))

;;;###autoload
(defun bob/pi-send-buffer ()
  "Send the entire buffer to pi with filename context."
  (interactive)
  (bob/pi-send-region (point-min) (point-max)))

;;;###autoload
(defun bob/pi-ask-region (beg end)
  "Prompt for an instruction, then send the region + instruction to pi.
Stays in Emacs after sending — does not switch focus to Kitty."
  (interactive "r")
  (let* ((instruction (read-string "Pi: "))
         (text (buffer-substring-no-properties beg end))
         (payload (concat instruction "\n\n" (bob/pi--format-region text)))
         (win-id (bob/pi--current-win-id)))
    (bob/pi--send-raw win-id payload)
    (deactivate-mark)
    (message "Sent to pi.")))

;;;###autoload
(defun bob/pi-ask-dwim ()
  "Prompt for instruction and send region if active, otherwise current defun."
  (interactive)
  (if (use-region-p)
      (bob/pi-ask-region (region-beginning) (region-end))
    (save-excursion
      (mark-defun)
      (bob/pi-ask-region (region-beginning) (region-end))
      (deactivate-mark))))

;;;###autoload
(defun bob/pi-pulse-region (file start-line end-line)
  "Pulse-highlight lines START-LINE to END-LINE in FILE.
Called by pi after it edits a region, to give visual feedback."
  (require 'pulse)
  (let ((buf (find-buffer-visiting file)))
    (unless buf
      (setq buf (find-file-noselect file)))
    (with-current-buffer buf
      (let ((beg (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- start-line))
                   (point)))
            (end (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- end-line))
                   (end-of-line)
                   (point))))
        (pulse-momentary-highlight-region beg end)))))

;;; Keybindings

(global-set-key (kbd "C-c p p") #'bob/open-pi-in-kitty)
(global-set-key (kbd "C-c p s") #'bob/pi-send-dwim)
(global-set-key (kbd "C-c p S") #'bob/pi-send-buffer)
(global-set-key (kbd "C-c p a") #'bob/pi-ask-dwim)

(provide 'pi)
