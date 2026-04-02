;;; solveit.el --- Solveit editor integration -*- lexical-binding: t; -*-
;; Loaded from the Emacs init so agent skills can call solveit functions directly.
;; Provides solveit-open-location (single file) and solveit-load-review
;; (multi-file) with per-chunk TTS narration, highlighted regions,
;; symbol highlighting, and posframe tooltips.

(require 'posframe)
(require 'cl-lib)
(require 'seq)

(defgroup solveit nil
  "Editor integration for Solveit-style agent workflows."
  :group 'tools)

(defcustom solveit-tts-disabled-flag
  (seq-find (lambda (path)
              (file-directory-p (file-name-directory path)))
            (list (expand-file-name "shared/tts-disabled" "~/.claude")
                  (expand-file-name "shared-skills/shared/tts-disabled" "~/.codex/skills")
                  (expand-file-name ".shared-skills/shared/tts-disabled" "~/dotfiles")))
  "Flag file that disables Solveit narration when it exists."
  :type '(choice (const :tag "No flag file" nil) file)
  :group 'solveit)

(defcustom solveit-tts-script
  (expand-file-name "modules/text-to-speech.py" user-emacs-directory)
  "Path to the Solveit text-to-speech helper script."
  :type '(choice (const :tag "No TTS script" nil) file)
  :group 'solveit)

(defun solveit-review--tts-script ()
  "Return the configured Solveit TTS helper script."
  (or solveit-tts-script
      (user-error "Solveit TTS script is not configured")))

;;; Faces

(defface solveit-highlight-face
  '((t :extend t))
  "Face for highlighted regions in review mode."
  :group 'faces)

(set-face-attribute 'solveit-highlight-face nil
                    :background (color-lighten-name
                                 (face-attribute 'default :background) 20)
                    :extend t)

(defface solveit-symbol-face
  '((t :inherit highlight))
  "Face for specific symbols within highlighted regions."
  :group 'faces)
(set-face-attribute 'solveit-symbol-face nil :inherit 'highlight)

;;; Buffer-local state

(defvar-local solveit-review--overlays nil
  "List of overlays created by solveit-review-mode.")

(defvar-local solveit-review--descriptions nil
  "Alist mapping region overlay to description string for posframe tooltips.")

(defvar-local solveit-review--prev-read-only nil
  "Previous read-only state before review mode was activated.")

(defvar-local solveit-review--audio-files nil
  "List of MP3 file paths received from the TTS generator, in chunk order.")

(defvar-local solveit-review--current-chunk 0
  "Index of the currently active chunk (single-file mode only).")

(defvar-local solveit-review--chunk-ovs nil
  "Region overlays in document order, one per chunk.")

(defvar-local solveit-review--feedback-target-id nil
  "Explicit feedback target id for the active review session.")

;;; Global state

(defvar solveit-review--active-buffer nil
  "Buffer that currently has solveit-review-mode active.")

(defvar solveit-review--tts-process nil
  "The running TTS generator process.")

(defvar solveit-review--afplay-process nil
  "The running afplay process for current chunk audio.")

(defvar solveit-review--tts-output-buffer ""
  "Accumulated stdout from TTS generator, buffered until newline.")

(defvar solveit-review--audio-tmpdir nil
  "Tmpdir created by the TTS generator; deleted on cleanup.")


;;; Global multi-file session state (solveit-load-review)

(defvar solveit-review--operations nil
  "Flat ordered list of operation plists for solveit-load-review sessions.")

(defvar solveit-review--global-index 0
  "Current operation index in a solveit-load-review session.")

(defvar solveit-review--global-mode nil
  "Non-nil when a solveit-load-review session is active.")

(defvar solveit-review--global-audio-list nil
  "Audio paths indexed by global operation index.  nil slots = not yet generated.")

(defvar solveit-review--tts-received-count 0
  "Number of audio paths received from the TTS process so far (global mode).")

(defvar solveit-review--session-feedback-target-id nil
  "Feedback target id for the current review session.")

;;; Audio

(defun kitty-get-socket ()
  "Find kitty socket file matching the pattern."
  (let ((sockets (directory-files "/tmp" t "^kitty-emacs")))
    (when sockets
      (concat "unix:" (car sockets)))))

(defun bob/kitten (&rest args)
  "Send ARGS to kitty using the current remote-control socket."
  (let ((kitty-socket (kitty-get-socket)))
    (if kitty-socket
        (apply #'call-process "kitty" nil 0 nil
               "@" "--to" kitty-socket args)
      (error "Could not find kitty socket"))))

(defun solveit-review--play-audio (path)
  "Play audio file at PATH via afplay, killing any current playback."
  (when (and solveit-review--afplay-process
             (process-live-p solveit-review--afplay-process))
    (kill-process solveit-review--afplay-process))
  (setq solveit-review--afplay-process
        (start-process "solveit-afplay" nil "afplay" path)))

(defun solveit-review--truncate-text (text &optional limit)
  "Return TEXT trimmed to LIMIT chars, preserving the start of the string."
  (let ((limit (or limit 1200)))
    (if (and text (> (length text) limit))
        (concat (substring text 0 limit) "\n[truncated]")
      text)))

(defun solveit-review--indent-text (text &optional prefix)
  "Indent each line in TEXT with PREFIX."
  (let ((prefix (or prefix "    ")))
    (mapconcat (lambda (line) (concat prefix line))
               (split-string text "\n")
               "\n")))

(defun solveit-review--chunk-overlay ()
  "Return the overlay for the currently active chunk, if any."
  (let ((idx (if solveit-review--global-mode
                 solveit-review--global-index
               solveit-review--current-chunk)))
    (nth idx solveit-review--chunk-ovs)))

(defun solveit-review--chunk-context ()
  "Return a formatted context block for the active chunk."
  (let* ((chunk-idx (if solveit-review--global-mode
                        solveit-review--global-index
                      solveit-review--current-chunk))
         (op (and solveit-review--global-mode
                  (nth chunk-idx solveit-review--operations)))
         (ov (and (not solveit-review--global-mode)
                  (solveit-review--chunk-overlay)))
         (name (or (and op (plist-get op :name))
                   (and ov (overlay-get ov 'solveit-name))))
         (desc (or (and op (plist-get op :description))
                   (and ov (overlay-get ov 'solveit-description))))
         (narration (or (and op (plist-get op :narration))
                        (and ov (overlay-get ov 'solveit-narration))))
         (symbols (or (and op (plist-get op :symbols))
                      (and ov (overlay-get ov 'solveit-symbols))))
         (start-line (or (and op (plist-get op :start_line))
                         (and ov (overlay-start ov)
                              (line-number-at-pos (overlay-start ov)))))
         (end-line (or (and op (plist-get op :end_line))
                       (and ov (overlay-end ov)
                            (line-number-at-pos (overlay-end ov)))))
         (excerpt
          (cond
           ((and op start-line end-line)
            (let ((buf (or (find-buffer-visiting (plist-get op :file))
                           solveit-review--active-buffer
                           (current-buffer))))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- start-line))
                    (let ((start (point)))
                      (goto-char (point-min))
                      (forward-line (1- end-line))
                      (end-of-line)
                      (solveit-review--truncate-text
                       (string-trim (buffer-substring-no-properties start (point))))))))))
           ((and ov (overlay-start ov) (overlay-end ov))
            (solveit-review--truncate-text
             (string-trim (buffer-substring-no-properties
                           (overlay-start ov)
                           (overlay-end ov)))))))
         (file (or (and op (file-name-nondirectory (plist-get op :file)))
                   (when buffer-file-name (file-name-nondirectory buffer-file-name)))))
    (mapconcat
     #'identity
     (delq nil
           (list
            (format "chunk-context:")
            (format "  index: %d" chunk-idx)
            (format "  name: %s" (or name "?"))
            (format "  file: %s" (or file "?"))
            (format "  start-line: %s"
                    (if start-line (number-to-string start-line) "?"))
            (format "  end-line: %s"
                    (if end-line (number-to-string end-line) "?"))
            (when desc (format "  description: %s" desc))
            (when narration
              (format "  narration:\n%s"
                      (solveit-review--indent-text narration "    ")))
            (when symbols
              (format "  symbols: %s" (mapconcat #'identity symbols ", ")))
            (when excerpt
              (format "  excerpt:\n%s"
                      (solveit-review--indent-text excerpt "    ")))))
     "\n")))

(defun solveit-review--tts-filter (_proc output)
  "Receive lines from the TTS generator process.
TMPDIR: prefix -> store tmpdir for later cleanup.
ERROR: prefix  -> write tts-disabled flag and notify.
file path      -> store and play if it matches the current position."
  (setq solveit-review--tts-output-buffer
        (concat solveit-review--tts-output-buffer output))
  (let ((lines (split-string solveit-review--tts-output-buffer "\n")))
    (setq solveit-review--tts-output-buffer (car (last lines)))
    (dolist (line (butlast lines))
      (let ((trimmed (string-trim line)))
        (cond
         ((string-prefix-p "TMPDIR:" trimmed)
          (setq solveit-review--audio-tmpdir (substring trimmed 7)))
         ((string-prefix-p "ERROR:" trimmed)
          (let ((reason (substring trimmed 6)))
            (when solveit-tts-disabled-flag
              (make-directory (file-name-directory solveit-tts-disabled-flag) t)
              (write-region (format "%s\n" reason) nil solveit-tts-disabled-flag))
            (message "Solveit TTS error: %s%s"
                     reason
                     (if solveit-tts-disabled-flag
                         (format " -- narration disabled. Fix the issue and delete %s to re-enable."
                                 solveit-tts-disabled-flag)
                       " -- narration disabled."))))
         ((and (> (length trimmed) 0) (file-exists-p trimmed))
          (if solveit-review--global-mode
              (let ((idx solveit-review--tts-received-count))
                (setf (nth idx solveit-review--global-audio-list) trimmed)
                (cl-incf solveit-review--tts-received-count)
                (when (= idx solveit-review--global-index)
                  (solveit-review--play-audio trimmed)))
            (when (buffer-live-p solveit-review--active-buffer)
              (with-current-buffer solveit-review--active-buffer
                (setq solveit-review--audio-files
                      (append solveit-review--audio-files (list trimmed)))
                (let ((idx (1- (length solveit-review--audio-files))))
                  (when (= idx solveit-review--current-chunk)
                    (solveit-review--play-audio trimmed))))))))))))

;;; Tooltip

(defun solveit-review--show-tooltip ()
  "Show posframe tooltip at right edge of window, vertically at the highlighted region start."
  (let ((desc nil)
        (anchor nil))
    (dolist (entry solveit-review--descriptions)
      (let ((ov (car entry))
            (text (cdr entry)))
        (when (and (overlay-buffer ov)
                   (>= (point) (overlay-start ov))
                   (<= (point) (overlay-end ov)))
          (setq desc text)
          (setq anchor (overlay-start ov)))))
    (if desc
        (let* ((pos-info (posn-at-point anchor))
               (anchor-y (when pos-info
                           (+ (window-pixel-top (selected-window))
                              (cdr (posn-x-y pos-info))))))
          (posframe-show "*solveit-tooltip*"
                         :string desc
                         :poshandler (lambda (info)
                                       (let* ((wl (plist-get info :parent-window-left))
                                              (ww (plist-get info :parent-window-width))
                                              (pw (plist-get info :posframe-width))
                                              (x  (max 0 (- (+ wl ww) pw 5))))
                                         (cons x (or anchor-y 0))))
                         :internal-border-width 12
                         :face 'tooltip))
      (posframe-delete "*solveit-tooltip*"))))

;;; Feedback

(defun solveit-send-feedback (feedback)
  "Append FEEDBACK with chunk context to the configured feedback target (no submit)."
  (interactive "sFeedback: ")
  (let* ((chunk-context (solveit-review--chunk-context))
         (context (concat chunk-context "\nfeedback: " feedback "\n"))
         (tab-id (or solveit-review--feedback-target-id
                    solveit-review--session-feedback-target-id)))
    (unless tab-id
      (error "No feedback target tab id set for this review buffer"))
    (bob/kitten "send-text" "--match" (format "id:%s" tab-id) context)
    (message "Feedback sent to configured tab")))

(defun solveit-set-feedback-target (tab-id)
  "Set TAB-ID as the explicit feedback target for the active review buffer."
  (interactive "sFeedback target id: ")
  (setq-local solveit-review--feedback-target-id tab-id)
  (setq solveit-review--session-feedback-target-id tab-id)
  (message "Solveit feedback target set to %s" tab-id))

;;; Navigation

(defun solveit-review--next-highlight ()
  "Move to the next chunk and play its narration."
  (interactive)
  (if solveit-review--global-mode
      (solveit-review--global-step 1)
    (let ((next-idx (1+ solveit-review--current-chunk)))
      (when (< next-idx (length solveit-review--chunk-ovs))
        (setq solveit-review--current-chunk next-idx)
        (goto-char (overlay-start (nth next-idx solveit-review--chunk-ovs)))
        (recenter 5)
        (let ((path (nth next-idx solveit-review--audio-files)))
          (when path (solveit-review--play-audio path)))))))

(defun solveit-review--prev-highlight ()
  "Move to the previous chunk and play its narration."
  (interactive)
  (if solveit-review--global-mode
      (solveit-review--global-step -1)
    (let ((prev-idx (1- solveit-review--current-chunk)))
      (when (>= prev-idx 0)
        (setq solveit-review--current-chunk prev-idx)
        (goto-char (overlay-start (nth prev-idx solveit-review--chunk-ovs)))
        (recenter 5)
        (let ((path (nth prev-idx solveit-review--audio-files)))
          (when path (solveit-review--play-audio path)))))))

(defun solveit-review--global-step (delta)
  "Move DELTA steps through the global operations list, switching files as needed."
  (let* ((next-idx (+ solveit-review--global-index delta))
         (next-op (and (>= next-idx 0)
                       (< next-idx (length solveit-review--operations))
                       (nth next-idx solveit-review--operations))))
    (when next-op
      (setq solveit-review--global-index next-idx)
      (let ((file (plist-get next-op :file)))
        (unless (and solveit-review--active-buffer
                     (buffer-live-p solveit-review--active-buffer)
                     (with-current-buffer solveit-review--active-buffer
                       (and (buffer-file-name)
                            (file-equal-p file (buffer-file-name)))))
          (solveit-review--switch-to-file file)))
      (when (and solveit-review--active-buffer
                 (buffer-live-p solveit-review--active-buffer))
        (with-current-buffer solveit-review--active-buffer
          (let ((ov (cl-find (plist-get next-op :name) solveit-review--chunk-ovs
                             :key (lambda (o) (overlay-get o 'solveit-name))
                             :test #'equal)))
            (when ov
              (goto-char (overlay-start ov))
              (recenter 5)))))
      (let ((path (nth next-idx solveit-review--global-audio-list)))
        (when path (solveit-review--play-audio path))))))

(defun solveit-review--switch-to-file (file &optional feedback-target-id)
  "Within a global session, switch to FILE and apply its highlights.
FEEDBACK-TARGET-ID, when non-nil, becomes the active feedback target in the buffer."
  (when solveit-review-mode
    (solveit-review-mode -1))
  (find-file file)
  ;; Also disable review mode if it was left active in this buffer from a prior session
  (when solveit-review-mode
    (solveit-review-mode -1))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (memq (overlay-get ov 'face) '(solveit-highlight-face solveit-symbol-face))
      (delete-overlay ov)))
  (setq solveit-review--overlays nil)
  (setq solveit-review--descriptions nil)
  (setq solveit-review--chunk-ovs nil)
  (setq solveit-review--feedback-target-id
        (or feedback-target-id solveit-review--session-feedback-target-id))
  (let ((file-ops (seq-filter (lambda (op) (equal (plist-get op :file) file))
                              solveit-review--operations)))
    (solveit-review--apply-highlights
     (mapcar (lambda (op)
               (list (plist-get op :name)
                     (plist-get op :start_line)
                     (plist-get op :end_line)
                     (plist-get op :description)
                     (plist-get op :symbols)
                     (plist-get op :narration)))
             file-ops)))
  (setq solveit-review--chunk-ovs
        (sort (mapcar #'car solveit-review--descriptions)
              (lambda (a b) (< (overlay-start a) (overlay-start b)))))
  (setq solveit-review--active-buffer (current-buffer))
  (solveit-review-mode 1))

;;; Minor mode

(define-minor-mode solveit-review-mode
  "Minor mode for guided code review with highlights and narration."
  :lighter " SolveReview"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'solveit-review-mode-disable)
            (define-key map (kbd "C-c C-n") #'solveit-review--next-highlight)
            (define-key map (kbd "C-c C-p") #'solveit-review--prev-highlight)
            (define-key map (kbd "C-c F") #'solveit-send-feedback)
            map)
  (if solveit-review-mode
      (progn
        (setq solveit-review--prev-read-only buffer-read-only)
        (setq buffer-read-only t)
        (setq-local minor-mode-overriding-map-alist
                    (cons (cons 'solveit-review-mode solveit-review-mode-map)
                          (assq-delete-all 'solveit-review-mode minor-mode-overriding-map-alist)))
        (add-hook 'post-command-hook #'solveit-review--show-tooltip nil t)
        (add-hook 'kill-buffer-hook #'solveit-review--cleanup nil t))
    (solveit-review--cleanup)))

;; Re-apply keybindings on every load so reload takes effect
(define-key solveit-review-mode-map (kbd "q")       #'solveit-review-mode-disable)
(define-key solveit-review-mode-map (kbd "C-c C-n") #'solveit-review--next-highlight)
(define-key solveit-review-mode-map (kbd "C-c C-p") #'solveit-review--prev-highlight)
(define-key solveit-review-mode-map (kbd "C-c F")   #'solveit-send-feedback)

(defun solveit-review-mode-disable ()
  "Disable solveit-review-mode and end any active global session."
  (interactive)
  (solveit-review-mode -1)
  (solveit-review--cleanup-session))

(defun solveit-review--cleanup-session ()
  "Release session-wide review state and narration resources."
  (setq solveit-review--operations nil)
  (setq solveit-review--global-index 0)
  (setq solveit-review--global-mode nil)
  (setq solveit-review--global-audio-list nil)
  (setq solveit-review--tts-received-count 0)
  (setq solveit-review--session-feedback-target-id nil)
  (when (and solveit-review--tts-process
             (process-live-p solveit-review--tts-process))
    (kill-process solveit-review--tts-process))
  (setq solveit-review--tts-process nil)
  (setq solveit-review--tts-output-buffer "")
  (when (and solveit-review--audio-tmpdir
             (file-directory-p solveit-review--audio-tmpdir))
    (delete-directory solveit-review--audio-tmpdir t))
  (setq solveit-review--audio-tmpdir nil))

(defun solveit-review--cleanup ()
  "Remove overlays, restore state, kill processes, delete audio files."
  (dolist (ov solveit-review--overlays)
    (delete-overlay ov))
  (setq solveit-review--overlays nil)
  (setq solveit-review--descriptions nil)
  (setq solveit-review--chunk-ovs nil)
  (setq buffer-read-only solveit-review--prev-read-only)
  (posframe-delete "*solveit-tooltip*")
  (remove-hook 'post-command-hook #'solveit-review--show-tooltip t)
  (when (and solveit-review--afplay-process
             (process-live-p solveit-review--afplay-process))
    (kill-process solveit-review--afplay-process))
  (setq solveit-review--afplay-process nil)
  (setq solveit-review--audio-files nil)
  (setq solveit-review--current-chunk 0)
  (when (eq solveit-review--active-buffer (current-buffer))
    (setq solveit-review--active-buffer nil)))

;;; Highlights

(defun solveit-review--apply-highlights (chunks)
  "Apply highlight overlays from CHUNKS.
Each entry is (NAME START-LINE END-LINE DESCRIPTION &optional SYMBOLS NARRATION).
NARRATION is still handled by the caller for TTS, but is also stored on the
overlay so feedback can include the original chunk context."
  (dolist (entry chunks)
    (let* ((name (nth 0 entry))
           (start-line (nth 1 entry))
           (end-line (nth 2 entry))
           (description (nth 3 entry))
           (symbols (nth 4 entry))
           (narration (nth 5 entry))
           (start (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- start-line))
                    (let* ((bias-pos (point))
                           (found (save-excursion
                                    (goto-char (point-min))
                                    (let (best best-dist)
                                      (while (search-forward name nil t)
                                        (let* ((bol (save-excursion (beginning-of-line) (point)))
                                               (dist (abs (- bol bias-pos))))
                                          (when (or (null best-dist) (< dist best-dist))
                                            (setq best bol best-dist dist))))
                                      best))))
                      (or found (save-excursion (goto-char (point-min)) (forward-line (1- start-line)) (point))))))
           (line-count (- end-line start-line))
           (end (save-excursion
                  (goto-char start)
                  (forward-line line-count)
                  (end-of-line)
                  (point)))
           (ov (make-overlay start end)))
      (overlay-put ov 'face 'solveit-highlight-face)
      (overlay-put ov 'solveit-name name)
      (overlay-put ov 'solveit-description description)
      (overlay-put ov 'solveit-symbols symbols)
      (overlay-put ov 'solveit-narration narration)
      (push ov solveit-review--overlays)
      (push (cons ov description) solveit-review--descriptions)
      (when symbols
        (save-excursion
          (dolist (sym symbols)
            (goto-char start)
            (while (search-forward sym end t)
              (let ((sym-ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put sym-ov 'face 'solveit-symbol-face)
                (push sym-ov solveit-review--overlays)))))))))

;;; Entry points

(defun solveit-open-location (file line &optional chunks feedback-target-id)
  "Open FILE at LINE with optional single-file review mode.
CHUNKS: list of (NAME START-LINE END-LINE DESCRIPTION SYMBOLS NARRATION).
FEEDBACK-TARGET-ID, when non-nil, is the feedback target used for feedback."
  (when (and solveit-review--active-buffer
             (buffer-live-p solveit-review--active-buffer))
    (with-current-buffer solveit-review--active-buffer
      (when solveit-review-mode
        (solveit-review-mode -1))))
  (find-file file)
  (goto-char (point-min))
  (forward-line (1- line))
  (recenter 3)
  (select-frame-set-input-focus (selected-frame))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (memq (overlay-get ov 'face) '(solveit-highlight-face solveit-symbol-face))
      (delete-overlay ov)))
  (when chunks
    (setq solveit-review--active-buffer (current-buffer))
    (setq solveit-review--current-chunk 0)
    (setq solveit-review--audio-files nil)
    (setq solveit-review--audio-tmpdir nil)
    (setq solveit-review--tts-output-buffer "")
    (setq solveit-review--feedback-target-id feedback-target-id)
    (setq solveit-review--session-feedback-target-id feedback-target-id)
    (solveit-review--apply-highlights chunks)
    (setq solveit-review--chunk-ovs
          (sort (mapcar #'car solveit-review--descriptions)
                (lambda (a b) (< (overlay-start a) (overlay-start b)))))
    (when solveit-review--chunk-ovs
      (goto-char (overlay-start (car solveit-review--chunk-ovs)))
      (recenter 5))
    (solveit-review-mode 1)
    (if (and solveit-tts-disabled-flag (file-exists-p solveit-tts-disabled-flag))
        (message "Solveit TTS disabled (see %s). Narration skipped." solveit-tts-disabled-flag)
      (let* ((narrations (mapcar (lambda (c) (or (nth 5 c) "")) chunks))
             (input-file (make-temp-file "solveit-tts-input-" nil ".json"))
             (script-path (solveit-review--tts-script))
             (_ (with-temp-file input-file (insert (json-encode narrations))))
             (proc (start-process "solveit-tts" "*solveit-tts*" "python" script-path input-file)))
        (set-process-filter proc #'solveit-review--tts-filter)
        (setq solveit-review--tts-process proc)))))

(defun solveit-load-review (ops-file &optional feedback-target-id)
  "Load a multi-file review session from OPS-FILE (JSON array of operation objects).
Each object must have keys: file, line, name, start_line, end_line,
description, symbols (array), narration.
Deletes OPS-FILE after reading.
FEEDBACK-TARGET-ID, when non-nil, is the feedback target used for feedback."
  (let* ((raw (json-read-file ops-file))
         (_ (delete-file ops-file))
         (ops (mapcar (lambda (obj)
                        (list :file        (alist-get 'file obj)
                              :line        (alist-get 'line obj)
                              :name        (alist-get 'name obj)
                              :start_line  (alist-get 'start_line obj)
                              :end_line    (alist-get 'end_line obj)
                              :description (alist-get 'description obj)
                              :symbols     (append (alist-get 'symbols obj) nil)
                              :narration   (alist-get 'narration obj)))
                      (append raw nil))))
    ;; Tear down any previous session
    (when solveit-review--global-mode
      (when (and solveit-review--active-buffer
                 (buffer-live-p solveit-review--active-buffer))
        (with-current-buffer solveit-review--active-buffer
          (when solveit-review-mode
            (solveit-review-mode -1)))))
    ;; Set up global state
    (setq solveit-review--operations ops)
    (setq solveit-review--global-index 0)
    (setq solveit-review--global-mode t)
    (setq solveit-review--global-audio-list (make-list (length ops) nil))
    (setq solveit-review--tts-received-count 0)
    (setq solveit-review--tts-output-buffer "")
    (setq solveit-review--audio-tmpdir nil)
    (setq solveit-review--session-feedback-target-id feedback-target-id)
    ;; Open first file and apply its highlights
    (let* ((first-op (car ops))
           (file (plist-get first-op :file)))
      (solveit-review--switch-to-file file feedback-target-id)
      (select-frame-set-input-focus (selected-frame))
      (let ((ov (cl-find (plist-get first-op :name) solveit-review--chunk-ovs
                         :key (lambda (o) (overlay-get o 'solveit-name))
                         :test #'equal)))
        (when ov
          (goto-char (overlay-start ov))
          (recenter 5))))
    ;; Spawn one TTS process for all narrations
    (if (and solveit-tts-disabled-flag (file-exists-p solveit-tts-disabled-flag))
        (message "Solveit TTS disabled. Narration skipped.")
      (let* ((narrations (mapcar (lambda (op) (or (plist-get op :narration) "")) ops))
             (input-file (make-temp-file "solveit-tts-input-" nil ".json"))
             (script-path (solveit-review--tts-script))
             (_ (with-temp-file input-file (insert (json-encode narrations))))
             (proc (start-process "solveit-tts" "*solveit-tts*" "python" script-path input-file)))
        (set-process-filter proc #'solveit-review--tts-filter)
        (setq solveit-review--tts-process proc)))))

(defun solveit-open-location-for-kitty (file line &optional chunks kitty-window-id)
  "Open FILE at LINE with CHUNKS and an explicit Kitty window id."
  (solveit-open-location file line chunks kitty-window-id))

(defun solveit-load-review-for-kitty (ops-file &optional kitty-window-id)
  "Open OPS-FILE as a review session with an explicit Kitty window id."
  (solveit-load-review ops-file kitty-window-id))

(defun solveit-show-plan (project-root plan-text)
  "Display PLAN-TEXT in a dedicated org-mode buffer."
  (let ((buf (get-buffer-create "*solveit-plan*"))
        (default-directory (file-name-as-directory project-root)))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert plan-text)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))

(defun solveit-show-diff (file)
  "Show the diff for FILE against the working tree."
  (let ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (select-frame-set-input-focus (selected-frame))
    (vc-diff)))

(defun solveit-commit (message)
  "Open a Magit commit buffer with MESSAGE prepopulated."
  (require 'magit)
  (magit-commit-create (list "--edit" (concat "--message=" message))))

(provide 'review-mode)

(provide 'solveit)
