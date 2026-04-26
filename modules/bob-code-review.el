;;; bob-code-review.el --- Solveit editor integration -*- lexical-binding: t; -*-
;; Loaded from the Emacs init so agent skills can call bob-code-review functions directly.
;; Provides bob-code-review-open-location (single file) and bob-code-review-load-review
;; (multi-file) with per-chunk TTS narration, highlighted regions,
;; symbol highlighting, and posframe tooltips.

(require 'posframe)
(require 'cl-lib)
(require 'seq)

(defgroup bob-code-review nil
  "Editor integration for Solveit-style agent workflows."
  :group 'tools)

(defcustom bob-code-review-tts-script
  (expand-file-name "modules/text-to-speech.py" user-emacs-directory)
  "Path to the Solveit text-to-speech helper script."
  :type '(choice (const :tag "No TTS script" nil) file)
  :group 'bob-code-review)

(defun bob-code-review--tts-script ()
  "Return the configured Solveit TTS helper script."
  (or bob-code-review-tts-script
      (user-error "Solveit TTS script is not configured")))

;;; Faces

(defface bob-code-review-highlight-face
  '((t :extend t))
  "Face for highlighted regions in review mode."
  :group 'faces)

(set-face-attribute 'bob-code-review-highlight-face nil
                    :background (color-lighten-name
                                 (face-attribute 'default :background) 20)
                    :extend t)

(defface bob-code-review-symbol-face
  '((t :inherit highlight))
  "Face for specific symbols within highlighted regions."
  :group 'faces)
(set-face-attribute 'bob-code-review-symbol-face nil :inherit 'highlight)

;;; Buffer-local state

(defvar-local bob-code-review--overlays nil
  "List of overlays created by bob-code-review-mode.")

(defvar-local bob-code-review--descriptions nil
  "Alist mapping region overlay to description string for posframe tooltips.")

(defvar-local bob-code-review--prev-read-only nil
  "Previous read-only state before review mode was activated.")

(defvar-local bob-code-review--audio-files nil
  "List of MP3 file paths received from the TTS generator, in chunk order.")

(defvar-local bob-code-review--current-chunk 0
  "Index of the currently active chunk (single-file mode only).")

(defvar-local bob-code-review--chunk-ovs nil
  "Region overlays in document order, one per chunk.")

(defvar-local bob-code-review--feedback-target-id nil
  "Explicit feedback target id for the active review session.")

;;; Global state

(defvar bob-code-review--active-buffer nil
  "Buffer that currently has bob-code-review-mode active.")

(defvar bob-code-review--tts-process nil
  "The running TTS generator process.")

(defvar bob-code-review--afplay-process nil
  "The running afplay process for current chunk audio.")

(defvar bob-code-review--tts-output-buffer ""
  "Accumulated stdout from TTS generator, buffered until newline.")

(defvar bob-code-review--audio-tmpdir nil
  "Tmpdir created by the TTS generator; deleted on cleanup.")

(defvar bob-code-review--tts-stderr ""
  "Accumulated unrecognized output from TTS process (likely stderr/tracebacks).")


;;; Global multi-file session state (bob-code-review-load-review)

(defvar bob-code-review--operations nil
  "Flat ordered list of operation plists for bob-code-review-load-review sessions.")

(defvar bob-code-review--global-index 0
  "Current operation index in a bob-code-review-load-review session.")

(defvar bob-code-review--global-mode nil
  "Non-nil when a bob-code-review-load-review session is active.")

(defvar bob-code-review--global-audio-list nil
  "Audio paths indexed by global operation index.  nil slots = not yet generated.")

(defvar bob-code-review--tts-received-count 0
  "Number of audio paths received from the TTS process so far (global mode).")

(defvar bob-code-review--session-feedback-target-id nil
  "Feedback target id for the current review session.")

;;; Audio

(defun kitty-get-socket ()
  "Find kitty socket file matching the pattern."
  (let ((sockets (directory-files "/tmp" t "^kitty-emacs")))
    (when sockets
      (concat "unix:" (car sockets)))))


(defun bob-code-review--play-audio (path)
  "Play audio file at PATH via afplay, killing any current playback."
  (when (and bob-code-review--afplay-process
             (process-live-p bob-code-review--afplay-process))
    (kill-process bob-code-review--afplay-process))
  (setq bob-code-review--afplay-process
        (start-process "bob-code-review-afplay" nil "afplay" path)))

(defun bob-code-review--truncate-text (text &optional limit)
  "Return TEXT trimmed to LIMIT chars, preserving the start of the string."
  (let ((limit (or limit 1200)))
    (if (and text (> (length text) limit))
        (concat (substring text 0 limit) "\n[truncated]")
      text)))

(defun bob-code-review--indent-text (text &optional prefix)
  "Indent each line in TEXT with PREFIX."
  (let ((prefix (or prefix "    ")))
    (mapconcat (lambda (line) (concat prefix line))
               (split-string text "\n")
               "\n")))

(defun bob-code-review--chunk-overlay ()
  "Return the overlay for the currently active chunk, if any."
  (let ((idx (if bob-code-review--global-mode
                 bob-code-review--global-index
               bob-code-review--current-chunk)))
    (nth idx bob-code-review--chunk-ovs)))

(defun bob-code-review--display-file (file)
  "Return a readable display path for FILE.
Prefer a path relative to the nearest git root, falling back to an abbreviated
absolute path."
  (when file
    (let* ((root (or (locate-dominating-file default-directory ".git")
                     default-directory))
           (expanded (if (file-name-absolute-p file) file
                       (expand-file-name file root)))
           (git-root (locate-dominating-file expanded ".git")))
      (if git-root
          (file-relative-name expanded git-root)
        (abbreviate-file-name expanded)))))

(defun bob-code-review--chunk-context ()
  "Return a formatted context block for the active chunk."
  (let* ((chunk-idx (if bob-code-review--global-mode
                        bob-code-review--global-index
                      bob-code-review--current-chunk))
         (op (and bob-code-review--global-mode
                  (nth chunk-idx bob-code-review--operations)))
         (ov (and (not bob-code-review--global-mode)
                  (bob-code-review--chunk-overlay)))
         (name (or (and op (plist-get op :name))
                   (and ov (overlay-get ov 'bob-code-review-name))))
         (desc (or (and op (plist-get op :description))
                   (and ov (overlay-get ov 'bob-code-review-description))))
         (narration (or (and op (plist-get op :narration))
                        (and ov (overlay-get ov 'bob-code-review-narration))))
         (symbols (or (and op (plist-get op :symbols))
                      (and ov (overlay-get ov 'bob-code-review-symbols))))
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
                           bob-code-review--active-buffer
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
                      (bob-code-review--truncate-text
                       (string-trim (buffer-substring-no-properties start (point))))))))))
           ((and ov (overlay-start ov) (overlay-end ov))
            (bob-code-review--truncate-text
             (string-trim (buffer-substring-no-properties
                           (overlay-start ov)
                           (overlay-end ov)))))))
         (file (or (and op (bob-code-review--display-file (plist-get op :file)))
                   (when buffer-file-name
                     (bob-code-review--display-file buffer-file-name)))))
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
                      (bob-code-review--indent-text narration "    ")))
            (when symbols
              (format "  symbols: %s" (mapconcat #'identity symbols ", ")))
            (when excerpt
              (format "  excerpt:\n%s"
                      (bob-code-review--indent-text excerpt "    ")))))
     "\n")))

(defun bob-code-review--tts-sentinel (proc event)
  "Log TTS process failures so errors don't vanish silently."
  (when (and (memq (process-status proc) '(exit signal))
             (/= (process-exit-status proc) 0))
    (let ((stderr (string-trim bob-code-review--tts-stderr)))
      (message "Solveit TTS process %s (exit %d)%s"
               (string-trim event)
               (process-exit-status proc)
               (if (string-empty-p stderr) ""
                 (format ":\n%s" stderr))))))

(defun bob-code-review--tts-filter (_proc output)
  "Receive lines from the TTS generator process.
TMPDIR: prefix -> store tmpdir for later cleanup.
ERROR: prefix  -> log a message; narration is skipped for the rest of the run.
file path      -> store and play if it matches the current position."
  (setq bob-code-review--tts-output-buffer
        (concat bob-code-review--tts-output-buffer output))
  (let ((lines (split-string bob-code-review--tts-output-buffer "\n")))
    (setq bob-code-review--tts-output-buffer (car (last lines)))
    (dolist (line (butlast lines))
      (let ((trimmed (string-trim line)))
        (cond
         ((string-prefix-p "TMPDIR:" trimmed)
          (setq bob-code-review--audio-tmpdir (substring trimmed 7)))
         ((string-prefix-p "ERROR:" trimmed)
          (message "Solveit TTS error: %s -- narration skipped for this session."
                   (substring trimmed 6)))
         ((and (> (length trimmed) 0) (file-exists-p trimmed))
          (if bob-code-review--global-mode
              (let ((idx bob-code-review--tts-received-count))
                (setf (nth idx bob-code-review--global-audio-list) trimmed)
                (cl-incf bob-code-review--tts-received-count)
                (when (= idx bob-code-review--global-index)
                  (bob-code-review--play-audio trimmed)))
            (when (buffer-live-p bob-code-review--active-buffer)
              (with-current-buffer bob-code-review--active-buffer
                (setq bob-code-review--audio-files
                      (append bob-code-review--audio-files (list trimmed)))
                (let ((idx (1- (length bob-code-review--audio-files))))
                  (when (= idx bob-code-review--current-chunk)
                    (bob-code-review--play-audio trimmed)))))))
         ((> (length trimmed) 0)
          (setq bob-code-review--tts-stderr
                (concat bob-code-review--tts-stderr trimmed "\n"))))))))

;;; Tooltip

(defun bob-code-review--show-tooltip ()
  "Show posframe tooltip at right edge of window, vertically at the highlighted region start."
  (let ((desc nil)
        (anchor nil))
    (dolist (entry bob-code-review--descriptions)
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
          (posframe-show "*bob-code-review-tooltip*"
                         :string desc
                         :poshandler (lambda (info)
                                       (let* ((wl (plist-get info :parent-window-left))
                                              (ww (plist-get info :parent-window-width))
                                              (pw (plist-get info :posframe-width))
                                              (x  (max 0 (- (+ wl ww) pw 5))))
                                         (cons x (or anchor-y 0))))
                         :internal-border-width 12
                         :face 'tooltip))
      (posframe-delete "*bob-code-review-tooltip*"))))

;;; Feedback

(defun bob-code-review-send-feedback (feedback)
  "Append FEEDBACK with chunk context to the configured feedback target (no submit)."
  (interactive "sFeedback: ")
  (let* ((chunk-context (bob-code-review--chunk-context))
         (context (concat chunk-context "\nfeedback: " feedback "\n"))
         (tab-id (or bob-code-review--feedback-target-id
                    bob-code-review--session-feedback-target-id)))
    (unless tab-id
      (error "No feedback target tab id set for this review buffer"))
    (bob/kitten "send-text" "--match" (format "id:%s" tab-id) context)
    (message "Feedback sent to configured tab")))

(defun bob-code-review-set-feedback-target (tab-id)
  "Set TAB-ID as the explicit feedback target for the active review buffer."
  (interactive "sFeedback target id: ")
  (setq-local bob-code-review--feedback-target-id tab-id)
  (setq bob-code-review--session-feedback-target-id tab-id)
  (message "Solveit feedback target set to %s" tab-id))

;;; Navigation

(defun bob-code-review--next-highlight ()
  "Move to the next chunk and play its narration."
  (interactive)
  (if bob-code-review--global-mode
      (bob-code-review--global-step 1)
    (let ((next-idx (1+ bob-code-review--current-chunk)))
      (when (< next-idx (length bob-code-review--chunk-ovs))
        (setq bob-code-review--current-chunk next-idx)
        (goto-char (overlay-start (nth next-idx bob-code-review--chunk-ovs)))
        (recenter 5)
        (let ((path (nth next-idx bob-code-review--audio-files)))
          (when path (bob-code-review--play-audio path)))))))

(defun bob-code-review--prev-highlight ()
  "Move to the previous chunk and play its narration."
  (interactive)
  (if bob-code-review--global-mode
      (bob-code-review--global-step -1)
    (let ((prev-idx (1- bob-code-review--current-chunk)))
      (when (>= prev-idx 0)
        (setq bob-code-review--current-chunk prev-idx)
        (goto-char (overlay-start (nth prev-idx bob-code-review--chunk-ovs)))
        (recenter 5)
        (let ((path (nth prev-idx bob-code-review--audio-files)))
          (when path (bob-code-review--play-audio path)))))))

(defun bob-code-review--global-step (delta)
  "Move DELTA steps through the global operations list, switching files as needed."
  (let* ((next-idx (+ bob-code-review--global-index delta))
         (next-op (and (>= next-idx 0)
                       (< next-idx (length bob-code-review--operations))
                       (nth next-idx bob-code-review--operations))))
    (when next-op
      (setq bob-code-review--global-index next-idx)
      (let ((file (plist-get next-op :file)))
        (unless (and bob-code-review--active-buffer
                     (buffer-live-p bob-code-review--active-buffer)
                     (with-current-buffer bob-code-review--active-buffer
                       (and (buffer-file-name)
                            (file-equal-p file (buffer-file-name)))))
          (bob-code-review--switch-to-file file)))
      (when (and bob-code-review--active-buffer
                 (buffer-live-p bob-code-review--active-buffer))
        (with-current-buffer bob-code-review--active-buffer
          (let ((ov (cl-find (plist-get next-op :name) bob-code-review--chunk-ovs
                             :key (lambda (o) (overlay-get o 'bob-code-review-name))
                             :test #'equal)))
            (when ov
              (goto-char (overlay-start ov))
              (recenter 5)))))
      (let ((path (nth next-idx bob-code-review--global-audio-list)))
        (when path (bob-code-review--play-audio path))))))

(defun bob-code-review--switch-to-file (file &optional feedback-target-id)
  "Within a global session, switch to FILE and apply its highlights.
FEEDBACK-TARGET-ID, when non-nil, becomes the active feedback target in the buffer."
  (when bob-code-review-mode
    (bob-code-review-mode -1))
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   default-directory))
         (abs-file (if (file-name-absolute-p file) file
                     (expand-file-name file root))))
    (find-file abs-file))
  ;; Also disable review mode if it was left active in this buffer from a prior session
  (when bob-code-review-mode
    (bob-code-review-mode -1))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (memq (overlay-get ov 'face) '(bob-code-review-highlight-face bob-code-review-symbol-face))
      (delete-overlay ov)))
  (setq bob-code-review--overlays nil)
  (setq bob-code-review--descriptions nil)
  (setq bob-code-review--chunk-ovs nil)
  (setq bob-code-review--feedback-target-id
        (or feedback-target-id bob-code-review--session-feedback-target-id))
  (let ((file-ops (seq-filter (lambda (op) (equal (plist-get op :file) file))
                              bob-code-review--operations)))
    (bob-code-review--apply-highlights
     (mapcar (lambda (op)
               (list (plist-get op :name)
                     (plist-get op :start_line)
                     (plist-get op :end_line)
                     (plist-get op :description)
                     (plist-get op :symbols)
                     (plist-get op :narration)))
             file-ops)))
  (setq bob-code-review--chunk-ovs
        (sort (mapcar #'car bob-code-review--descriptions)
              (lambda (a b) (< (overlay-start a) (overlay-start b)))))
  (setq bob-code-review--active-buffer (current-buffer))
  (bob-code-review-mode 1))

;;; Minor mode

(define-minor-mode bob-code-review-mode
  "Minor mode for guided code review with highlights and narration."
  :lighter " CodeReview"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'bob-code-review-mode-disable)
            (define-key map (kbd "C-c C-n") #'bob-code-review--next-highlight)
            (define-key map (kbd "C-c C-p") #'bob-code-review--prev-highlight)
            (define-key map (kbd "C-c F") #'bob-code-review-send-feedback)
            map)
  (if bob-code-review-mode
      (progn
        (setq bob-code-review--prev-read-only buffer-read-only)
        (setq buffer-read-only t)
        (setq-local minor-mode-overriding-map-alist
                    (cons (cons 'bob-code-review-mode bob-code-review-mode-map)
                          (assq-delete-all 'bob-code-review-mode minor-mode-overriding-map-alist)))
        (add-hook 'post-command-hook #'bob-code-review--show-tooltip nil t)
        (add-hook 'kill-buffer-hook #'bob-code-review--cleanup nil t))
    (bob-code-review--cleanup)))

;; Re-apply keybindings on every load so reload takes effect
(define-key bob-code-review-mode-map (kbd "q")       #'bob-code-review-mode-disable)
(define-key bob-code-review-mode-map (kbd "C-c C-n") #'bob-code-review--next-highlight)
(define-key bob-code-review-mode-map (kbd "C-c C-p") #'bob-code-review--prev-highlight)
(define-key bob-code-review-mode-map (kbd "C-c F")   #'bob-code-review-send-feedback)

(defun bob-code-review-mode-disable ()
  "Disable bob-code-review-mode and end any active global session."
  (interactive)
  (bob-code-review-mode -1)
  (bob-code-review--cleanup-session))

(defun bob-code-review--cleanup-session ()
  "Release session-wide review state and narration resources."
  (setq bob-code-review--operations nil)
  (setq bob-code-review--global-index 0)
  (setq bob-code-review--global-mode nil)
  (setq bob-code-review--global-audio-list nil)
  (setq bob-code-review--tts-received-count 0)
  (setq bob-code-review--session-feedback-target-id nil)
  (when (and bob-code-review--tts-process
             (process-live-p bob-code-review--tts-process))
    (kill-process bob-code-review--tts-process))
  (setq bob-code-review--tts-process nil)
  (setq bob-code-review--tts-output-buffer "")
  (when (and bob-code-review--audio-tmpdir
             (file-directory-p bob-code-review--audio-tmpdir))
    (delete-directory bob-code-review--audio-tmpdir t))
  (setq bob-code-review--audio-tmpdir nil))

(defun bob-code-review--cleanup ()
  "Remove overlays, restore state, kill processes, delete audio files."
  (dolist (ov bob-code-review--overlays)
    (delete-overlay ov))
  (setq bob-code-review--overlays nil)
  (setq bob-code-review--descriptions nil)
  (setq bob-code-review--chunk-ovs nil)
  (setq buffer-read-only bob-code-review--prev-read-only)
  (posframe-delete "*bob-code-review-tooltip*")
  (remove-hook 'post-command-hook #'bob-code-review--show-tooltip t)
  (when (and bob-code-review--afplay-process
             (process-live-p bob-code-review--afplay-process))
    (kill-process bob-code-review--afplay-process))
  (setq bob-code-review--afplay-process nil)
  (setq bob-code-review--audio-files nil)
  (setq bob-code-review--current-chunk 0)
  (when (eq bob-code-review--active-buffer (current-buffer))
    (setq bob-code-review--active-buffer nil)))

;;; Highlights

(defun bob-code-review--apply-highlights (chunks)
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
      (overlay-put ov 'face 'bob-code-review-highlight-face)
      (overlay-put ov 'bob-code-review-name name)
      (overlay-put ov 'bob-code-review-description description)
      (overlay-put ov 'bob-code-review-symbols symbols)
      (overlay-put ov 'bob-code-review-narration narration)
      (push ov bob-code-review--overlays)
      (push (cons ov description) bob-code-review--descriptions)
      (when symbols
        (save-excursion
          (dolist (sym symbols)
            (goto-char start)
            (while (search-forward sym end t)
              (let ((sym-ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put sym-ov 'face 'bob-code-review-symbol-face)
                (push sym-ov bob-code-review--overlays)))))))))

;;; Entry points

(defun bob-code-review-open-location (file line &optional chunks feedback-target-id narrate)
  "Open FILE at LINE with optional single-file review mode.
CHUNKS: list of (NAME START-LINE END-LINE DESCRIPTION SYMBOLS NARRATION).
FEEDBACK-TARGET-ID, when non-nil, is the feedback target used for feedback.
If nil, auto-discovers the Kitty window ID for the current project.
NARRATE, when non-nil, launches TTS narration for the chunks."

  (setq feedback-target-id (or feedback-target-id
                               (and (fboundp 'bob/kitty-pi-window-id)
                                    (bob/kitty-pi-window-id))))
  (when (and bob-code-review--active-buffer
             (buffer-live-p bob-code-review--active-buffer))
    (with-current-buffer bob-code-review--active-buffer
      (when bob-code-review-mode
        (bob-code-review-mode -1))))
  (find-file file)
  (goto-char (point-min))
  (forward-line (1- line))
  (recenter 3)
  (select-frame-set-input-focus (selected-frame))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (memq (overlay-get ov 'face) '(bob-code-review-highlight-face bob-code-review-symbol-face))
      (delete-overlay ov)))
  (when chunks
    (setq bob-code-review--active-buffer (current-buffer))
    (setq bob-code-review--current-chunk 0)
    (setq bob-code-review--audio-files nil)
    (setq bob-code-review--audio-tmpdir nil)
    (setq bob-code-review--tts-output-buffer "")
    (setq bob-code-review--feedback-target-id feedback-target-id)
    (setq bob-code-review--session-feedback-target-id (or feedback-target-id
                                                        bob-code-review--session-feedback-target-id))
    (bob-code-review--apply-highlights chunks)
    (setq bob-code-review--chunk-ovs
          (sort (mapcar #'car bob-code-review--descriptions)
                (lambda (a b) (< (overlay-start a) (overlay-start b)))))
    (when bob-code-review--chunk-ovs
      (goto-char (overlay-start (car bob-code-review--chunk-ovs)))
      (recenter 5))
    (bob-code-review-mode 1)
    (when narrate
      (let* ((narrations (mapcar (lambda (c) (or (nth 5 c) "")) chunks))
             (input-file (make-temp-file "bob-code-review-tts-input-" nil ".json"))
             (script-path (bob-code-review--tts-script))
             (_ (with-temp-file input-file (insert (json-encode narrations))))
             (proc (start-process "bob-code-review-tts" "*bob-code-review-tts*" "python" script-path input-file)))
        (set-process-filter proc #'bob-code-review--tts-filter)
        (set-process-sentinel proc #'bob-code-review--tts-sentinel)
        (setq bob-code-review--tts-process proc)))))

(defun bob-code-review-load-review (ops-file &optional feedback-target-id narrate)
  "Load a multi-file review session from OPS-FILE (JSON array of operation objects).
Each object must have keys: file, line, name, start_line, end_line,
description, symbols (array), narration.
Deletes OPS-FILE after reading.
FEEDBACK-TARGET-ID, when non-nil, is the feedback target used for feedback.
If nil, auto-discovers the Kitty window ID for the current project.
NARRATE, when non-nil, launches TTS narration for the chunks."

  (setq feedback-target-id (or feedback-target-id
                               (and (fboundp 'bob/kitty-pi-window-id)
                                    (bob/kitty-pi-window-id))))
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
    (when bob-code-review--global-mode
      (when (and bob-code-review--active-buffer
                 (buffer-live-p bob-code-review--active-buffer))
        (with-current-buffer bob-code-review--active-buffer
          (when bob-code-review-mode
            (bob-code-review-mode -1)))))
    ;; Set up global state
    (setq bob-code-review--operations ops)
    (setq bob-code-review--global-index 0)
    (setq bob-code-review--global-mode t)
    (setq bob-code-review--global-audio-list (make-list (length ops) nil))
    (setq bob-code-review--tts-received-count 0)
    (setq bob-code-review--tts-output-buffer "")
    (setq bob-code-review--audio-tmpdir nil)
    (setq bob-code-review--session-feedback-target-id feedback-target-id)
    ;; Open first file and apply its highlights
    (let* ((first-op (car ops))
           (file (plist-get first-op :file)))
      (bob-code-review--switch-to-file file feedback-target-id)
      (select-frame-set-input-focus (selected-frame))
      (let ((ov (cl-find (plist-get first-op :name) bob-code-review--chunk-ovs
                         :key (lambda (o) (overlay-get o 'bob-code-review-name))
                         :test #'equal)))
        (when ov
          (goto-char (overlay-start ov))
          (recenter 5))))
    ;; Spawn one TTS process for all narrations
    (when narrate
      (let* ((narrations (mapcar (lambda (op) (or (plist-get op :narration) "")) ops))
             (input-file (make-temp-file "bob-code-review-tts-input-" nil ".json"))
             (script-path (bob-code-review--tts-script))
             (_ (with-temp-file input-file (insert (json-encode narrations))))
             (proc (start-process "bob-code-review-tts" "*bob-code-review-tts*" "python" script-path input-file)))
        (set-process-filter proc #'bob-code-review--tts-filter)
        (set-process-sentinel proc #'bob-code-review--tts-sentinel)
        (setq bob-code-review--tts-process proc)))))

;; Deprecated: feedback target is now auto-discovered.  Kept as aliases
;; for backward compatibility with older skill invocations.
(defalias 'bob-code-review-open-location-for-kitty #'bob-code-review-open-location)
(defalias 'bob-code-review-load-review-for-kitty #'bob-code-review-load-review)

(defun bob-code-review-show-plan (project-root plan-text)
  "Display PLAN-TEXT in a dedicated org-mode buffer."
  (let ((buf (get-buffer-create "*bob-code-review-plan*"))
        (default-directory (file-name-as-directory project-root)))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert plan-text)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))

(defun bob-code-review-show-diff (file)
  "Show the diff for FILE against the working tree."
  (let ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (select-frame-set-input-focus (selected-frame))
    (vc-diff)))

(defun bob-code-review-commit (message)
  "Open a Magit commit buffer with MESSAGE prepopulated."
  (require 'magit)
  (magit-commit-create (list "--edit" (concat "--message=" message))))

(provide 'review-mode)

(provide 'bob-code-review)
