;;; bob-gptel-quick.el --- Fast posframe lookups with gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Small, self-contained gptel quick lookup command.  It is inspired by
;; karthink/gptel-quick, but keeps the request assembly, posframe styling,
;; and pending spinner in one place so it can match this configuration.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'subr-x)
(require 'thingatpt)

(declare-function pdf-view-active-region-p "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-window-center "posframe")
(declare-function gptel-anthropic-oauth--get-oauth-headers "gptel-anthropic-oauth")

(defgroup bob-gptel-quick nil
  "Fast posframe lookups with gptel."
  :group 'gptel)

(defcustom bob/gptel-quick-word-count 30
  "Approximate word count for quick answers."
  :type 'integer
  :group 'bob-gptel-quick)

(defcustom bob/gptel-quick-timeout nil
  "Seconds before the quick answer popup is dismissed.

When nil, never auto-dismiss; use \[keyboard-quit] while the quick
popup transient map is active."
  :type '(choice (const :tag "Never auto-dismiss" nil)
                 (number :tag "Seconds"))
  :group 'bob-gptel-quick)

(defcustom bob/gptel-quick-spinner-interval 0.12
  "Seconds between spinner frame updates in the quick popup."
  :type 'number
  :group 'bob-gptel-quick)

(defcustom bob/gptel-quick-spinner-frames
  (if (boundp 'bob/gptel-activity-spinner-frames)
      bob/gptel-activity-spinner-frames
    '("⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"))
  "Sequence of characters cycled as the quick popup spinner."
  :type '(repeat string)
  :group 'bob-gptel-quick)

(defcustom bob/gptel-quick-display 'posframe
  "How to display quick lookup results.

Set to `posframe' to use posframe when available.  Any other value
falls back to the echo area."
  :type '(choice
          (const :tag "In posframe" posframe)
          (const :tag "Echo area" nil))
  :group 'bob-gptel-quick)

(defvar bob/gptel-quick-backend
  (gptel-make-anthropic "Claude quick"
    :stream t
    ;; No thinking and no tools: quick lookups should be a single fast
    ;; round-trip, not an agentic session.
    :request-params '(:max_tokens 2048))
  "Non-thinking Anthropic backend used by `bob/gptel-quick'.")

(defvar bob/gptel-quick-model 'claude-sonnet-4-6
  "Model used by `bob/gptel-quick'.")

(defconst bob/gptel-quick--buffer-name " *gptel-quick*"
  "Name of the quick lookup posframe buffer.")

(defvar bob/gptel-quick--spinner-timer nil
  "Timer advancing the quick popup spinner.")

(defvar bob/gptel-quick--spinner-idx 0
  "Current quick popup spinner frame index.")

(defvar bob/gptel-quick--spinner-pos nil
  "Position where the pending spinner should be displayed.")

(defun bob/gptel-quick--pin-oauth-header ()
  "Pin OAuth headers on `bob/gptel-quick-backend'.

The global `gptel-anthropic-oauth' advice swaps headers on the
dynamic `gptel-backend', but inside gptel's request machinery the
backend is rebound to the FSM's backend.  Pin a header function on
this backend directly so it authenticates on every request."
  (when (and bob/gptel-quick-backend
             (fboundp 'gptel-anthropic-oauth--get-oauth-headers))
    (setf (gptel-backend-header bob/gptel-quick-backend)
          (lambda (_info)
            (let ((gptel-backend bob/gptel-quick-backend))
              (gptel-anthropic-oauth--get-oauth-headers))))))

(defun bob/gptel-quick--system-message (count)
  "Return the quick lookup system message for COUNT words."
  (format "Answer in %d words or fewer. Use only the supplied text unless the question asks otherwise."
          count))

(defun bob/gptel-quick--text-at-point ()
  "Return active region, PDF selection, thing at point, or buffer text."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((and (derived-mode-p 'pdf-view-mode)
         (fboundp 'pdf-view-active-region-p)
         (pdf-view-active-region-p))
    (mapconcat #'identity (pdf-view-active-region-text) "\n\n"))
   ((thing-at-point 'sexp))
   (t (buffer-substring-no-properties (point-min) (point-max)))))

(defun bob/gptel-quick--posn ()
  "Return a position object near the active region or point."
  (posn-at-point (and (use-region-p) (region-beginning))))

;; From (info "(elisp) Accessing Mouse") via gptel-quick.
(defun bob/gptel-quick--frame-relative-coordinates (position)
  "Return frame-relative coordinates from POSITION."
  (let* ((x-y (posn-x-y position))
         (window (posn-window position))
         (edges (window-inside-pixel-edges window)))
    (cons (+ (or (car x-y) 0) (car edges))
          (+ (or (cdr x-y) 0) (cadr edges)))))

(defun bob/gptel-quick--posframe-available-p ()
  "Non-nil if quick lookup should use posframe."
  (and (display-graphic-p)
       (eq bob/gptel-quick-display 'posframe)
       (require 'posframe nil t)))

(defun bob/gptel-quick--poshandler-above (info)
  "Position quick posframe above the captured point coordinates."
  (let* ((position (plist-get info :position))
         (x-pixel-offset (plist-get info :x-pixel-offset))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (posframe-width (or (plist-get info :posframe-width) 0))
         (posframe-height (or (plist-get info :posframe-height) 0))
         (frame-width (or (plist-get info :parent-frame-width) 0))
         (x (+ (car position) (or x-pixel-offset 0)))
         (y (+ (cdr position) (or y-pixel-offset 0))))
    (cons (max 0 (min x (- frame-width posframe-width)))
          (max 0 (- y posframe-height 6)))))

(defun bob/gptel-quick--delete-fence-lines ()
  "Delete Markdown fence marker lines from the current buffer.

Run after `gfm-view-mode' fontification: the code body keeps its
native fontification, but the hidden ``` fence lines no longer leave
blank code-background bands in the posframe.  Also remove a single
blank separator immediately before an opening fence, which otherwise
shows up as a full-width grey band in the compact popup."
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*```.*$" nil t)
        (let ((beg (line-beginning-position))
              (end (min (point-max) (1+ (line-end-position)))))
          (save-excursion
            (goto-char beg)
            (when (and (> (line-number-at-pos) 1)
                       (progn (forward-line -1)
                              (looking-at-p "^[[:space:]]*$")))
              (setq beg (line-beginning-position))))
          (delete-region beg end))))))

(defun bob/gptel-quick--rendered-height (text)
  "Return popup height for TEXT after fence cleanup."
  (with-temp-buffer
    (insert text)
    (bob/gptel-quick--delete-fence-lines)
    (max 1 (count-lines (point-min) (point-max)))))

(defun bob/gptel-quick--show (text pos &optional timeout render-markdown)
  "Show TEXT near POS in a styled posframe or the echo area.

If TIMEOUT is non-nil, dismiss the popup after that many seconds.
If RENDER-MARKDOWN is non-nil, enable `gfm-view-mode' after inserting TEXT."
  (if (bob/gptel-quick--posframe-available-p)
      (let ((fringe-indicator-alist nil)
            coords poshandler)
        ;; `markdown-view-mode' / `gfm-view-mode' make the buffer read-only,
        ;; but `posframe-show' reinserts text for spinner and response updates.
        ;; Keep the spinner plain, and enable markdown only for final output.
        (when-let* ((buffer (get-buffer bob/gptel-quick--buffer-name)))
          (with-current-buffer buffer
            (setq buffer-read-only nil)))
        (if (and pos (not (equal (posn-x-y pos) '(0 . 0))))
            (setq coords (bob/gptel-quick--frame-relative-coordinates pos)
                  poshandler #'bob/gptel-quick--poshandler-above)
          (setq poshandler #'posframe-poshandler-window-center))
        (posframe-show bob/gptel-quick--buffer-name
                       :string text
                       :position coords
                       :border-width 2
                       :border-color (face-attribute 'shadow :foreground nil t)
                       :foreground-color (face-attribute 'tooltip :foreground nil t)
                       :background-color (face-attribute 'tooltip :background nil t)
                       :poshandler poshandler
                       :left-fringe 8
                       :right-fringe 8
                       :min-width 36
                       :max-width fill-column
                       :height (and render-markdown
                                    (bob/gptel-quick--rendered-height text))
                       :min-height 1
                       :timeout timeout)
        (when-let* ((buffer (get-buffer bob/gptel-quick--buffer-name)))
          (with-current-buffer buffer
            (when render-markdown
              (let ((inhibit-read-only t)
                    (buffer-read-only nil))
                (when (require 'markdown-mode nil t)
                  (gfm-view-mode)
                  ;; View modes enable markup hiding via font-lock.  Force it
                  ;; now because this buffer is displayed in a child frame and
                  ;; may not get jit-lock fontified before the user sees it.
                  (font-lock-ensure)
                  (jit-lock-fontify-now)
                  (bob/gptel-quick--delete-fence-lines))))
            (visual-line-mode 1)
            (setq-local truncate-lines nil)
            (setq buffer-read-only nil)
            (when render-markdown
              ;; Do not call `posframe-refresh' here: switching major modes
              ;; resets posframe's buffer-local bookkeeping, and refresh can
              ;; then resize the selected parent frame instead of the child.
              (force-window-update buffer)
              (redisplay t)))))
    (message "%s" text)))

(defun bob/gptel-quick--hide ()
  "Hide the quick lookup posframe."
  (when (and (eq bob/gptel-quick-display 'posframe)
             (fboundp 'posframe-hide))
    (posframe-hide bob/gptel-quick--buffer-name)))

(defun bob/gptel-quick--spinner-frame ()
  "Return the current quick spinner frame."
  (propertize (nth (mod bob/gptel-quick--spinner-idx
                        (length bob/gptel-quick-spinner-frames))
                   bob/gptel-quick-spinner-frames)
              'face (if (facep 'bob/gptel-activity-spinner-face)
                        'bob/gptel-activity-spinner-face
                      'mode-line-emphasis)))

(defun bob/gptel-quick--spinner-text ()
  "Return the pending quick lookup spinner text."
  (concat (bob/gptel-quick--spinner-frame)
          " Querying Claude…"))

(defun bob/gptel-quick--spinner-tick ()
  "Advance and redraw the quick lookup spinner."
  (cl-incf bob/gptel-quick--spinner-idx)
  (bob/gptel-quick--show (bob/gptel-quick--spinner-text)
                         bob/gptel-quick--spinner-pos))

(defun bob/gptel-quick--spinner-start (pos)
  "Start the quick lookup spinner at POS."
  (bob/gptel-quick--spinner-stop)
  (setq bob/gptel-quick--spinner-pos pos
        bob/gptel-quick--spinner-idx 0)
  (bob/gptel-quick--show (bob/gptel-quick--spinner-text) pos)
  (when (bob/gptel-quick--posframe-available-p)
    (setq bob/gptel-quick--spinner-timer
          (run-at-time bob/gptel-quick-spinner-interval
                       bob/gptel-quick-spinner-interval
                       #'bob/gptel-quick--spinner-tick))))

(defun bob/gptel-quick--spinner-stop ()
  "Stop the quick lookup spinner."
  (when bob/gptel-quick--spinner-timer
    (cancel-timer bob/gptel-quick--spinner-timer)
    (setq bob/gptel-quick--spinner-timer nil))
  (setq bob/gptel-quick--spinner-idx 0))

(defun bob/gptel-quick--response-map (prompt response count pos)
  "Return transient keymap for quick RESPONSE to PROMPT.

COUNT and POS are used by the longer-answer command."
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit]
                (lambda () (interactive) (bob/gptel-quick--hide)))
    (define-key map (kbd "+")
                (lambda ()
                  (interactive)
                  (bob/gptel-quick--request prompt (* count 4) pos)))
    (define-key map [remap kill-ring-save]
                (lambda ()
                  (interactive)
                  (kill-new response)
                  (message "Copied summary to kill-ring.")))
    (define-key map (kbd "M-RET")
                (lambda ()
                  (interactive)
                  (gptel (generate-new-buffer-name "*gptel-quick*") nil
                         (concat prompt "\n\n"
                                 (propertize response 'gptel 'response) "\n\n")
                         t)))
    map))

(defun bob/gptel-quick--callback (response info)
  "Handle quick lookup RESPONSE using request INFO."
  (bob/gptel-quick--spinner-stop)
  (pcase-let ((`(,prompt ,count ,pos) (plist-get info :context)))
    (pcase response
      ('nil
       (bob/gptel-quick--show
        (format "Response failed with error: %s" (plist-get info :status))
        pos bob/gptel-quick-timeout))
      ((pred stringp)
       (bob/gptel-quick--show response pos bob/gptel-quick-timeout t)
       (set-transient-map
        (bob/gptel-quick--response-map prompt response count pos)
        nil #'bob/gptel-quick--hide nil bob/gptel-quick-timeout))
      (`(tool-call . ,tool-calls)
       (bob/gptel-quick--show (format "%S" tool-calls)
                              pos bob/gptel-quick-timeout)))))

(defun bob/gptel-quick--request (prompt count pos)
  "Send PROMPT as a quick lookup request for COUNT words near POS."
  (bob/gptel-quick--pin-oauth-header)
  (bob/gptel-quick--spinner-start pos)
  (let* ((gptel-backend bob/gptel-quick-backend)
         (gptel-model bob/gptel-quick-model)
         ;; Keep this non-agentic and non-streaming.  The popup spinner
         ;; provides progress feedback while we wait for the single response.
         (gptel-use-curl nil)
         (gptel-use-tools nil)
         (gptel-tools nil)
         (gptel-use-context nil)
         (gptel-max-tokens (floor (+ (sqrt (length prompt)) (* count 2.5)))))
    (gptel-request prompt
      :system (bob/gptel-quick--system-message count)
      :context (list prompt count pos)
      :callback #'bob/gptel-quick--callback)))

;;;###autoload
(defun bob/gptel-quick (&optional prompt count)
  "Ask PROMPT via a quick gptel posframe popup.

Interactively, an empty minibuffer summarizes the active region,
PDF selection, thing at point, or buffer.  A typed question asks
that question about the same text.  With prefix argument COUNT,
use it as the approximate word budget."
  (interactive
   (let* ((text (string-trim (or (bob/gptel-quick--text-at-point) "")))
          (question (read-string "Ask (empty = summarize text): "))
          (count (or current-prefix-arg bob/gptel-quick-word-count)))
     (list (if (string-empty-p (string-trim question))
               text
             (format "Question: %s\n\nText:\n%s" question text))
           count)))
  (unless (and prompt (not (string-empty-p (string-trim prompt))))
    (user-error "No text to send"))
  (bob/gptel-quick--request prompt (or count bob/gptel-quick-word-count)
                            (bob/gptel-quick--posn)))

(provide 'bob-gptel-quick)
;;; bob-gptel-quick.el ends here
