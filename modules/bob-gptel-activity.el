;;; bob-gptel-activity.el --- Activity log + modeline spinner for gptel -*- lexical-binding: t; -*-

;; Surfaces what gptel is doing in two places so you can glance or
;; drill-down without opening the rewrite overlay or chat buffer:
;;
;; 1. `bob/gptel-activity-mode' -- a global minor mode that adds a
;;    segment to `global-mode-string' showing a spinner and the current
;;    phase ("Querying", "Tool: grep", "Receiving", ...) whenever
;;    gptel has an in-flight request or tool subprocess.  The segment
;;    hides itself when everything is idle.
;;
;; 2. `bob/gptel-activity-log' (buffer `*gptel-activity*') -- an
;;    append-only `special-mode' buffer with one line per event, tagged
;;    with timestamp and request id.  Pop it up with
;;    `bob/gptel-activity-show'.
;;
;; Everything hangs off gptel's public abnormal hooks
;; (`gptel-post-request-hook', `gptel-pre-tool-call-functions',
;; `gptel-post-tool-call-functions', `gptel-pre-response-hook',
;; `gptel-post-response-functions') and the tool-process registry from
;; `bob-gptel-tools' -- no advice, no private FSM poking.
;;
;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)

(defgroup bob-gptel-activity nil
  "Activity log and modeline spinner for gptel."
  :group 'gptel)

(defcustom bob/gptel-activity-log-max-lines 500
  "Maximum number of lines kept in `*gptel-activity*'.
Oldest lines are trimmed when the log exceeds this size."
  :type 'integer
  :group 'bob-gptel-activity)

(defcustom bob/gptel-activity-spinner-frames
  '("⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷")
  "Sequence of characters cycled as the modeline spinner."
  :type '(repeat string)
  :group 'bob-gptel-activity)

(defcustom bob/gptel-activity-spinner-interval 0.12
  "Seconds between spinner frame updates."
  :type 'number
  :group 'bob-gptel-activity)

(defcustom bob/gptel-activity-watchdog-interval 2.0
  "Seconds between reconciliation sweeps while a request is tracked.

If a tracked request's FSM disappears from `gptel--request-alist'
but `on-post-response' never fired (can happen on error/abort
paths in gptel), the watchdog logs a `Done (stale)' line and
clears state so the spinner stops."
  :type 'number
  :group 'bob-gptel-activity)

(defcustom bob/gptel-activity-args-max-chars 80
  "Maximum characters shown for tool-call argument preview in the log."
  :type 'integer
  :group 'bob-gptel-activity)

(defface bob/gptel-activity-spinner-face
  '((t :inherit mode-line-emphasis))
  "Face for the spinner in the modeline."
  :group 'bob-gptel-activity)

(defface bob/gptel-activity-phase-face
  '((t :inherit mode-line-emphasis :weight normal))
  "Face for the phase label next to the spinner."
  :group 'bob-gptel-activity)

(defface bob/gptel-activity-tool-face
  '((t :inherit font-lock-keyword-face))
  "Face for tool names in the modeline and log."
  :group 'bob-gptel-activity)

;;; State --------------------------------------------------------------

(defvar bob/gptel-activity--phase nil
  "Current activity phase symbol, or nil when idle.
One of `sending', `waiting', `tool', `receiving'.")

(defvar bob/gptel-activity--detail nil
  "Extra phase detail (e.g. current tool name), as a short string.")

(defvar bob/gptel-activity--spinner-idx 0
  "Current spinner frame index.")

(defvar bob/gptel-activity--spinner-timer nil
  "Repeating timer that advances the spinner while a phase is active.")

(defvar bob/gptel-activity--watchdog-timer nil
  "Repeating timer that reconciles tracked requests against gptel state.

Fires every `bob/gptel-activity-watchdog-interval' while any
request is tracked.  Handles cases where `on-post-response' is
never invoked (FSM error paths, etc.) so the spinner can't get
stuck.")

(defvar bob/gptel-activity--requests nil
  "Alist of in-flight requests: (FSM . PLIST).

PLIST keys: `:id', `:backend', `:model', `:buffer', `:start',
`:tool-count', `:chunks'.  Entries are added when the FSM is first
seen in `gptel--request-alist' (via `gptel-post-request-hook')
and removed in `gptel-post-response-functions'.")

(defvar bob/gptel-activity--current-request nil
  "Plist for the request whose event is currently being logged.

Bound dynamically inside hook handlers so `bob/gptel-activity--log'
can tag lines with the request id.")

(defvar bob/gptel-activity--next-id 0
  "Monotonic request id counter.")

(defun bob/gptel-activity--current-fsm ()
  "Return the FSM for the most-recently-started gptel request, or nil.

Found by inspecting `gptel--request-alist'.  Used because
`gptel-post-request-hook' runs with no arguments, and the buffer
it fires in is the process buffer, not the user's buffer."
  (when (and (boundp 'gptel--request-alist) gptel--request-alist)
    (cadr (car gptel--request-alist))))

;;; Log buffer ---------------------------------------------------------

(defconst bob/gptel-activity-buffer-name "*gptel-activity*"
  "Name of the activity log buffer.")

(defun bob/gptel-activity--buffer ()
  "Return the activity log buffer, creating it if necessary."
  (or (get-buffer bob/gptel-activity-buffer-name)
      (with-current-buffer (get-buffer-create bob/gptel-activity-buffer-name)
        (special-mode)
        (setq truncate-lines t)
        (setq-local window-point-insertion-type t)
        (current-buffer))))

(defun bob/gptel-activity--trim-log ()
  "Trim `*gptel-activity*' to `bob/gptel-activity-log-max-lines'."
  (with-current-buffer (bob/gptel-activity--buffer)
    (let ((inhibit-read-only t)
          (excess (- (count-lines (point-min) (point-max))
                     bob/gptel-activity-log-max-lines)))
      (when (> excess 0)
        (save-excursion
          (goto-char (point-min))
          (forward-line excess)
          (delete-region (point-min) (point)))))))

(defun bob/gptel-activity--log (fmt &rest args)
  "Append a timestamped line to `*gptel-activity*' with FMT and ARGS."
  (let* ((msg (apply #'format fmt args))
         (id (plist-get bob/gptel-activity--current-request :id))
         (id-tag (if id (format " #%d" id) ""))
         (ts (format-time-string "%H:%M:%S"))
         (line (format "%s%s  %s\n" ts id-tag msg)))
    (with-current-buffer (bob/gptel-activity--buffer)
      (let ((inhibit-read-only t)
            (at-end (eobp)))
        (save-excursion
          (goto-char (point-max))
          (insert line))
        ;; Auto-scroll any visible window if the user was at the tail.
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (when at-end
            (set-window-point win (point-max))))))
    (bob/gptel-activity--trim-log)))

;;;###autoload
(defun bob/gptel-activity-show ()
  "Pop up the gptel activity log."
  (interactive)
  (pop-to-buffer (bob/gptel-activity--buffer)))

;;;###autoload
(defun bob/gptel-activity-clear ()
  "Clear the gptel activity log."
  (interactive)
  (with-current-buffer (bob/gptel-activity--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;;###autoload
(defun bob/gptel-activity-reset ()
  "Force-reset all activity state.

Stops the spinner, cancels the watchdog, clears tracked requests
and blanks the modeline phase.  Does NOT abort any in-flight
gptel requests (use `bob/gptel-abort' for that), does NOT clear
the activity log.  This is the safe \"unstick the UI\" escape
hatch for when spinner state drifts out of sync with reality."
  (interactive)
  (bob/gptel-activity--spinner-stop)
  (bob/gptel-activity--watchdog-stop)
  (setq bob/gptel-activity--phase nil
        bob/gptel-activity--detail nil
        bob/gptel-activity--requests nil
        bob/gptel-activity--current-request nil)
  (force-mode-line-update t)
  (when (called-interactively-p 'interactive)
    (message "gptel-activity state reset")))

;;; Spinner ------------------------------------------------------------

(defun bob/gptel-activity--spinner-frame ()
  "Return the current spinner glyph as a propertized string."
  (propertize (nth (mod bob/gptel-activity--spinner-idx
                        (length bob/gptel-activity-spinner-frames))
                   bob/gptel-activity-spinner-frames)
              'face 'bob/gptel-activity-spinner-face))

(defun bob/gptel-activity--spinner-advance ()
  "Advance the spinner index and refresh the modeline."
  (cl-incf bob/gptel-activity--spinner-idx)
  (force-mode-line-update t))

(defun bob/gptel-activity--spinner-start ()
  "Start the spinner timer if not already running."
  (unless bob/gptel-activity--spinner-timer
    (setq bob/gptel-activity--spinner-timer
          (run-at-time 0 bob/gptel-activity-spinner-interval
                       #'bob/gptel-activity--spinner-advance))))

(defun bob/gptel-activity--spinner-stop ()
  "Stop the spinner timer."
  (when bob/gptel-activity--spinner-timer
    (cancel-timer bob/gptel-activity--spinner-timer)
    (setq bob/gptel-activity--spinner-timer nil))
  (setq bob/gptel-activity--spinner-idx 0)
  (force-mode-line-update t))

;;; Watchdog ----------------------------------------------------------

(defun bob/gptel-activity--watchdog-tick ()
  "Reconcile tracked requests against `gptel--request-alist'.

Remove any tracked entry whose FSM is no longer in
`gptel--request-alist' (i.e. gptel has finished with it, perhaps
via an error or abort path that didn't run
`gptel-post-response-functions').  Stop the watchdog itself when
there is nothing left to track."
  (condition-case err
      (let* ((live-fsms (when (and (boundp 'gptel--request-alist)
                                   gptel--request-alist)
                          (mapcar (lambda (entry) (cadr entry))
                                  gptel--request-alist)))
             (stale (cl-remove-if
                     (lambda (entry) (memq (car entry) live-fsms))
                     bob/gptel-activity--requests)))
        (dolist (entry stale)
          (let* ((bob/gptel-activity--current-request (cdr entry))
                 (start (plist-get (cdr entry) :start))
                 (dur (and start (- (float-time) start))))
            (bob/gptel-activity--log "← Done (stale) %s"
                                     (if dur (format "(%.1fs)" dur) "")))
          (setq bob/gptel-activity--requests
                (delq entry bob/gptel-activity--requests)))
        (when (null bob/gptel-activity--requests)
          (bob/gptel-activity--set-phase nil)
          (setq bob/gptel-activity--current-request nil)
          (bob/gptel-activity--watchdog-stop)))
    (error
     (message "gptel-activity watchdog error: %S" err)
     ;; Don't let a broken tick leave the watchdog running forever.
     (bob/gptel-activity--watchdog-stop)
     (bob/gptel-activity--spinner-stop))))

(defun bob/gptel-activity--watchdog-start ()
  "Start the watchdog timer if not already running."
  (unless bob/gptel-activity--watchdog-timer
    (setq bob/gptel-activity--watchdog-timer
          (run-at-time bob/gptel-activity-watchdog-interval
                       bob/gptel-activity-watchdog-interval
                       #'bob/gptel-activity--watchdog-tick))))

(defun bob/gptel-activity--watchdog-stop ()
  "Stop the watchdog timer."
  (when bob/gptel-activity--watchdog-timer
    (cancel-timer bob/gptel-activity--watchdog-timer)
    (setq bob/gptel-activity--watchdog-timer nil)))

;;; Phase management ---------------------------------------------------

(defconst bob/gptel-activity--phase-labels
  '((sending   . "Sending")
    (waiting   . "Querying")
    (tool      . "Tool")
    (receiving . "Receiving"))
  "Human-readable labels for each phase symbol.")

(defun bob/gptel-activity--set-phase (phase &optional detail)
  "Set the current activity PHASE and optional DETAIL string.

Starts/stops the spinner AND the watchdog timer in lockstep."
  (setq bob/gptel-activity--phase phase
        bob/gptel-activity--detail detail)
  (cond
   (phase
    (bob/gptel-activity--spinner-start)
    (bob/gptel-activity--watchdog-start))
   (t
    (bob/gptel-activity--spinner-stop)
    (bob/gptel-activity--watchdog-stop)))
  (force-mode-line-update t))

(defun bob/gptel-activity--modeline-segment ()
  "Return the propertized modeline segment, or an empty string when idle."
  (if (not bob/gptel-activity--phase)
      ""
    (let* ((label (or (alist-get bob/gptel-activity--phase
                                 bob/gptel-activity--phase-labels)
                      (symbol-name bob/gptel-activity--phase)))
           (detail bob/gptel-activity--detail)
           (tool-bit (when (and (eq bob/gptel-activity--phase 'tool) detail)
                       (concat ":"
                               (propertize detail 'face
                                           'bob/gptel-activity-tool-face)))))
      (concat " "
              (bob/gptel-activity--spinner-frame)
              " "
              (propertize label 'face 'bob/gptel-activity-phase-face)
              (or tool-bit "")))))

;;; Hook handlers ------------------------------------------------------

(defun bob/gptel-activity--short-args (args)
  "Stringify tool ARGS plist, truncated to a short preview."
  (let* ((s (prin1-to-string args))
         (max bob/gptel-activity-args-max-chars))
    (if (<= (length s) max)
        s
      (concat (substring s 0 (max 0 (- max 1))) "…"))))

(defmacro bob/gptel-activity--safe-hook (hook-name &rest body)
  "Run BODY inside a `condition-case' labelled HOOK-NAME.

Errors are demoted to a message so a broken hook can't leave the
activity module's state half-updated (and the spinner stuck).
The watchdog will still eventually reconcile."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "gptel-activity %s hook error: %S" ,hook-name err))))

(defun bob/gptel-activity--on-post-request ()
  "Hook: gptel entered the WAIT state.

This fires at the start of every request AND after each tool-use
cycle when the FSM transitions back to WAIT for a follow-up call.
We log a fresh `Request sent' line only on the first visit; later
visits just flip the phase back to `waiting'."
  (bob/gptel-activity--safe-hook 'post-request
    (let* ((fsm (bob/gptel-activity--current-fsm))
           (existing (and fsm (alist-get fsm bob/gptel-activity--requests))))
      (cond
       ;; Already tracked: this is a post-tool re-request.  Don't log.
       (existing
        (setq bob/gptel-activity--current-request existing)
        (bob/gptel-activity--set-phase 'waiting))
       ;; Brand-new request.
       (t
        (let* ((info (and fsm (gptel-fsm-info fsm)))
               (buf-name (or (plist-get info :buffer)
                             ;; Last resort: buffer we're running in.
                             (buffer-name)))
               (buf-name (if (bufferp buf-name) (buffer-name buf-name)
                           buf-name))
               (backend-obj (and info (plist-get info :backend)))
               (backend (cond ((gptel-backend-p backend-obj)
                               (gptel-backend-name backend-obj))
                              ((and (boundp 'gptel-backend) gptel-backend)
                               (gptel-backend-name gptel-backend))))
               (model-sym (and info (plist-get info :model)))
               (model (cond (model-sym (gptel--model-name model-sym))
                            ((and (boundp 'gptel-model) gptel-model)
                             (gptel--model-name gptel-model))))
               (id (cl-incf bob/gptel-activity--next-id))
               (plist (list :id id :backend backend :model model
                            :buffer buf-name :start (float-time)
                            :tool-count 0 :chunks 0)))
          (when fsm
            (push (cons fsm plist) bob/gptel-activity--requests))
          (setq bob/gptel-activity--current-request plist)
          (bob/gptel-activity--log "→ Request sent  [%s/%s]  in %s"
                                   (or backend "?") (or model "?") buf-name)
          (bob/gptel-activity--set-phase 'waiting)))))))

(defun bob/gptel-activity--request-for-plist (plist)
  "Return the tracked request-plist that matches hook-arg PLIST.

Uses the hook plist's `:buffer' (which is a buffer name string
provided by gptel) to find the matching entry in
`bob/gptel-activity--requests'.  Falls back to the
most-recently-added entry."
  (let ((hook-buf (plist-get plist :buffer)))
    (or (cdr
         (cl-find-if
          (lambda (entry)
            (let ((tracked-buf (plist-get (cdr entry) :buffer)))
              (and hook-buf tracked-buf
                   (string= hook-buf tracked-buf))))
          bob/gptel-activity--requests))
        (cdr (car bob/gptel-activity--requests)))))

(defun bob/gptel-activity--on-pre-tool-call (plist)
  "Hook: a tool call is about to run.  PLIST carries :name, :args."
  (bob/gptel-activity--safe-hook 'pre-tool-call
    (let* ((name (plist-get plist :name))
           (args (plist-get plist :args))
           (bob/gptel-activity--current-request
            (or (bob/gptel-activity--request-for-plist plist)
                bob/gptel-activity--current-request)))
      (when bob/gptel-activity--current-request
        (plist-put bob/gptel-activity--current-request :tool-count
                   (1+ (or (plist-get bob/gptel-activity--current-request
                                      :tool-count)
                           0))))
      (bob/gptel-activity--log "  ↳ Tool %s(%s)"
                               (propertize (or name "?") 'face
                                           'bob/gptel-activity-tool-face)
                               (bob/gptel-activity--short-args args))
      (bob/gptel-activity--set-phase 'tool name)))
  nil)

(defun bob/gptel-activity--on-post-tool-call (plist)
  "Hook: a tool call just finished.  PLIST carries :name, :result."
  (bob/gptel-activity--safe-hook 'post-tool-call
    (let* ((name (plist-get plist :name))
           (result (plist-get plist :result))
           (size (if (stringp result) (length result) 0))
           (bob/gptel-activity--current-request
            (or (bob/gptel-activity--request-for-plist plist)
                bob/gptel-activity--current-request)))
      (bob/gptel-activity--log "    ← %s → %d byte%s"
                               (propertize (or name "?") 'face
                                           'bob/gptel-activity-tool-face)
                               size (if (= size 1) "" "s"))
      ;; Don't flip phase back here -- gptel will either issue
      ;; another tool call, start streaming the response, or finish.
      ;; The pre/post response hooks handle those transitions.
      ))
  nil)

(defun bob/gptel-activity--on-pre-response ()
  "Hook: the response is about to be inserted.  Switch to `receiving'."
  (bob/gptel-activity--safe-hook 'pre-response
    (bob/gptel-activity--set-phase 'receiving)))

(defun bob/gptel-activity--on-post-response (_beg _end)
  "Hook: the request finished (DONE, ERRS, or ABRT).

Finds which tracked request just completed by looking for any
FSM in `bob/gptel-activity--requests' that is no longer in
`gptel--request-alist'.  Logs a `Done' line for each.  The
watchdog catches any stragglers."
  (bob/gptel-activity--safe-hook 'post-response
    (let* ((live-fsms (when (and (boundp 'gptel--request-alist)
                                 gptel--request-alist)
                        (mapcar (lambda (entry) (cadr entry))
                                gptel--request-alist)))
           (completed (cl-remove-if
                       (lambda (entry) (memq (car entry) live-fsms))
                       bob/gptel-activity--requests)))
      (dolist (entry completed)
        (let* ((bob/gptel-activity--current-request (cdr entry))
               (start (plist-get (cdr entry) :start))
               (dur (and start (- (float-time) start)))
               (tools (or (plist-get (cdr entry) :tool-count) 0)))
          (bob/gptel-activity--log "← Done  %s%s"
                                   (if dur (format "(%.1fs)" dur) "")
                                   (if (> tools 0)
                                       (format ", %d tool call%s"
                                               tools (if (= tools 1) "" "s"))
                                     ""))
          (setq bob/gptel-activity--requests
                (delq entry bob/gptel-activity--requests))))
      (when (null bob/gptel-activity--requests)
        (bob/gptel-activity--set-phase nil)
        (setq bob/gptel-activity--current-request nil)))))

;;; Mode ---------------------------------------------------------------

(defvar bob/gptel-activity--modeline-construct
  '(:eval (bob/gptel-activity--modeline-segment))
  "`global-mode-string' construct evaluated each modeline refresh.")

;;;###autoload
(define-minor-mode bob/gptel-activity-mode
  "Global minor mode showing gptel activity in the modeline and a log.

When enabled, adds a spinner + phase indicator to the modeline via
`global-mode-string' and logs every request / tool call / response
to the `*gptel-activity*' buffer.  Use
`bob/gptel-activity-show' to inspect the log."
  :global t
  :group 'bob-gptel-activity
  :lighter nil
  (cond
   (bob/gptel-activity-mode
    (add-to-list 'global-mode-string
                 bob/gptel-activity--modeline-construct t)
    (add-hook 'gptel-post-request-hook       #'bob/gptel-activity--on-post-request)
    (add-hook 'gptel-pre-tool-call-functions #'bob/gptel-activity--on-pre-tool-call)
    (add-hook 'gptel-post-tool-call-functions #'bob/gptel-activity--on-post-tool-call)
    (add-hook 'gptel-pre-response-hook       #'bob/gptel-activity--on-pre-response)
    (add-hook 'gptel-post-response-functions #'bob/gptel-activity--on-post-response))
   (t
    (setq global-mode-string
          (delq bob/gptel-activity--modeline-construct global-mode-string))
    (remove-hook 'gptel-post-request-hook       #'bob/gptel-activity--on-post-request)
    (remove-hook 'gptel-pre-tool-call-functions #'bob/gptel-activity--on-pre-tool-call)
    (remove-hook 'gptel-post-tool-call-functions #'bob/gptel-activity--on-post-tool-call)
    (remove-hook 'gptel-pre-response-hook       #'bob/gptel-activity--on-pre-response)
    (remove-hook 'gptel-post-response-functions #'bob/gptel-activity--on-post-response)
    (bob/gptel-activity--spinner-stop)
    (bob/gptel-activity--watchdog-stop)
    (bob/gptel-activity--set-phase nil)
    (setq bob/gptel-activity--requests nil
          bob/gptel-activity--current-request nil))))

(provide 'bob-gptel-activity)
;;; bob-gptel-activity.el ends here
