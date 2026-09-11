;;; session-manager.el --- Create native Mentat sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Registered tools for creating and reloading undisplayed Mentat sessions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mentat-elisp-library)
(require 'mentat-buffer)
(require 'mentat-prompt)
(require 'mentat-registry)
(require 'mentat-ui)
(require 'seq)

(defun mentat-session-manager--result (buffer state directory request-id)
  "Return bounded session metadata for BUFFER and Pi STATE in DIRECTORY.
REQUEST-ID identifies the optional initial handoff submission."
  (let ((result
         `((session-id . ,(alist-get 'sessionId state))
           (buffer . ,(buffer-name buffer))
           (directory . ,directory))))
    (if request-id
        (append result `((handoff-request-id . ,request-id)))
      result)))

(defun mentat-session-manager--starter (directory name handoff)
  "Return a callback starter creating a Mentat session in DIRECTORY.
NAME optionally names the session.  HANDOFF is submitted once Pi is ready."
  (lambda (resolve reject on-cancel)
    (let ((root (file-name-as-directory (file-truename directory)))
          buffer
          settled)
      (cl-labels
          ((dispose ()
             (when (buffer-live-p buffer)
               (kill-buffer buffer)))
           (succeed (value)
             (unless settled
               (setq settled t)
               (funcall resolve value)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (dispose)
               (funcall reject reason)))
           (cancel ()
             (unless settled
               (setq settled t)
               (dispose))))
        (funcall on-cancel #'cancel)
        (condition-case err
            (setq buffer
                  (mentat-start-session
                   root
                   :name name
                   :ready-handler
                   (lambda (ready-buffer state)
                     (setq buffer ready-buffer)
                     (if (and handoff (not (string-blank-p handoff)))
                         (with-current-buffer ready-buffer
                           (if-let* ((request-id (mentat-submit handoff)))
                               (succeed
                                (mentat-session-manager--result
                                 ready-buffer state root request-id))
                             (fail
                              "Mentat session started but the handoff could not be submitted")))
                       (succeed
                        (mentat-session-manager--result
                         ready-buffer state root nil))))
                   :error-handler
                   (lambda (failed-buffer response)
                     (setq buffer failed-buffer)
                     (fail (or (alist-get 'error response)
                               "Mentat session startup failed")))))
          (error
           (fail (error-message-string err))))))))

(mentat-defun mentat-session-create (directory &optional name handoff)
  "Create a native Mentat session rooted in DIRECTORY.

NAME optionally labels the persisted Pi session.  HANDOFF, when nonblank, is
submitted as the first prompt after Pi reports ready.  The new conversation
buffer is created without displaying it.  Resolve with the session ID, buffer,
root directory, and optional handoff request ID once startup succeeds."
  (:execution async)
  (unless (and (stringp directory) (file-directory-p directory))
    (user-error "Mentat session directory does not exist: %S" directory))
  (when (and name
             (or (not (stringp name)) (string-blank-p name)))
    (user-error "Mentat session name must be a nonblank string"))
  (when (and handoff (not (stringp handoff)))
    (user-error "Mentat session handoff must be a string"))
  (mentat-session-manager--starter directory name handoff))

(defun mentat-session-manager--reload-starter (buffer)
  "Return a callback starter that reloads the Mentat session in BUFFER."
  (lambda (resolve reject on-cancel)
    (let (settled)
      (cl-labels
          ((succeed (value)
             (unless settled
               (setq settled t)
               (funcall resolve value)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (funcall reject reason)))
           (cancel ()
             (setq settled t)))
        (funcall on-cancel #'cancel)
        (condition-case err
            (with-current-buffer buffer
              (mentat-reload
               (lambda (ready-buffer state)
                 (succeed
                  (mentat-session-manager--result
                   ready-buffer state
                   (file-name-as-directory
                    (file-truename
                     (buffer-local-value 'default-directory ready-buffer)))
                   nil)))
               (lambda (_failed-buffer reason)
                 (fail reason))))
          (error
           (fail (error-message-string err))))))))

(mentat-defun mentat-session-reload (buffer)
  "Reload the idle Mentat session in BUFFER.

Restart the session's Pi process so changes to Pi extensions take effect, then
resolve after Mentat restores the persisted conversation history.  BUFFER must
name a live Mentat conversation other than a session currently executing a
prompt."
  (:execution async)
  (unless (stringp buffer)
    (user-error "Mentat session buffer must be a string"))
  (let ((conversation (get-buffer buffer)))
    (unless (buffer-live-p conversation)
      (user-error "No live Mentat session buffer: %s" buffer))
    (with-current-buffer conversation
      (unless (derived-mode-p 'mentat-buffer-mode)
        (user-error "Buffer is not a Mentat conversation: %s" buffer)))
    (mentat-session-manager--reload-starter conversation)))

(defconst mentat-session-manager--list-limit 100
  "Maximum sessions returned by `mentat-session-list'.")

(defun mentat-session-manager--entry-result (entry)
  "Return bounded metadata for registered session ENTRY."
  (let* ((session-id (mentat--registry-entry-session-id entry))
         (metadata (mentat--open-entry-metadata entry))
         (view (mentat--open-live-view session-id))
         (buffer (and view (mentat--buffer-buffer view)))
         (modified (plist-get metadata :modified))
         (last-activity
          (and modified
               (format-time-string "%Y-%m-%dT%H:%M:%SZ" modified t))))
    `((session-id . ,session-id)
      (name . ,(plist-get metadata :name))
      (directory . ,(mentat--registry-entry-root entry))
      (status . ,(if view (mentat--buffer-status view) "closed"))
      (live . ,(and view t))
      (last-activity . ,last-activity)
      ,@(when (buffer-live-p buffer)
          `((buffer . ,(buffer-name buffer)))))))

(mentat-defun mentat-session-list (&optional directory)
  "List registered Mentat sessions, optionally restricted to DIRECTORY.

Return at most 100 sessions with stable IDs, names, roots, status, activity
time, and live buffer names.  DIRECTORY must name an existing project root."
  (when (and directory
             (or (not (stringp directory))
                 (not (file-directory-p directory))))
    (user-error "Mentat session directory does not exist: %S" directory))
  (let* ((root (and directory
                    (file-name-as-directory (file-truename directory))))
         (entries (mentat--registry-list root)))
    (mapcar #'mentat-session-manager--entry-result
            (seq-take entries mentat-session-manager--list-limit))))

(defun mentat-session-manager--send-starter (entry prompt)
  "Return a callback starter that sends PROMPT to registered session ENTRY."
  (lambda (resolve reject on-cancel)
    (let ((session-id (mentat--registry-entry-session-id entry))
          buffer
          resumed
          submitted
          settled)
      (cl-labels
          ((succeed (value)
             (unless settled
               (setq settled t)
               (funcall resolve value)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (funcall reject reason)))
           (cancel ()
             (unless settled
               (setq settled t)
               (when (and resumed (not submitted) (buffer-live-p buffer))
                 (kill-buffer buffer))))
           (send (view)
             (setq buffer (mentat--buffer-buffer view))
             (if (not (equal "idle" (mentat--buffer-status view)))
                 (fail (format "Mentat session is not idle: %s" session-id))
               (condition-case err
                   (let ((request-id
                          (mentat--buffer-submit
                           view prompt nil
                           (lambda (_session response)
                             (succeed
                              `((session-id . ,session-id)
                                (request-id . ,(alist-get 'id response))
                                (buffer . ,(buffer-name buffer))
                                (resumed . ,resumed))))
                           (lambda (_session response)
                             (fail (or (alist-get 'error response)
                                       "Mentat prompt was rejected"))))))
                     (setq submitted (and request-id t)))
                 (error (fail (error-message-string err)))))))
        (funcall on-cancel #'cancel)
        (condition-case err
            (if-let* ((view (mentat--open-live-view session-id)))
                (send view)
              (setq resumed t)
              (mentat--open-entry-view
               entry
               (lambda (view _state) (send view))
               (lambda (_view response)
                 (fail (or (alist-get 'error response)
                           "Mentat session resume failed"))))))
          (error (fail (error-message-string err)))))))

(mentat-defun mentat-session-send (session-id prompt)
  "Send PROMPT to the idle registered Mentat SESSION-ID.

Reuse a live session or resume a closed session without displaying it.  Reject
busy sessions instead of steering or queueing.  Resolve after Pi accepts the
prompt with the session ID, request ID, buffer name, and resume status."
  (:execution async)
  (unless (and (stringp session-id) (not (string-blank-p session-id)))
    (user-error "Mentat session ID must be a nonblank string"))
  (unless (and (stringp prompt) (not (string-blank-p prompt)))
    (user-error "Mentat session prompt must be a nonblank string"))
  (let ((entry
         (cl-find-if
          (lambda (candidate)
            (equal session-id
                   (mentat--registry-entry-session-id candidate)))
          (mentat--registry-list))))
    (unless entry
      (user-error "No registered Mentat session: %s" session-id))
    (mentat-session-manager--send-starter entry prompt)))

(provide 'session-manager)
;;; session-manager.el ends here
