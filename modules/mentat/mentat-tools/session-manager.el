;;; session-manager.el --- Shared Mentat session support -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'subr-x)

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

(defconst mentat-session-manager--list-limit 100
  "Maximum sessions returned by `mentat-session-list'.")

(defun mentat-session-manager--entry-result (entry)
  "Return bounded metadata for registered session ENTRY."
  (let* ((session-id (mentat--registry-entry-session-id entry))
         (metadata (mentat--open-session-file-metadata
                    (mentat--registry-entry-session-file entry)))
         (view (mentat--open-live-view session-id))
         (buffer (and view (mentat--buffer-buffer view)))
         (modified (mentat--open-entry-modified-time entry))
         (last-activity
          (and modified
               (format-time-string "%Y-%m-%dT%H:%M:%SZ" modified t))))
    `((session-id . ,session-id)
      (name . ,(or (plist-get metadata :name)
                    (plist-get metadata :preview)
                    "Unnamed session"))
      (directory . ,(mentat--registry-entry-root entry))
      (status . ,(if view (mentat--buffer-status view) "closed"))
      (live . ,(and view t))
      (last-activity . ,last-activity)
      ,@(when (buffer-live-p buffer)
          `((buffer . ,(buffer-name buffer)))))))

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

(provide 'session-manager)
;;; session-manager.el ends here
