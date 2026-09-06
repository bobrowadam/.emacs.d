;;; session-manager.el --- Create native Mentat sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Registered tools for creating and reloading undisplayed Mentat sessions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mentat-elisp-library)
(require 'mentat-buffer)
(require 'mentat-prompt)

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

(provide 'session-manager)
;;; session-manager.el ends here
