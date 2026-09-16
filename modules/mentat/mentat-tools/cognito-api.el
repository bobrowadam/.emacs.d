;;; cognito-api.el --- Read-only Bradwell Cognito inspection -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'mentat-elisp-library)

(defconst mentat-cognito--environments
  '(("dev" "bradwell-dev")
    ("prod" "bradwell-prod"))
  "Bradwell Cognito environments and AWS profiles.")

(defun mentat-cognito--json (text)
  "Parse AWS JSON TEXT as alists and lists."
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object :json-false))

(defun mentat-cognito--attribute (attributes name)
  "Return NAME from Cognito ATTRIBUTES."
  (alist-get 'Value
             (seq-find (lambda (attribute)
                         (equal (alist-get 'Name attribute) name))
                       attributes)))

(defun mentat-cognito--starter (environment email)
  "Return an async Cognito user lookup starter for ENVIRONMENT and EMAIL."
  (let ((config (assoc environment mentat-cognito--environments)))
    (unless config (error "ENVIRONMENT must be dev or prod"))
    (lambda (resolve reject on-cancel)
      (let ((aws (executable-find "aws"))
            (profile (nth 1 config))
            process buffer timer done)
        (unless aws (error "AWS CLI is unavailable"))
        (cl-labels
            ((cleanup ()
               (when (timerp timer) (cancel-timer timer))
               (when (process-live-p process) (delete-process process))
               (when (buffer-live-p buffer) (kill-buffer buffer)))
             (finish (callback value)
               (unless done
                 (setq done t)
                 (cleanup)
                 (funcall callback value)))
             (fail (stage)
               (finish reject
                       (format "Cognito %s read failed during %s; check AWS SSO access"
                               environment stage)))
             (run (stage args callback)
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (setq buffer (generate-new-buffer " *cognito-api*"))
               (condition-case nil
                   (setq process
                         (make-process
                          :name "cognito-api"
                          :buffer buffer
                          :stderr buffer
                          :command (append (list aws "--profile" profile
                                                 "--region" "us-east-1")
                                           args
                                           '("--no-cli-pager"))
                          :connection-type 'pipe
                          :noquery t
                          :sentinel
                          (lambda (completed _event)
                            (when (and (not done)
                                       (memq (process-status completed) '(exit signal)))
                              (if (not (zerop (process-exit-status completed)))
                                  (fail stage)
                                (condition-case nil
                                    (funcall callback
                                             (with-current-buffer buffer
                                               (buffer-substring-no-properties
                                                (point-min) (point-max))))
                                  (error (finish reject
                                                 (format "Cognito %s returned invalid data during %s"
                                                         environment stage)))))))))
                 (error (fail stage))))
             (list-groups (pool user summary)
               (run "group lookup"
                    (list "cognito-idp" "admin-list-groups-for-user"
                          "--user-pool-id" pool "--username" user "--output" "json")
                    (lambda (text)
                      (let* ((payload (mentat-cognito--json text))
                             (groups (mapcar (lambda (group) (alist-get 'GroupName group))
                                             (alist-get 'Groups payload))))
                        (finish resolve
                                (append summary `((groups . ,groups))))))))
             (find-user (pool)
               (run "user lookup"
                    (list "cognito-idp" "list-users"
                          "--user-pool-id" pool
                          "--filter" (format "email = \"%s\"" email)
                          "--output" "json")
                    (lambda (text)
                      (let* ((payload (mentat-cognito--json text))
                             (users (alist-get 'Users payload))
                             (user (car users)))
                        (if (null user)
                            (finish resolve '((found . :json-false)))
                          (let* ((attributes (alist-get 'Attributes user))
                                 (username (alist-get 'Username user))
                                 (summary
                                  `((found . t)
                                    (sub . ,username)
                                    (name . ,(mentat-cognito--attribute attributes "name"))
                                    (given-name . ,(mentat-cognito--attribute attributes "given_name"))
                                    (family-name . ,(mentat-cognito--attribute attributes "family_name"))
                                    (status . ,(alist-get 'UserStatus user))
                                    (enabled . ,(alist-get 'Enabled user))
                                    (created . ,(alist-get 'UserCreateDate user))
                                    (email-verified . ,(mentat-cognito--attribute attributes "email_verified")))))
                            (list-groups pool username summary)))))))
             (read-pool ()
               (run "pool lookup"
                    (list "ssm" "get-parameter"
                          "--name" (format "/bradwell/%s/cognito-user-pool-id" environment)
                          "--query" "Parameter.Value" "--output" "text")
                    (lambda (text)
                      (let ((pool (string-trim text)))
                        (unless (string-match-p "\\`[[:alnum:]_-]+\\'" pool)
                          (error "Invalid pool ID"))
                        (find-user pool))))))
          (setq timer (run-at-time 60 nil
                                   (lambda ()
                                     (finish reject
                                             (format "Cognito %s read timed out" environment)))))
          (funcall on-cancel (lambda () (setq done t) (cleanup)))
          (read-pool))))))

(mentat-defun mentat-cognito-inspect-user-by-email (environment email)
  "Inspect a Bradwell Cognito user by EMAIL in dev or prod ENVIRONMENT.
Return bounded identity, account-status, verification, and group fields.
The email is used for lookup but omitted from the result.  This tool is read-only."
  (:execution async)
  (unless (and (stringp email)
               (<= (length email) 320)
               (string-match-p "\\`[^[:space:]@]+@[^[:space:]@]+\\'" email))
    (error "EMAIL must be a valid non-empty email address"))
  (mentat-cognito--starter environment email))

(provide 'cognito-api)
;;; cognito-api.el ends here
