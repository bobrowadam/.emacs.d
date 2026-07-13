;;; bob-aws.el --- AWS command helpers -*- lexical-binding: t; -*-

(defconst bob/aws-sso-profiles '("bradwell-dev" "bradwell-prod")
  "AWS IAM Identity Center profiles available for interactive login.")

(defun bob/aws-sso-login (profile)
  "Log in to AWS IAM Identity Center using PROFILE.

Display the command output in a profile-specific buffer."
  (interactive
   (list (completing-read "AWS SSO profile: " bob/aws-sso-profiles nil t)))
  (unless (executable-find "aws")
    (user-error "AWS CLI executable not found"))
  (let ((buffer (get-buffer-create (format "*aws-sso:%s*" profile))))
    (when-let* ((process (get-buffer-process buffer)))
      (when (process-live-p process)
        (user-error "AWS SSO login is already running for %s" profile)))
    (with-current-buffer buffer
      (erase-buffer))
    (make-process
     :name (format "aws-sso-%s" profile)
     :buffer buffer
     :command (list "aws" "sso" "login" "--profile" profile)
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (message "AWS SSO login %s for %s (see %s)"
                  (if (zerop (process-exit-status process)) "completed" "failed")
                  profile
                  (buffer-name (process-buffer process))))))
    (display-buffer buffer)
    (message "Starting AWS SSO login for %s" profile)))

(defun bob/aws-sso-login-dev ()
  "Log in to the Bradwell development AWS profile."
  (interactive)
  (bob/aws-sso-login "bradwell-dev"))

(defun bob/aws-sso-login-prod ()
  "Log in to the Bradwell production AWS profile."
  (interactive)
  (bob/aws-sso-login "bradwell-prod"))

(defvar-keymap bob/aws-sso-prefix-map
  :doc "Keymap for AWS SSO commands."
  "d" #'bob/aws-sso-login-dev
  "p" #'bob/aws-sso-login-prod)

(global-set-key (kbd "C-c A") bob/aws-sso-prefix-map)

(provide 'bob-aws)

;;; bob-aws.el ends here
