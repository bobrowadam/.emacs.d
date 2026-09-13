;;; bob-customerio.el --- Customer.io App API access -*- lexical-binding: t; -*-

;;; Commentary:
;; Read Customer.io workspace and transactional template data.
;; Credentials come from the selected Bradwell AWS SSM parameter.
;; No API keys or raw authentication failures are returned to callers.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(defvar bob/customerio-environments
  '(("dev" . ("bradwell-dev" "us-east-1" "/bradwell/dev/customerio-app-api-key"))
    ("prod" . ("bradwell-prod" "us-east-1" "/bradwell/prod/customerio-app-api-key")))
  "Environment names mapped to AWS profile, region and SSM parameter.")

(defun bob/customerio-path (operation &optional id)
  "Return the read-only API path for OPERATION and optional template ID."
  (unless (member operation '("workspaces" "templates" "template" "contents"))
    (error "Unsupported Customer.io operation"))
  (if (member operation '("workspaces" "templates"))
      (if (equal operation "workspaces") "/v1/workspaces" "/v1/transactional")
    (unless (and (integerp id) (> id 0))
      (error "Template ID must be a positive integer"))
    (format "/v1/transactional/%d%s" id
            (if (equal operation "contents") "/contents" ""))))

(defun bob/customerio-read (environment operation id resolve reject)
  "Read OPERATION for ENVIRONMENT and optional template ID asynchronously.
Call RESOLVE with decoded JSON or REJECT with a credential-safe error.
Return a cancellation function.  Requests time out after 60 seconds.
JSON false is `:false'; JSON null is `:null'; arrays are vectors."
  (let* ((config (cdr (assoc environment bob/customerio-environments)))
         (path (bob/customerio-path operation id))
         (aws (executable-find "aws"))
         (credential-buffer nil) (process nil) (response-buffer nil)
         (timer nil) (done nil))
    (unless config (error "Unknown Customer.io environment"))
    (unless aws (error "AWS CLI is unavailable"))
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer))
           (when (and process (process-live-p process)) (delete-process process))
           (dolist (buffer (list credential-buffer response-buffer))
             (when (buffer-live-p buffer) (kill-buffer buffer))))
         (finish (callback value)
           (unless done
             (setq done t)
             (cleanup)
             (funcall callback value)))
         (fetch (key)
           (let ((url-request-method "GET")
                 (url-request-extra-headers
                  `(("Authorization" . ,(concat "Bearer " key))
                    ("Accept" . "application/json")))
                 (url-max-redirections 0)
                 (url-show-status nil))
             (setq response-buffer
                   (url-retrieve
                    (concat "https://api.customer.io" path)
                    (lambda (status)
                      (unless done
                        (let ((http url-http-response-status))
                          (if (or (plist-get status :error)
                                  (not (and (integerp http) (<= 200 http) (< http 300))))
                              (finish reject (format "Customer.io %s %s failed (HTTP %s)"
                                                     environment operation (or http "unavailable")))
                            (condition-case nil
                                (let ((data
                                       (progn
                                         (goto-char url-http-end-of-headers)
                                         (json-parse-buffer :object-type 'alist
                                                            :null-object :null
                                                            :false-object :false))))
                                  (finish resolve data))
                              (error (finish reject "Customer.io returned invalid JSON")))))))
                    nil t t)))))
      (setq credential-buffer (generate-new-buffer " *customerio-credential*"))
      (setq timer (run-at-time 60 nil (lambda () (finish reject "Customer.io request timed out"))))
      (condition-case nil
          (setq process
                (make-process
                 :name "customerio-credential" :buffer credential-buffer
                 :command (list aws "--profile" (nth 0 config) "--region" (nth 1 config)
                                "ssm" "get-parameter" "--name" (nth 2 config)
                                "--with-decryption" "--output" "json" "--no-cli-pager")
                 :connection-type 'pipe :noquery t
                 :sentinel
                 (lambda (proc _event)
                   (when (and (not done) (memq (process-status proc) '(exit signal)))
                     (if (not (zerop (process-exit-status proc)))
                         (finish reject (format "AWS credential retrieval failed for %s; check SSO access" environment))
                       (condition-case nil
                           (let ((key (with-current-buffer credential-buffer
                                        (goto-char (point-min))
                                        (alist-get 'Value
                                                   (alist-get 'Parameter
                                                              (json-parse-buffer :object-type 'alist))))))
                             (kill-buffer credential-buffer)
                             (unless (and (stringp key) (> (length key) 0))
                               (error "Missing credential"))
                             (fetch key))
                         (error (finish reject "Customer.io authentication or request initialization failed"))))))))
        (error (finish reject "Unable to start Customer.io credential retrieval")))
      (lambda () (unless done (setq done t) (cleanup))))))

(provide 'bob-customerio)
;;; bob-customerio.el ends here
