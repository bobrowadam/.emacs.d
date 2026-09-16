;;; langfuse-api.el --- Read-only Langfuse inspection -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'seq)
(require 'subr-x)
(require 'mentat-elisp-library)

(defconst mentat-langfuse--environments
  '(("dev" "bradwell-dev" "https://langfuse.dev.gist.legal")
    ("prod" "bradwell-prod" "https://langfuse.gist.legal"))
  "Langfuse environment, AWS profile, and API URL mappings.")

(defun mentat-langfuse--get (object key)
  "Return KEY from JSON alist OBJECT."
  (when (listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'equal))))

(defun mentat-langfuse--valid-time-p (value)
  "Return non-nil when VALUE is an ISO 8601 timestamp."
  (and (stringp value)
       (condition-case nil
           (progn (date-to-time value) t)
         (error nil))))

(defun mentat-langfuse--observation-summary (observation)
  "Return safe bounded fields from OBSERVATION."
  `((id . ,(mentat-langfuse--get observation 'id))
    (trace-id . ,(mentat-langfuse--get observation 'traceId))
    (name . ,(mentat-langfuse--get observation 'name))
    (type . ,(mentat-langfuse--get observation 'type))
    (start-time . ,(or (mentat-langfuse--get observation 'startTime)
                       (mentat-langfuse--get observation 'timestamp)))
    (end-time . ,(mentat-langfuse--get observation 'endTime))
    (level . ,(mentat-langfuse--get observation 'level))
    (status-message . ,(mentat-langfuse--get observation 'statusMessage))
    (model . ,(mentat-langfuse--get observation 'model))
    (total-cost . ,(mentat-langfuse--get observation 'calculatedTotalCost))))

(defun mentat-langfuse--trace-summary (trace)
  "Return safe bounded fields from TRACE."
  `((id . ,(mentat-langfuse--get trace 'id))
    (name . ,(mentat-langfuse--get trace 'name))
    (timestamp . ,(mentat-langfuse--get trace 'timestamp))
    (session-id . ,(mentat-langfuse--get trace 'sessionId))
    (release . ,(mentat-langfuse--get trace 'release))
    (version . ,(mentat-langfuse--get trace 'version))))

(defun mentat-langfuse--trace-list-summary (data)
  "Return safe bounded fields from trace list DATA."
  (let ((meta (mentat-langfuse--get data 'meta)))
    `((items . ,(mapcar #'mentat-langfuse--trace-summary
                       (mentat-langfuse--get data 'data)))
      (page . ,(mentat-langfuse--get meta 'page))
      (total-items . ,(mentat-langfuse--get meta 'totalItems))
      (total-pages . ,(mentat-langfuse--get meta 'totalPages)))))

(defun mentat-langfuse--relevant-observation-p (observation)
  "Return non-nil for product-level OBSERVATION rather than transport noise."
  (let ((name (or (mentat-langfuse--get observation 'name) ""))
        (type (mentat-langfuse--get observation 'type)))
    (or (member type '("GENERATION" "EVENT"))
        (string-prefix-p "tool:" name)
        (member name '("webapp_agent_send_message" "research_send_message"
                       "response" "classify" "context_retrieval")))))

(defun mentat-langfuse--trace-detail-summary (trace)
  "Return bounded product-level observation metadata from TRACE."
  (let* ((observations (mentat-langfuse--get trace 'observations))
         (relevant (seq-filter #'mentat-langfuse--relevant-observation-p observations))
         (limit 200))
    (append (mentat-langfuse--trace-summary trace)
            `((observation-count . ,(length observations))
              (relevant-observation-count . ,(length relevant))
              (observations-truncated . ,(if (> (length relevant) limit) t :json-false))
              (observations . ,(mapcar #'mentat-langfuse--observation-summary
                                      (seq-take relevant limit)))))))

(defun mentat-langfuse--request-error (reject environment &rest response)
  "Reject failed Langfuse RESPONSE for ENVIRONMENT without secrets."
  (let ((http-response (plist-get response :response)))
    (funcall reject
             (format "Langfuse %s request failed%s"
                     environment
                     (if http-response
                         (format " with HTTP %s"
                                 (request-response-status-code http-response))
                       "")))))

(defun mentat-langfuse--starter (environment path params transform)
  "Return an authenticated async Langfuse GET starter for PATH."
  (let ((config (assoc environment mentat-langfuse--environments)))
    (unless config (error "ENVIRONMENT must be dev or prod"))
    (lambda (resolve reject on-cancel)
      (let ((aws (executable-find "aws"))
            (credential-buffer nil)
            (credential-process nil)
            (request-response nil)
            (cancelled nil)
            (done nil))
        (unless aws (error "AWS CLI is unavailable"))
        (cl-labels
            ((cleanup ()
               (when (and credential-process (process-live-p credential-process))
                 (delete-process credential-process))
               (when (buffer-live-p credential-buffer)
                 (kill-buffer credential-buffer)))
             (finish (callback value)
               (unless done
                 (setq done t)
                 (cleanup)
                 (funcall callback value)))
             (fetch (public-key secret-key)
               (setq request-response
                     (request
                       (concat (nth 2 config) path)
                       :type "GET"
                       :params params
                       :headers `(("Authorization" . ,(concat "Basic "
                                                              (base64-encode-string
                                                               (concat public-key ":" secret-key) t)))
                                  ("Accept" . "application/json"))
                       :parser (lambda ()
                                 (json-parse-buffer
                                  :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil))
                       :timeout 30
                       :success (lambda (&rest result)
                                  (unless cancelled
                                    (finish resolve
                                            (funcall transform
                                                     (plist-get result :data)))))
                       :error (lambda (&rest result)
                                (unless cancelled
                                  (apply #'mentat-langfuse--request-error
                                         (lambda (message) (finish reject message))
                                         environment result)))))))
          (setq credential-buffer (generate-new-buffer " *langfuse-credentials*"))
          (setq credential-process
                (make-process
                 :name "langfuse-credentials"
                 :buffer credential-buffer
                 :command
                 (list aws "--profile" (nth 1 config) "--region" "us-east-1"
                       "ssm" "get-parameters" "--names"
                       (format "/bradwell/%s/langfuse-public-key" environment)
                       (format "/bradwell/%s/langfuse-secret-key" environment)
                       "--with-decryption" "--output" "json" "--no-cli-pager")
                 :connection-type 'pipe :noquery t
                 :sentinel
                 (lambda (process _event)
                   (when (and (not done)
                              (memq (process-status process) '(exit signal)))
                     (if (not (zerop (process-exit-status process)))
                         (finish reject
                                 (format "Langfuse credential retrieval failed for %s; check AWS SSO access"
                                         environment))
                       (condition-case nil
                           (let* ((payload (with-current-buffer credential-buffer
                                             (goto-char (point-min))
                                             (json-parse-buffer :object-type 'alist
                                                                :array-type 'list)))
                                  (parameters (mentat-langfuse--get payload 'Parameters))
                                  (public-name (format "/bradwell/%s/langfuse-public-key" environment))
                                  (secret-name (format "/bradwell/%s/langfuse-secret-key" environment))
                                  (public-key nil)
                                  (secret-key nil))
                             (dolist (parameter parameters)
                               (let ((name (mentat-langfuse--get parameter 'Name))
                                     (value (mentat-langfuse--get parameter 'Value)))
                                 (cond ((equal name public-name) (setq public-key value))
                                       ((equal name secret-name) (setq secret-key value)))))
                             (unless (and (stringp public-key) (stringp secret-key))
                               (error "Missing Langfuse credentials"))
                             (kill-buffer credential-buffer)
                             (setq credential-buffer nil)
                             (fetch public-key secret-key)
                             (setq public-key nil secret-key nil))
                         (error (finish reject "Langfuse credential response was invalid"))))))))
          (funcall on-cancel
                   (lambda ()
                     (setq cancelled t done t)
                     (when request-response (request-abort request-response))
                     (cleanup))))))))

(defun mentat-langfuse--validate-window (from-time to-time limit)
  "Validate FROM-TIME, TO-TIME, and LIMIT."
  (unless (and (mentat-langfuse--valid-time-p from-time)
               (mentat-langfuse--valid-time-p to-time)
               (time-less-p (date-to-time from-time) (date-to-time to-time)))
    (error "FROM-TIME and TO-TIME must be ordered ISO 8601 timestamps"))
  (unless (and (integerp limit) (> limit 0) (<= limit 100))
    (error "LIMIT must be an integer from 1 to 100")))

(mentat-defun mentat-langfuse-list-user-traces
  (environment langfuse-user-id from-time to-time &key (limit 50))
  "List safe trace metadata for LANGFUSE-USER-ID in ENVIRONMENT.
The identifier is normally the user's email in current Bradwell traces.
FROM-TIME and TO-TIME are required ISO 8601 bounds.  Inputs, outputs, prompts,
emails, and arbitrary metadata are never returned."
  (:execution async)
  (unless (and (stringp langfuse-user-id)
               (not (string-empty-p langfuse-user-id)))
    (error "LANGFUSE-USER-ID must be a non-empty string"))
  (mentat-langfuse--validate-window from-time to-time limit)
  (mentat-langfuse--starter
   environment "/api/public/traces"
   `((userId . ,langfuse-user-id)
     (fromTimestamp . ,from-time)
     (toTimestamp . ,to-time)
     (limit . ,limit))
   #'mentat-langfuse--trace-list-summary))

(mentat-defun mentat-langfuse-inspect-trace (environment trace-id)
  "Inspect safe observation metadata for TRACE-ID in ENVIRONMENT.
Trace inputs, outputs, prompts, emails, and arbitrary metadata are omitted."
  (:execution async)
  (unless (and (stringp trace-id)
               (not (string-empty-p trace-id))
               (not (string-match-p "[^[:alnum:]_-]" trace-id)))
    (error "TRACE-ID contains unsupported characters"))
  (mentat-langfuse--starter
   environment (format "/api/public/traces/%s" trace-id) nil
   #'mentat-langfuse--trace-detail-summary))

(provide 'langfuse-api)
;;; langfuse-api.el ends here
