;;; elastic-agent-builder.el --- Elastic Agent Builder MCP helper -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'subr-x)
(require 'url)
(require 'mentat-elisp-library)
(require 'mentat-emacs)

(defconst mentat-elastic-mcp-endpoint
  "https://bradwell-6e35cb.kb.us-east-1.aws.found.io/api/agent_builder/mcp"
  "Elastic Agent Builder MCP endpoint.")

(defvar mentat-elastic-mcp-auth-host "mcp.elastic.co"
  "Auth-source host containing the Elastic MCP API key.")

(defvar mentat-elastic-mcp-auth-user "bob"
  "Auth-source user containing the Elastic MCP API key.")

(defvar mentat-elastic-mcp--request-id 0
  "Last JSON-RPC request identifier used for Elastic MCP calls.")

(defconst mentat-kibana-base-url
  "https://bradwell-6e35cb.kb.us-east-1.aws.found.io"
  "Bradwell Kibana base URL.")

(defconst mentat-kibana--read-only-rule-fields
  '("id" "rule_type_id" "consumer" "enabled" "execution_status"
    "monitoring" "created_by" "created_at" "updated_by" "updated_at"
    "api_key" "api_key_owner" "api_key_created_by_user" "revision"
    "running" "scheduled_task_id" "last_run" "next_run"
    "snooze_schedule" "mute_all" "muted_instance_ids"
    "muted_alert_ids" "notify_when" "throttle")
  "Kibana alert rule fields that must not be sent in an update.")

(defconst mentat-kibana--read-only-action-fields
  '("connector_type_id" "uuid")
  "Kibana alert action fields that must not be sent in an update.")

(defun mentat-kibana--rule-query-object (rule)
  "Return RULE's mutable KQL query object."
  (let* ((params (gethash "params" rule))
         (search (and (hash-table-p params)
                      (gethash "searchConfiguration" params)))
         (query (and (hash-table-p search) (gethash "query" search))))
    (unless (hash-table-p query)
      (error "Kibana rule has no searchConfiguration query object"))
    query))

(defun mentat-kibana--sanitize-rule (rule)
  "Make parsed Kibana RULE safe for display and PUT requests."
  (dolist (field mentat-kibana--read-only-rule-fields)
    (remhash field rule))
  (when (eq (gethash "alert_delay" rule) :json-null)
    (remhash "alert_delay" rule))
  (when (eq (gethash "tags" rule) :json-null)
    (puthash "tags" [] rule))
  (let ((actions (gethash "actions" rule)))
    (when (vectorp actions)
      (seq-doseq (action actions)
        (when (hash-table-p action)
          (dolist (field mentat-kibana--read-only-action-fields)
            (remhash field action))))))
  rule)

(defun mentat-kibana--json-string (object)
  "Return OBJECT as readable JSON while preserving null and false values."
  (decode-coding-string
   (json-serialize object
                   :null-object :json-null
                   :false-object :json-false)
   'utf-8))

(defun mentat-kibana--request-error (reject &rest response)
  "Reject a Kibana request with bounded details from RESPONSE."
  (let ((request-response (plist-get response :response))
        (data (plist-get response :data)))
    (funcall reject
             (format "Kibana request failed%s: %S%s"
                     (if request-response
                         (format " with HTTP %s"
                                 (request-response-status-code
                                  request-response))
                       "")
                     (plist-get response :error-thrown)
                     (if data (format "; response: %.1000S" data) "")))))

(defun mentat-kibana--request
    (api-key method path payload success reject)
  "Send an authenticated Kibana request and invoke SUCCESS or REJECT."
  (request
   (concat (string-remove-suffix "/" mentat-kibana-base-url) path)
   :type method
   :data (and payload
              (json-serialize payload
                              :null-object :json-null
                              :false-object :json-false))
   :headers `(("Authorization" . ,(mentat-elastic-mcp--authorization api-key))
              ("Accept" . "application/json")
              ("Content-Type" . "application/json")
              ("kbn-xsrf" . "true"))
   :parser (lambda ()
             (json-parse-buffer
              :object-type 'hash-table
              :array-type 'array
              :null-object :json-null
              :false-object :json-false))
   :timeout 30
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall success data)))
   :error (apply-partially #'mentat-kibana--request-error reject)))

(defun mentat-kibana--cancel-request (response)
  "Cancel Kibana request RESPONSE when it is active."
  (when response
    (request-abort response)))

(defun mentat-kibana--request-starter (start)
  "Return an async starter that authenticates before invoking START."
  (lambda (resolve reject on-cancel)
    (let ((cancelled nil)
          credential-cleanup
          response)
      (setq credential-cleanup
            (mentat-auth-source-secret-async
             "Kibana" (list mentat-elastic-mcp-auth-host)
             (lambda (api-key)
               (unless cancelled
                 (condition-case err
                     (setq response (funcall start api-key resolve reject))
                   (error
                    (funcall reject (error-message-string err))))))
             reject
             :user mentat-elastic-mcp-auth-user))
      (funcall on-cancel
               (lambda ()
                 (setq cancelled t)
                 (when credential-cleanup
                   (funcall credential-cleanup))
                 (mentat-kibana--cancel-request response))))))

(defun mentat-kibana--rule-summary (rule)
  "Return bounded verification metadata for parsed Kibana RULE."
  (let ((actions (gethash "actions" rule)))
    `((id . ,(gethash "id" rule))
      (name . ,(gethash "name" rule))
      (updated-at . ,(gethash "updated_at" rule))
      (query . ,(gethash "query" (mentat-kibana--rule-query-object rule)))
      (schedule . ,(gethash "schedule" rule))
      (action-count . ,(if (vectorp actions) (length actions) 0)))))

(mentat-defun mentat-kibana-get-alert-rule (rule-id)
  "Get Kibana alert RULE-ID without secrets or read-only update fields."
  (:execution async)
  (unless (and (stringp rule-id) (not (string-empty-p rule-id)))
    (error "RULE-ID must be a non-empty string"))
  (mentat-kibana--request-starter
   (lambda (api-key resolve reject)
     (mentat-kibana--request
      api-key "GET" (format "/api/alerting/rule/%s" rule-id) nil
      (lambda (rule)
        (funcall resolve
                 (mentat-kibana--json-string
                  (mentat-kibana--sanitize-rule rule))))
      reject))))

(mentat-defun mentat-kibana-update-alert-rule-query
    (rule-id expected-query replacement-query)
  "Replace RULE-ID's KQL only when it equals EXPECTED-QUERY.

Fetch the current rule, sanitize its PUT body, update the query, and fetch it
again to verify REPLACEMENT-QUERY is live.  Return bounded rule metadata."
  (:execution async)
  (dolist (value (list rule-id expected-query replacement-query))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "RULE-ID and query arguments must be non-empty strings")))
  (lambda (resolve reject on-cancel)
    (let ((api-key nil)
          (cancelled nil)
          credential-cleanup
          response)
      (cl-labels
          ((fail (message)
             (unless cancelled
               (setq api-key nil)
               (funcall reject message)))
           (finish (rule)
             (unless cancelled
               (setq api-key nil)
               (funcall resolve (mentat-kibana--rule-summary rule))))
           (verify-rule (rule)
             (condition-case err
                 (if (equal (gethash "query"
                                     (mentat-kibana--rule-query-object rule))
                            replacement-query)
                     (finish rule)
                   (fail "Kibana rule verification returned a different query"))
               (error (fail (error-message-string err)))))
           (verify-update (_updated-rule)
             (unless cancelled
               (setq response
                     (mentat-kibana--request
                      api-key "GET"
                      (format "/api/alerting/rule/%s" rule-id)
                      nil #'verify-rule #'fail))))
           (update-rule (rule)
             (condition-case err
                 (let* ((body (mentat-kibana--sanitize-rule rule))
                        (query (mentat-kibana--rule-query-object body))
                        (current-query (gethash "query" query)))
                   (if (equal current-query replacement-query)
                       (finish rule)
                     (unless (equal current-query expected-query)
                       (error "Kibana rule query changed; refusing update"))
                     (puthash "query" replacement-query query)
                     (setq response
                           (mentat-kibana--request
                            api-key "PUT"
                            (format "/api/alerting/rule/%s" rule-id)
                            body #'verify-update #'fail))))
               (error (fail (error-message-string err)))))
           (fetch-rule ()
             (setq response
                   (mentat-kibana--request
                    api-key "GET" (format "/api/alerting/rule/%s" rule-id)
                    nil #'update-rule #'fail))))
        (setq credential-cleanup
              (mentat-auth-source-secret-async
               "Kibana" (list mentat-elastic-mcp-auth-host)
               (lambda (secret)
                 (unless cancelled
                   (setq api-key secret)
                   (fetch-rule)))
               #'fail
               :user mentat-elastic-mcp-auth-user))
        (funcall on-cancel
                 (lambda ()
                   (setq cancelled t
                         api-key nil)
                   (when credential-cleanup
                     (funcall credential-cleanup))
                   (mentat-kibana--cancel-request response)))))))

(defun mentat-elastic-mcp--authorization (api-key)
  "Return an Authorization header value for API-KEY."
  (if (string-prefix-p "ApiKey " api-key)
      api-key
    (concat "ApiKey " api-key)))

(defun mentat-elastic-mcp--handle-response (success error status)
  "Handle an Elastic MCP response with SUCCESS and ERROR using STATUS."
  (unwind-protect
      (condition-case err
          (if-let* ((request-error (plist-get status :error)))
              (funcall error (format "Elastic MCP request failed: %S" request-error))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Elastic MCP response has no HTTP header terminator"))
            (let* ((payload (json-parse-buffer :object-type 'alist
                                               :array-type 'list))
                   (rpc-error (alist-get 'error payload)))
              (if rpc-error
                  (funcall error (format "Elastic MCP error: %S" rpc-error))
                (funcall success (alist-get 'result payload)))))
        (error (funcall error (error-message-string err))))
    (kill-buffer (current-buffer))))

(defun mentat-elastic-mcp--request
    (api-key tool arguments success error)
  "Call Elastic MCP TOOL with ARGUMENTS and callbacks using API-KEY."
  (let* ((request-id (cl-incf mentat-elastic-mcp--request-id))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(mentat-elastic-mcp--authorization api-key))
            ("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data
          (json-serialize
           `((jsonrpc . "2.0")
             (id . ,request-id)
             (method . "tools/call")
             (params . ((name . ,tool) (arguments . ,arguments)))))))
    (url-retrieve mentat-elastic-mcp-endpoint
                  (apply-partially #'mentat-elastic-mcp--handle-response
                                   success error)
                  nil t t)))

(defun mentat-elastic-mcp--cancel-request (buffer)
  "Cancel the Elastic MCP request associated with BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun mentat-elastic-mcp--starter (tool arguments)
  "Return a callback starter for Elastic MCP TOOL and ARGUMENTS."
  (lambda (resolve reject on-cancel)
    (let ((cancelled nil)
          request
          credential-cleanup)
      (setq credential-cleanup
            (mentat-auth-source-secret-async
             "Elastic MCP" (list mentat-elastic-mcp-auth-host)
             (lambda (api-key)
               (unless cancelled
                 (setq request
                       (mentat-elastic-mcp--request
                        api-key tool arguments resolve reject))
                 (when cancelled
                   (mentat-elastic-mcp--cancel-request request))))
             reject
             :user mentat-elastic-mcp-auth-user))
      (funcall on-cancel
               (lambda ()
                 (setq cancelled t)
                 (funcall credential-cleanup)
                 (mentat-elastic-mcp--cancel-request request))))))

(mentat-defun mentat-elastic-mcp-call-tool (tool arguments)
  "Call Elastic Agent Builder MCP TOOL with ARGUMENTS."
  (:execution async)
  (mentat-elastic-mcp--starter tool arguments))

(provide 'elastic-agent-builder)
;;; elastic-agent-builder.el ends here
