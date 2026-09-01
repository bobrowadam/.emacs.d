;;; linear-api.el --- Asynchronous Linear API helpers -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'json)
(require 'seq)
(require 'url)
(require 'mentat-elisp-library)

(defconst mentat-linear-endpoint "https://api.linear.app/graphql"
  "Linear GraphQL endpoint.")

(defvar mentat-linear-auth-host "linear.app"
  "Auth-source host containing the Linear API key.")

(defvar mentat-linear-auth-user "linear_api_key"
  "Auth-source user containing the Linear API key.")

(defun mentat-linear--api-key ()
  "Return the Linear API key from `auth-source'."
  (let* ((entry (car (auth-source-search
                      :host mentat-linear-auth-host
                      :user mentat-linear-auth-user
                      :max 1
                      :require '(:secret))))
         (stored-secret (and entry (plist-get entry :secret))))
    (unless stored-secret
      (user-error "No Linear credential found in Emacs auth-source"))
    (if (functionp stored-secret)
        (funcall stored-secret)
      stored-secret)))

(defun mentat-linear--default-error (text)
  "Report Linear API error TEXT."
  (message "Linear API error: %s" text))

(defun mentat-linear--handle-response (success error status)
  "Handle a Linear response with SUCCESS and ERROR callbacks using STATUS."
  (unwind-protect
      (condition-case err
          (if-let* ((request-error (plist-get status :error)))
              (funcall error (format "Request failed: %S" request-error))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Linear response has no HTTP header terminator"))
            (let* ((payload (json-parse-buffer :object-type 'alist
                                               :array-type 'list))
                   (graphql-errors (alist-get 'errors payload)))
              (if graphql-errors
                  (funcall error (format "GraphQL errors: %S" graphql-errors))
                (funcall success (alist-get 'data payload)))))
        (error (funcall error (error-message-string err))))
    (kill-buffer (current-buffer))))

(defun mentat-linear--request (query variables success error)
  "Run asynchronous Linear QUERY with VARIABLES and callbacks."
  (let* ((api-key (mentat-linear--api-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,api-key)
            ("Content-Type" . "application/json")))
         (url-request-data
          (json-serialize `((query . ,query) (variables . ,variables)))))
    (url-retrieve mentat-linear-endpoint
                  (apply-partially #'mentat-linear--handle-response
                                   success error)
                  nil t t)))

(defun mentat-linear--deliver-field (field success data)
  "Pass FIELD from DATA to SUCCESS."
  (funcall success (alist-get field data)))

(defun mentat-linear--deliver-mutation (field success error data)
  "Pass successful mutation FIELD from DATA to SUCCESS, or call ERROR."
  (let ((result (alist-get field data)))
    (if (eq (alist-get 'success result) t)
        (funcall success result)
      (funcall error (format "Linear mutation %s was unsuccessful" field)))))

(defun mentat-linear--get-issue (identifier success error)
  "Fetch Linear issue IDENTIFIER using SUCCESS and ERROR callbacks."
  (mentat-linear--request
   "query ($id: String!) {
      issue(id: $id) {
        id identifier title description
        state { id name type }
        team { id name states { nodes { id name type position } } }
      }
    }"
   `((id . ,identifier))
   (apply-partially #'mentat-linear--deliver-field 'issue success)
   error))

(defun mentat-linear--add-comment (identifier body success error)
  "Add BODY to Linear issue IDENTIFIER using SUCCESS and ERROR callbacks."
  (mentat-linear--request
   "mutation ($issueId: String!, $body: String!) {
      commentCreate(input: { issueId: $issueId, body: $body }) {
        success
        comment { id body createdAt }
      }
    }"
   `((issueId . ,identifier) (body . ,body))
   (apply-partially #'mentat-linear--deliver-mutation
                    'commentCreate success error)
   error))

(defun mentat-linear--set-state (identifier state-id success error)
  "Set Linear issue IDENTIFIER to STATE-ID using SUCCESS and ERROR callbacks."
  (mentat-linear--request
   "mutation ($issueId: String!, $stateId: String!) {
      issueUpdate(id: $issueId, input: { stateId: $stateId }) {
        success
        issue { id identifier state { id name type } }
      }
    }"
   `((issueId . ,identifier) (stateId . ,state-id))
   (apply-partially #'mentat-linear--deliver-mutation
                    'issueUpdate success error)
   error))

(defun mentat-linear--complete-after-fetch (identifier success error issue)
  "Complete IDENTIFIER after fetching ISSUE, using SUCCESS and ERROR."
  (let* ((states (alist-get 'nodes
                            (alist-get 'states
                                       (alist-get 'team issue))))
         (completed (seq-find
                     (lambda (state)
                       (equal (alist-get 'type state) "completed"))
                     states)))
    (if completed
        (mentat-linear--set-state identifier (alist-get 'id completed)
                                 success error)
      (funcall error "The issue team has no completed workflow state"))))

(defun mentat-linear--cancel-request (buffer)
  "Cancel the Linear request associated with BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun mentat-linear--starter (start)
  "Return a callback starter that invokes START and supports cancellation."
  (lambda (resolve reject on-cancel)
    (let ((request (funcall start resolve reject)))
      (funcall on-cancel
               (lambda () (mentat-linear--cancel-request request))))))

(mentat-defun mentat-linear-get-issue (identifier)
  "Fetch Linear issue IDENTIFIER and resolve with its alist."
  (:execution async)
  (mentat-linear--starter
   (lambda (resolve reject)
     (mentat-linear--get-issue identifier resolve reject))))

(mentat-defun mentat-linear-add-comment (identifier body)
  "Add BODY to Linear issue IDENTIFIER and resolve with the result."
  (:execution async)
  (mentat-linear--starter
   (lambda (resolve reject)
     (mentat-linear--add-comment identifier body resolve reject))))

(mentat-defun mentat-linear-set-state (identifier state-id)
  "Set Linear issue IDENTIFIER to STATE-ID and resolve with the result."
  (:execution async)
  (mentat-linear--starter
   (lambda (resolve reject)
     (mentat-linear--set-state identifier state-id resolve reject))))

(mentat-defun mentat-linear-complete-issue (identifier)
  "Move Linear issue IDENTIFIER to its team's completed workflow state."
  (:execution async)
  (mentat-linear--starter
   (lambda (resolve reject)
     (mentat-linear--get-issue
      identifier
      (apply-partially #'mentat-linear--complete-after-fetch
                       identifier resolve reject)
      reject))))

(provide 'linear-api)
;;; linear-api.el ends here
