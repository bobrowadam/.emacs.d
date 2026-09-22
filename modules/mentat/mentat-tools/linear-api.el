;;; linear-api.el --- Asynchronous Linear API helpers -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'json)
(require 'seq)
(require 'url)
(require 'url-parse)
(require 'mentat-emacs)

(defconst mentat-linear-endpoint "https://api.linear.app/graphql"
  "Linear GraphQL endpoint.")

(defvar mentat-linear-auth-host "linear.app"
  "Auth-source host containing the Linear API key.")

(defvar mentat-linear-auth-user "linear_api_key"
  "Auth-source user containing the Linear API key.")

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

(defun mentat-linear--request (api-key query variables success error)
  "Run asynchronous Linear QUERY with VARIABLES and callbacks using API-KEY."
  (let* ((url-request-method "POST")
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

(defun mentat-linear--get-issue (api-key identifier success error)
  "Fetch Linear issue IDENTIFIER using API-KEY, SUCCESS, and ERROR callbacks."
  (mentat-linear--request
   api-key
   "query ($id: String!) {
      issue(id: $id) {
        id identifier title description url
        state { id name type }
        team { id name states { nodes { id name type position } } }
      }
    }"
   `((id . ,identifier))
   (apply-partially #'mentat-linear--deliver-field 'issue success)
   error))

(defun mentat-linear--cancel-request (buffer)
  "Cancel the Linear request associated with BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun mentat-linear--starter (start)
  "Return a callback starter that obtains credentials before invoking START."
  (lambda (resolve reject on-cancel)
    (let ((cancelled nil)
          request
          credential-cleanup)
      (setq credential-cleanup
            (mentat-auth-source-secret-async
             "Linear" (list mentat-linear-auth-host)
             (lambda (api-key)
               (unless cancelled
                 (setq request (funcall start api-key resolve reject))
                 (when cancelled
                   (mentat-linear--cancel-request request))))
             reject
             :user mentat-linear-auth-user))
      (funcall on-cancel
               (lambda ()
                 (setq cancelled t)
                 (funcall credential-cleanup)
                 (mentat-linear--cancel-request request))))))

(provide 'linear-api)
;;; linear-api.el ends here
