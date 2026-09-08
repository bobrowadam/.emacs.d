;;; posthog-api.el --- Asynchronous PostHog API helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'subr-x)
(require 'mentat-elisp-library)
(require 'mentat-emacs)

(defvar mentat-posthog-host "https://us.posthog.com"
  "PostHog application host.")

(defvar mentat-posthog-auth-host "posthog.com"
  "Auth-source host containing the PostHog personal API key.")

(defvar mentat-posthog-auth-user "bob"
  "Auth-source user containing the PostHog personal API key.")

(defvar mentat-posthog-default-project-id "314262"
  "Default PostHog project ID.")

(defun mentat-posthog--json-get (key object)
  "Get KEY from JSON alist OBJECT, accepting symbol or string keys."
  (when (listp object)
    (let ((symbol-key (if (symbolp key) key (intern key)))
          (string-key (if (stringp key) key (symbol-name key))))
      (or (alist-get symbol-key object)
          (alist-get string-key object nil nil #'equal)))))

(defun mentat-posthog--recording-summary (recording)
  "Return bounded non-personal metadata from RECORDING."
  `((id . ,(mentat-posthog--json-get 'id recording))
    (start-time . ,(mentat-posthog--json-get 'start_time recording))
    (end-time . ,(mentat-posthog--json-get 'end_time recording))
    (recording-duration . ,(mentat-posthog--json-get
                            'recording_duration recording))
    (ongoing . ,(mentat-posthog--json-get 'ongoing recording))))

(defun mentat-posthog--person-property (recording property)
  "Return person PROPERTY from RECORDING."
  (mentat-posthog--json-get
   property
   (mentat-posthog--json-get
    'properties
    (mentat-posthog--json-get 'person recording))))

(defun mentat-posthog--scan-result
    (property value matches property-present scanned available complete reason)
  "Build a bounded scan result for PROPERTY VALUE and MATCHES."
  `((property . ,property)
    (value . ,value)
    (found . ,(if matches t :json-false))
    (match-count . ,(length matches))
    (matches . ,(nreverse matches))
    (recordings-with-property . ,property-present)
    (scanned-recordings . ,scanned)
    (available-recordings . ,available)
    (complete-scan . ,(if complete t :json-false))
    (stopped-because . ,reason)))

(defun mentat-posthog--find-recordings-starter
    (property value project-id max-recordings max-matches page-size)
  "Return an async starter that finds recordings up to MAX-MATCHES and MAX-RECORDINGS,matching PROPERTY VALUE for PROJECT-ID."
  (lambda (resolve reject on-cancel)
    (let ((api-key nil)
          (available nil)
          (cancelled nil)
          (credential-cleanup nil)
          (matches nil)
          (offset 0)
          (property-present 0)
          (request-response nil)
          (scanned 0))
      (cl-labels
          ((finish
             (complete reason)
             (unless cancelled
               (setq api-key nil)
               (funcall resolve
                        (mentat-posthog--scan-result
                         property
                         value
                         matches
                         property-present
                         scanned
                         available
                         complete
                         reason))))
           (fail
             (message)
             (unless cancelled
               (setq api-key nil)
               (funcall reject message)))
           (fetch-page
             ()
             (unless cancelled
               (let ((limit (min page-size (- max-recordings scanned))))
                 (if (<= limit 0)
                     (finish nil "recording-limit")
                   (setq request-response
                         (request
                           (format "%s/api/projects/%s/session_recordings/"
                                   (string-remove-suffix
                                    "/" mentat-posthog-host)
                                   project-id)
                           :type "GET"
                           :params `((limit . ,limit) (offset . ,offset))
                           :headers
                           `(("Authorization"
                              .
                              ,(concat "Bearer " api-key))
                             ("Accept" . "application/json"))
                           :parser
                           (lambda ()
                             (json-parse-buffer
                              :object-type 'alist
                              :array-type 'list
                              :null-object nil
                              :false-object nil))
                           :timeout 30
                           :success
                           (cl-function
                            (lambda (&key data &allow-other-keys)
                              (unless cancelled
                                (let* ((results
                                        (or (mentat-posthog--json-get
                                             'results data)
                                            nil))
                                       (count
                                        (mentat-posthog--json-get
                                         'count data)))
                                  (when (numberp count)
                                    (setq available count))
                                  (dolist (recording results)
                                    (let ((property-value
                                           (mentat-posthog--person-property
                                            recording property)))
                                      (when property-value
                                        (setq property-present
                                              (1+ property-present)))
                                      (when (and (< (length matches)
                                                    max-matches)
                                                 (equal
                                                  property-value value))
                                        (push
                                         (mentat-posthog--recording-summary
                                          recording)
                                         matches))))
                                  (setq
                                   scanned (+ scanned (length results))
                                   offset (+ offset (length results)))
                                  (cond
                                   ((>= (length matches) max-matches)
                                    (finish nil "match-limit"))
                                   ((null results)
                                    (finish t "end-of-results"))
                                   ((and (numberp available)
                                         (>= offset available))
                                    (finish t "all-recordings-scanned"))
                                   ((>= scanned max-recordings)
                                    (finish nil "recording-limit"))
                                   (t
                                    (fetch-page)))))))
                           :error
                           (cl-function
                            (lambda (&key
                                error-thrown
                                response
                                &allow-other-keys)
                              (fail
                               (format "PostHog request failed%s: %S"
                                       (if response
                                           (format
                                            " with HTTP %s"
                                            (request-response-status-code
                                             response))
                                         "")
                                       error-thrown)))))))))))
        (setq credential-cleanup
              (mentat-auth-source-secret-async
               "PostHog" (list mentat-posthog-auth-host)
               (lambda (secret)
                 (unless cancelled
                   (setq api-key secret)
                   (fetch-page)))
               #'fail
               :user mentat-posthog-auth-user))
        (funcall on-cancel
                 (lambda ()
                   (setq
                    cancelled t
                    api-key nil)
                   (when credential-cleanup
                     (funcall credential-cleanup))
                   (when request-response
                     (request-abort request-response))))))))

(defun mentat-posthog--json-success (resolve &rest response)
  "Resolve with parsed JSON from RESPONSE."
  (funcall resolve (plist-get response :data)))

(defun mentat-posthog--json-error (reject &rest response)
  "Reject with a bounded error from RESPONSE."
  (let ((request-response (plist-get response :response))
        (data (plist-get response :data)))
    (funcall reject
             (format "PostHog request failed%s: %S%s"
                     (if request-response
                         (format " with HTTP %s"
                                 (request-response-status-code
                                  request-response))
                       "")
                     (plist-get response :error-thrown)
                     (if data (format "; response: %.1000S" data) "")))))

(defun mentat-posthog--json-request
    (api-key method path params payload resolve reject)
  "Send an authenticated JSON request and invoke RESOLVE or REJECT."
  (request
    (concat (string-remove-suffix "/" mentat-posthog-host) path)
    :type method
    :params params
    :data (and payload (json-serialize payload))
    :headers `(("Authorization" . ,(concat "Bearer " api-key))
               ("Accept" . "application/json")
               ("Content-Type" . "application/json"))
    :parser (lambda ()
              (json-parse-buffer
               :object-type 'alist
               :array-type 'list
               :null-object nil
               :false-object nil))
    :timeout 30
    :success (apply-partially #'mentat-posthog--json-success resolve)
    :error (apply-partially #'mentat-posthog--json-error reject)))

(defun mentat-posthog--cancel-request (response)
  "Cancel PostHog request RESPONSE when it is active."
  (when response
    (request-abort response)))

(defun mentat-posthog--request-starter (start)
  "Return an async starter that authenticates before invoking START."
  (lambda (resolve reject on-cancel)
    (let ((cancelled nil)
          (credential-cleanup nil)
          (response nil))
      (setq
       credential-cleanup
       (mentat-auth-source-secret-async
        "PostHog" (list mentat-posthog-auth-host)
        (lambda (api-key)
          (unless cancelled
            (condition-case err
                (progn
                  (setq response (funcall start api-key resolve reject))
                  (when cancelled
                    (mentat-posthog--cancel-request response)))
              (error
               (funcall reject (error-message-string err))))))
        reject
        :user mentat-posthog-auth-user))
      (funcall
       on-cancel
       (lambda ()
         (setq cancelled t)
         (funcall credential-cleanup)
         (mentat-posthog--cancel-request response))))))

(mentat-defun mentat-posthog-list-feature-flags
  (&key search
        (project-id mentat-posthog-default-project-id)
        (limit 100))
  "List PostHog feature flags, optionally filtering by SEARCH."
  (:execution async)
  (unless (and (integerp limit) (> limit 0) (<= limit 100))
    (error "LIMIT must be an integer from 1 to 100"))
  (mentat-posthog--request-starter
   (lambda (api-key resolve reject)
     (mentat-posthog--json-request
      api-key "GET"
      (format "/api/projects/%s/feature_flags/" project-id)
      (append `((limit . ,limit))
              (and search `((search . ,search))))
      nil resolve reject))))

(mentat-defun mentat-posthog-update-feature-flag-filters
  (flag-id filters-json &key
           (project-id mentat-posthog-default-project-id))
  "Replace FILTERS-JSON for PostHog feature FLAG-ID and return the updated flag."
  (:execution async)
  (unless (integerp flag-id)
    (error "FLAG-ID must be an integer"))
  (unless (stringp filters-json)
    (error "FILTERS-JSON must be a JSON object string"))
  (let ((filters (json-parse-string filters-json :object-type 'alist)))
    (unless (listp filters)
      (error "FILTERS-JSON must contain a JSON object"))
    (mentat-posthog--request-starter
     (lambda (api-key resolve reject)
       (mentat-posthog--json-request
        api-key "PATCH"
        (format "/api/projects/%s/feature_flags/%s/" project-id flag-id)
        nil `((filters . ,filters)) resolve reject)))))

(mentat-defun mentat-posthog-find-recordings-by-person-property
  (property value &key
            (project-id mentat-posthog-default-project-id)
            (max-recordings 5000)
            (max-matches 20)
            (page-size 100))
  "Find replay recordings whose person PROPERTY equals VALUE.

Return bounded recording metadata only.  PROJECT-ID defaults to the Bradwell
production project.  MAX-RECORDINGS bounds pagination, MAX-MATCHES bounds the
returned matches, and PAGE-SIZE controls each API request."
  (:execution async)
  (unless (and (stringp property) (not (string-empty-p property)))
    (error "PROPERTY must be a non-empty string"))
  (unless (and (stringp value) (not (string-empty-p value)))
    (error "VALUE must be a non-empty string"))
  (unless (and (integerp max-recordings) (> max-recordings 0))
    (error "MAX-RECORDINGS must be a positive integer"))
  (unless (and (integerp max-matches) (> max-matches 0))
    (error "MAX-MATCHES must be a positive integer"))
  (unless (and (integerp page-size) (> page-size 0) (<= page-size 100))
    (error "PAGE-SIZE must be an integer from 1 to 100"))
  (mentat-posthog--find-recordings-starter
   property value project-id max-recordings max-matches page-size))

(provide 'posthog-api)
;;; posthog-api.el ends here
