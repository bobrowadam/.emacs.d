;;; stripe-api.el --- Read-only Stripe diagnostics -*- lexical-binding: t; -*-

(require 'json)
(require 'request)
(require 'subr-x)
(require 'mentat-elisp-library)
(require 'mentat-emacs)

(defconst mentat-stripe-api-host "https://api.stripe.com"
  "Stripe API host.")

(defconst mentat-stripe-auth-host "api.stripe.com"
  "Auth-source host containing the Stripe secret key.")

(defconst mentat-stripe-auth-user "apikey"
  "Auth-source user containing the Stripe secret key.")

(defun mentat-stripe--get (object key)
  "Return KEY from JSON alist OBJECT."
  (when (listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'equal))))

(defun mentat-stripe--id (object)
  "Return an identifier from OBJECT, which can be an ID or expanded object."
  (if (stringp object) object (mentat-stripe--get object 'id)))

(defun mentat-stripe--invoice-intent (invoice)
  "Return the PaymentIntent reference from an INVOICE of any supported version."
  (or (mentat-stripe--get invoice 'payment_intent)
      (let* ((payments (mentat-stripe--get invoice 'payments))
             (invoice-payment (car (mentat-stripe--get payments 'data)))
             (payment (mentat-stripe--get invoice-payment 'payment)))
        (mentat-stripe--get payment 'payment_intent))))

(defun mentat-stripe--error-summary (error)
  "Return safe diagnostic fields from Stripe ERROR."
  (when (listp error)
    `((type . ,(mentat-stripe--get error 'type))
      (code . ,(mentat-stripe--get error 'code))
      (decline-code . ,(mentat-stripe--get error 'decline_code))
      (message . ,(mentat-stripe--get error 'message))
      (doc-url . ,(mentat-stripe--get error 'doc_url)))))

(defun mentat-stripe--charge-summary (charge)
  "Return safe payment diagnostics from expanded CHARGE."
  (when (listp charge)
    (let ((outcome (mentat-stripe--get charge 'outcome))
          (details (mentat-stripe--get charge 'payment_method_details)))
      `((id . ,(mentat-stripe--get charge 'id))
        (status . ,(mentat-stripe--get charge 'status))
        (failure-code . ,(mentat-stripe--get charge 'failure_code))
        (failure-message . ,(mentat-stripe--get charge 'failure_message))
        (payment-method-type . ,(mentat-stripe--get details 'type))
        (outcome . ((type . ,(mentat-stripe--get outcome 'type))
                    (reason . ,(mentat-stripe--get outcome 'reason))
                    (network-status . ,(mentat-stripe--get outcome 'network_status))
                    (seller-message . ,(mentat-stripe--get outcome 'seller_message))))))))

(defun mentat-stripe--event-summary (event &optional expanded-intent)
  "Return a bounded payment diagnostic summary for EVENT."
  (let* ((invoice (mentat-stripe--get (mentat-stripe--get event 'data) 'object))
         (invoice-intent (mentat-stripe--invoice-intent invoice))
         (intent (or expanded-intent invoice-intent))
         (charge (and (listp intent)
                      (mentat-stripe--get intent 'latest_charge))))
    `((event . ((id . ,(mentat-stripe--get event 'id))
                (type . ,(mentat-stripe--get event 'type))
                (created . ,(mentat-stripe--get event 'created))
                (livemode . ,(if (mentat-stripe--get event 'livemode) t :json-false))))
      (invoice . ((id . ,(mentat-stripe--get invoice 'id))
                  (customer-id . ,(mentat-stripe--id (mentat-stripe--get invoice 'customer)))
                  (status . ,(mentat-stripe--get invoice 'status))
                  (amount-due . ,(mentat-stripe--get invoice 'amount_due))
                  (amount-paid . ,(mentat-stripe--get invoice 'amount_paid))
                  (amount-remaining . ,(mentat-stripe--get invoice 'amount_remaining))
                  (currency . ,(mentat-stripe--get invoice 'currency))
                  (collection-method . ,(mentat-stripe--get invoice 'collection_method))
                  (billing-reason . ,(mentat-stripe--get invoice 'billing_reason))
                  (has-default-payment-method . ,(if (mentat-stripe--get invoice 'default_payment_method) t :json-false))
                  (has-default-source . ,(if (mentat-stripe--get invoice 'default_source) t :json-false))
                  (attempt-count . ,(mentat-stripe--get invoice 'attempt_count))
                  (next-payment-attempt . ,(mentat-stripe--get invoice 'next_payment_attempt))))
      (payment-intent . ((id . ,(mentat-stripe--id intent))
                         (status . ,(and (listp intent)
                                        (mentat-stripe--get intent 'status)))
                         (last-payment-error . ,(and (listp intent)
                                                     (mentat-stripe--error-summary
                                                      (mentat-stripe--get intent 'last_payment_error))))))
      (latest-charge . ,(if (listp charge)
                            (mentat-stripe--charge-summary charge)
                          `((id . ,(mentat-stripe--id charge))))))))

(defun mentat-stripe--request-error (reject &rest response)
  "Reject a Stripe request with bounded details from RESPONSE."
  (let* ((http-response (plist-get response :response))
         (data (plist-get response :data))
         (error-object (and data (mentat-stripe--get data 'error))))
    (funcall reject
             (format "Stripe request failed%s%s"
                     (if http-response
                         (format " with HTTP %s"
                                 (request-response-status-code http-response))
                       "")
                     (if error-object
                         (format ": %s"
                                 (or (mentat-stripe--get error-object 'message)
                                     "unknown Stripe error"))
                       "")))))

(defun mentat-stripe--diagnose-starter (event-id)
  "Return an async starter that diagnoses Stripe EVENT-ID."
  (lambda (resolve reject on-cancel)
    (let ((cancelled nil)
          (credential-cleanup nil)
          (response nil))
      (cl-labels
          ((send
             (api-key path params success)
             (setq response
                   (request
                     (concat mentat-stripe-api-host path)
                     :type "GET"
                     :params params
                     :headers `(("Authorization" . ,(concat "Basic "
                                                           (base64-encode-string
                                                            (concat api-key ":") t)))
                                ("Accept" . "application/json"))
                     :parser (lambda ()
                               (json-parse-buffer
                                :object-type 'alist
                                :array-type 'list
                                :null-object nil
                                :false-object nil))
                     :timeout 30
                     :success success
                     :error (lambda (&rest result)
                              (unless cancelled
                                (apply #'mentat-stripe--request-error
                                       reject result))))))
           (fetch-event
             (api-key)
             (send
              api-key (format "/v1/events/%s" event-id) nil
              (lambda (&rest result)
                (unless cancelled
                  (let* ((event (plist-get result :data))
                         (invoice (mentat-stripe--get
                                   (mentat-stripe--get event 'data) 'object))
                         (intent (mentat-stripe--invoice-intent invoice))
                         (intent-id (mentat-stripe--id intent)))
                    (if (or (listp intent) (not intent-id))
                        (funcall resolve (mentat-stripe--event-summary event))
                      (send
                       api-key (format "/v1/payment_intents/%s" intent-id)
                       '(("expand[]" . "latest_charge"))
                       (lambda (&rest intent-result)
                         (unless cancelled
                           (funcall resolve
                                    (mentat-stripe--event-summary
                                     event (plist-get intent-result :data)))))))))))))
        (setq credential-cleanup
              (mentat-auth-source-secret-async
               "Stripe" (list mentat-stripe-auth-host)
               (lambda (api-key)
                 (unless cancelled (fetch-event api-key)))
               reject
               :user mentat-stripe-auth-user)))
      (funcall on-cancel
               (lambda ()
                 (setq cancelled t)
                 (when credential-cleanup
                   (funcall credential-cleanup))
                 (when response
                   (request-abort response)))))))

(mentat-defun mentat-stripe-diagnose-event (event-id)
  "Diagnose Stripe payment failure EVENT-ID without returning customer data.

Retrieve the event read-only and expand an invoice PaymentIntent and its latest
Charge.  Return bounded invoice, PaymentIntent, Charge, and failure fields."
  (:execution async)
  (unless (and (stringp event-id)
               (string-match-p "\\`evt_[[:alnum:]_]+\\'" event-id))
    (error "EVENT-ID must be a Stripe event ID beginning evt_"))
  (mentat-stripe--diagnose-starter event-id))

(defun mentat-stripe--request-starter (path params transform)
  "Return an authenticated async GET starter for PATH and PARAMS."
  (lambda (resolve reject on-cancel)
    (let ((cancelled nil)
          (credential-cleanup nil)
          (response nil))
      (setq credential-cleanup
            (mentat-auth-source-secret-async
             "Stripe" (list mentat-stripe-auth-host)
             (lambda (api-key)
               (unless cancelled
                 (setq response
                       (request
                         (concat mentat-stripe-api-host path)
                         :type "GET"
                         :params params
                         :headers `(("Authorization" . ,(concat "Basic "
                                                               (base64-encode-string
                                                                (concat api-key ":") t)))
                                    ("Accept" . "application/json"))
                         :parser (lambda ()
                                   (json-parse-buffer
                                    :object-type 'alist
                                    :array-type 'list
                                    :null-object nil
                                    :false-object nil))
                         :timeout 30
                         :success (lambda (&rest result)
                                    (unless cancelled
                                      (funcall resolve
                                               (funcall transform
                                                        (plist-get result :data)))))
                         :error (lambda (&rest result)
                                  (unless cancelled
                                    (apply #'mentat-stripe--request-error
                                           reject result)))))))
             reject :user mentat-stripe-auth-user))
      (funcall on-cancel
               (lambda ()
                 (setq cancelled t)
                 (when credential-cleanup (funcall credential-cleanup))
                 (when response (request-abort response)))))))

(defun mentat-stripe--customer-summary (customer)
  "Return non-contact account state from CUSTOMER."
  (let ((settings (mentat-stripe--get customer 'invoice_settings)))
    `((id . ,(mentat-stripe--get customer 'id))
      (created . ,(mentat-stripe--get customer 'created))
      (deleted . ,(if (mentat-stripe--get customer 'deleted) t :json-false))
      (delinquent . ,(if (mentat-stripe--get customer 'delinquent) t :json-false))
      (has-default-payment-method . ,(if (mentat-stripe--get settings 'default_payment_method) t :json-false))
      (has-default-source . ,(if (mentat-stripe--get customer 'default_source) t :json-false)))))

(defun mentat-stripe--payment-method-summary (method)
  "Return bounded instrument details from METHOD."
  (let* ((type (mentat-stripe--get method 'type))
         (details (mentat-stripe--get method (and type (intern type)))))
    `((id . ,(mentat-stripe--get method 'id))
      (type . ,type)
      (created . ,(mentat-stripe--get method 'created))
      (brand . ,(mentat-stripe--get details 'brand))
      (last4 . ,(mentat-stripe--get details 'last4))
      (exp-month . ,(mentat-stripe--get details 'exp_month))
      (exp-year . ,(mentat-stripe--get details 'exp_year)))))

(defun mentat-stripe--invoice-history-summary (invoice)
  "Return bounded history fields from INVOICE."
  `((id . ,(mentat-stripe--get invoice 'id))
    (created . ,(mentat-stripe--get invoice 'created))
    (status . ,(mentat-stripe--get invoice 'status))
    (amount-due . ,(mentat-stripe--get invoice 'amount_due))
    (amount-paid . ,(mentat-stripe--get invoice 'amount_paid))
    (currency . ,(mentat-stripe--get invoice 'currency))
    (attempt-count . ,(mentat-stripe--get invoice 'attempt_count))
    (billing-reason . ,(mentat-stripe--get invoice 'billing_reason))
    (payment-intent-id . ,(mentat-stripe--id
                           (mentat-stripe--invoice-intent invoice)))))

(defun mentat-stripe--intent-summary (intent)
  "Return bounded history fields from PaymentIntent INTENT."
  (let* ((charge (mentat-stripe--get intent 'latest_charge))
         (details (and (listp charge)
                       (mentat-stripe--get charge 'payment_method_details)))
         (type (mentat-stripe--get details 'type))
         (instrument (and type (mentat-stripe--get details (intern type)))))
    `((id . ,(mentat-stripe--get intent 'id))
      (created . ,(mentat-stripe--get intent 'created))
      (status . ,(mentat-stripe--get intent 'status))
      (amount . ,(mentat-stripe--get intent 'amount))
      (amount-received . ,(mentat-stripe--get intent 'amount_received))
      (currency . ,(mentat-stripe--get intent 'currency))
      (payment-method-id . ,(mentat-stripe--id
                             (mentat-stripe--get intent 'payment_method)))
      (payment-method-type . ,type)
      (brand . ,(mentat-stripe--get instrument 'brand))
      (last4 . ,(mentat-stripe--get instrument 'last4))
      (latest-charge . ,(mentat-stripe--charge-summary charge))
      (last-payment-error . ,(mentat-stripe--error-summary
                              (mentat-stripe--get intent 'last_payment_error))))))

(defun mentat-stripe--list-summary (data summarizer)
  "Summarize list response DATA with SUMMARIZER."
  `((has-more . ,(if (mentat-stripe--get data 'has_more) t :json-false))
    (items . ,(mapcar summarizer (mentat-stripe--get data 'data)))))

(defun mentat-stripe--valid-id-p (id prefix)
  "Return non-nil when ID has PREFIX and safe Stripe ID characters."
  (and (stringp id)
       (string-prefix-p prefix id)
       (> (length id) (length prefix))
       (not (string-match-p "[^[:alnum:]_]"
                            (substring id (length prefix))))))

(mentat-defun mentat-stripe-inspect-customer (customer-id)
  "Inspect non-contact Stripe CUSTOMER-ID account and payment-default state."
  (:execution async)
  (unless (mentat-stripe--valid-id-p customer-id "cus_")
    (error "CUSTOMER-ID must begin cus_"))
  (mentat-stripe--request-starter
   (format "/v1/customers/%s" customer-id) nil
   #'mentat-stripe--customer-summary))

(mentat-defun mentat-stripe-list-customer-payment-methods (customer-id &key (limit 100))
  "List bounded non-contact details for CUSTOMER-ID payment methods."
  (:execution async)
  (unless (mentat-stripe--valid-id-p customer-id "cus_")
    (error "CUSTOMER-ID must begin cus_"))
  (unless (and (integerp limit) (> limit 0) (<= limit 100))
    (error "LIMIT must be an integer from 1 to 100"))
  (mentat-stripe--request-starter
   (format "/v1/customers/%s/payment_methods" customer-id)
   `((limit . ,limit))
   (lambda (data)
     (mentat-stripe--list-summary data #'mentat-stripe--payment-method-summary))))

(mentat-defun mentat-stripe-list-customer-invoices (customer-id &key (limit 100))
  "List bounded invoice history for CUSTOMER-ID, newest first."
  (:execution async)
  (unless (mentat-stripe--valid-id-p customer-id "cus_")
    (error "CUSTOMER-ID must begin cus_"))
  (unless (and (integerp limit) (> limit 0) (<= limit 100))
    (error "LIMIT must be an integer from 1 to 100"))
  (mentat-stripe--request-starter
   "/v1/invoices" `((customer . ,customer-id) (limit . ,limit))
   (lambda (data)
     (mentat-stripe--list-summary data #'mentat-stripe--invoice-history-summary))))

(mentat-defun mentat-stripe-list-customer-payment-intents (customer-id &key (limit 100))
  "List bounded PaymentIntent history for CUSTOMER-ID, newest first."
  (:execution async)
  (unless (mentat-stripe--valid-id-p customer-id "cus_")
    (error "CUSTOMER-ID must begin cus_"))
  (unless (and (integerp limit) (> limit 0) (<= limit 100))
    (error "LIMIT must be an integer from 1 to 100"))
  (mentat-stripe--request-starter
   "/v1/payment_intents"
   `((customer . ,customer-id) (limit . ,limit)
     ("expand[]" . "data.latest_charge"))
   (lambda (data)
     (mentat-stripe--list-summary data #'mentat-stripe--intent-summary))))

(defun mentat-stripe--charge-history-summary (charge)
  "Return bounded history fields from CHARGE."
  (append `((created . ,(mentat-stripe--get charge 'created))
            (amount . ,(mentat-stripe--get charge 'amount))
            (currency . ,(mentat-stripe--get charge 'currency))
            (paid . ,(if (mentat-stripe--get charge 'paid) t :json-false)))
          (mentat-stripe--charge-summary charge)))

(defun mentat-stripe--setup-intent-summary (intent)
  "Return bounded history fields from SetupIntent INTENT."
  `((id . ,(mentat-stripe--get intent 'id))
    (created . ,(mentat-stripe--get intent 'created))
    (status . ,(mentat-stripe--get intent 'status))
    (usage . ,(mentat-stripe--get intent 'usage))
    (payment-method-id . ,(mentat-stripe--id
                           (mentat-stripe--get intent 'payment_method)))
    (last-setup-error . ,(mentat-stripe--error-summary
                          (mentat-stripe--get intent 'last_setup_error)))))

(mentat-defun mentat-stripe-list-customer-charges (customer-id &key (limit 100))
  "List bounded Charge history for CUSTOMER-ID, newest first."
  (:execution async)
  (unless (mentat-stripe--valid-id-p customer-id "cus_")
    (error "CUSTOMER-ID must begin cus_"))
  (unless (and (integerp limit) (> limit 0) (<= limit 100))
    (error "LIMIT must be an integer from 1 to 100"))
  (mentat-stripe--request-starter
   "/v1/charges" `((customer . ,customer-id) (limit . ,limit))
   (lambda (data)
     (mentat-stripe--list-summary data #'mentat-stripe--charge-history-summary))))

(mentat-defun mentat-stripe-list-customer-setup-intents (customer-id &key (limit 100))
  "List bounded SetupIntent history for CUSTOMER-ID, newest first."
  (:execution async)
  (unless (mentat-stripe--valid-id-p customer-id "cus_")
    (error "CUSTOMER-ID must begin cus_"))
  (unless (and (integerp limit) (> limit 0) (<= limit 100))
    (error "LIMIT must be an integer from 1 to 100"))
  (mentat-stripe--request-starter
   "/v1/setup_intents" `((customer . ,customer-id) (limit . ,limit))
   (lambda (data)
     (mentat-stripe--list-summary data #'mentat-stripe--setup-intent-summary))))

(provide 'stripe-api)
;;; stripe-api.el ends here
