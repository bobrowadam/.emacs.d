;;; linear-api-tests.el --- Tests for Linear requests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'linear-api)
(require 'url-http)

(ert-deftest mentat-linear-request-builds-unicode-http-request ()
  (let ((description "Golem’s investigations time out. 日本語")
        request)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url &rest _args)
                 (with-temp-buffer
                   (setq-local url-http-target-url (url-generic-parse-url url)
                               url-http-extra-headers url-request-extra-headers
                               url-http-data url-request-data
                               url-http-method url-request-method
                               url-http-proxy nil
                               url-http-referer nil
                               url-http-extensions-header nil
                               url-http-version "1.1"
                               url-http-attempt-keepalives nil
                               url-mime-encoding-string nil
                               url-mime-charset-string nil
                               url-mime-language-string nil
                               url-mime-accept-string "*/*"
                               url-user-agent nil)
                   (setq request (url-http-create-request))))))
      (mentat-linear--request
       (string-to-multibyte "test-only-api-key")
       "mutation { issueCreate }" `((description . ,description))
       #'ignore (lambda (reason) (ert-fail reason))))
    (should (= (string-bytes request) (length request)))
    (let* ((body-start (+ (string-match "\r\n\r\n" request) 4))
           (body (substring request body-start))
           (payload (json-parse-string
                     (decode-coding-string (string-as-unibyte body) 'utf-8)
                     :object-type 'alist)))
      (should (string-match-p
               (format "Content-length: %d\r\n" (length body)) request))
      (should (equal (alist-get 'description (alist-get 'variables payload))
                     description)))))

(ert-deftest mentat-linear-request-encodes-unicode-as-utf8-bytes ()
  (let ((description "Golem’s investigations time out. 日本語")
        body)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _args)
                 (setq body url-request-data)
                 'request-buffer)))
      (should
       (eq (mentat-linear--request
            "test-only-api-key" "mutation { issueCreate }"
            `((description . ,description)) #'ignore #'ignore)
           'request-buffer)))
    (should-not (multibyte-string-p body))
    (let ((payload (json-parse-string
                    (decode-coding-string body 'utf-8)
                    :object-type 'alist)))
      (should (equal (alist-get 'description (alist-get 'variables payload))
                     description)))))

(ert-deftest mentat-linear-request-redacts-synchronous-errors ()
  (let ((key "test-only-api-key")
        failure)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _args)
                 (error "HTTP request failed: Authorization: %s" key))))
      (mentat-linear--request
       key "query { viewer { id } }" nil #'ignore
       (lambda (reason) (setq failure reason))))
    (should (stringp failure))
    (should (string-match-p "HTTP request failed" failure))
    (should (string-match-p "\\[REDACTED\\]" failure))
    (should-not (string-match-p (regexp-quote key) failure))))

(provide 'linear-api-tests)
;;; linear-api-tests.el ends here
