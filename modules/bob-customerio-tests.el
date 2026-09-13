;;; bob-customerio-tests.el --- Customer.io tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Test credential isolation, request completion and cancellation.

;;; Code:
(require 'ert)
(require 'bob-customerio)

(ert-deftest bob/customerio-invalid-input ()
  (should-error (bob/customerio-path "send"))
  (should-error (bob/customerio-path "template" "../workspaces"))
  (should-error (bob/customerio-read "unknown" "templates" nil #'ignore #'ignore)))

(ert-deftest bob/customerio-request-lifecycle ()
  (let (sentinel callback headers credential-buffer response-buffer result failure)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "aws"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq credential-buffer (plist-get args :buffer)
                       sentinel (plist-get args :sentinel))
                 (with-current-buffer credential-buffer
                   (insert "{\"Parameter\":{\"Value\":\"synthetic-test-key\"}}"))
                 'test-process))
              ((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) 0))
              ((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'url-retrieve)
               (lambda (url cb &rest _)
                 (should (equal url "https://api.customer.io/v1/transactional"))
                 (setq callback cb headers url-request-extra-headers
                       response-buffer (generate-new-buffer " *customerio-test-http*")))))
      (bob/customerio-read "dev" "templates" nil
                           (lambda (data) (setq result data))
                           (lambda (error) (setq failure error)))
      (funcall sentinel 'test-process "finished")
      (should-not (buffer-live-p credential-buffer))
      (should (equal (cdr (assoc "Authorization" headers)) "Bearer synthetic-test-key"))
      (with-current-buffer response-buffer
        (setq-local url-http-response-status 200)
        (setq-local url-http-end-of-headers (point-min))
        (insert "{\"messages\":[],\"enabled\":false}")
        (funcall callback nil))
      (should-not failure)
      (should (eq (alist-get 'enabled result) :false))
      (should (equal (alist-get 'messages result) []))
      (should-not (buffer-live-p response-buffer)))))

(ert-deftest bob/customerio-cancel-before-credentials ()
  (let (sentinel credential-buffer called deleted)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "aws"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq credential-buffer (plist-get args :buffer)
                       sentinel (plist-get args :sentinel))
                 'test-process))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process) (lambda (_) (setq deleted t))))
      (let ((cancel (bob/customerio-read "dev" "templates" nil
                                            (lambda (_) (setq called t))
                                            (lambda (_) (setq called t)))))
        (funcall cancel)
        (funcall sentinel 'test-process "killed")
        (funcall cancel)
        (should deleted)
        (should-not called)
        (should-not (buffer-live-p credential-buffer))))))

(provide 'bob-customerio-tests)
;;; bob-customerio-tests.el ends here
