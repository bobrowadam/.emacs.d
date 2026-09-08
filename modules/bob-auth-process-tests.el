;;; bob-auth-process-tests.el --- Authenticated process tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'bob-auth-process)

(defun bob/auth-process-test-wait (done)
  "Wait at most five seconds for DONE to return non-nil."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall done)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall done))))

(ert-deftest bob/auth-process-child-environment-is-isolated ()
  (let ((process-environment (copy-sequence process-environment))
        (output "") status child lookups)
    (setenv "BOB_TEST_FIRST" "parent")
    (setenv "BOB_TEST_SECOND" nil)
    (cl-letf (((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest selectors)
                 (push selectors lookups)
                 (should-not (plist-get selectors :create))
                 (pcase (plist-get selectors :host)
                   ("first.test" "fake-first")
                   ("second.test" "fake-second")))))
      (unwind-protect
          (progn
            (setq child
                  (bob/start-process-with-credentials
                   "/bin/sh"
                   '("-c" "test \"$BOB_TEST_FIRST\" = fake-first && test \"$BOB_TEST_SECOND\" = fake-second && test \"$PWD\" = / && printf verified")
                   '(("BOB_TEST_FIRST" "first.test" "one")
                     ("BOB_TEST_SECOND" "second.test" "two"))
                   :directory "/"
                   :filter (lambda (_process chunk) (setq output (concat output chunk)))
                   :sentinel (lambda (process _event)
                               (when (memq (process-status process) '(exit signal))
                                 (setq status (process-exit-status process))))))
            (process-send-eof child)
            (should (equal (getenv "BOB_TEST_FIRST") "parent"))
            (should-not (getenv "BOB_TEST_SECOND"))
            (bob/auth-process-test-wait (lambda () status))
            (should (= status 0))
            (should (equal output "verified"))
            (should (= (length lookups) 2))
            (should (equal (mapcar (lambda (s) (plist-get s :user)) lookups)
                           '("two" "one"))))
        (when (process-live-p child) (delete-process child))))))

(ert-deftest bob/auth-process-missing-secret-prevents-launch ()
  (let ((process-environment (copy-sequence process-environment)) launched)
    (setenv "BOB_TEST_FIRST" nil)
    (cl-letf (((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest selectors)
                 (when (equal (plist-get selectors :host) "first.test")
                   "fake-first")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (setq launched t))))
      (should-error
       (bob/start-process-with-credentials
        "/bin/true" nil '(("BOB_TEST_FIRST" "first.test" "one")
                           ("BOB_TEST_SECOND" "missing.test" "two")))
       :type 'user-error)
      (should-not launched)
      (should-not (getenv "BOB_TEST_FIRST")))))

(ert-deftest bob/auth-process-errors-are-sanitized ()
  (dolist (launch-error '(nil t))
    (cl-letf (((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest _)
                 (if launch-error "fake-sensitive-value"
                   (error "Backend exposed fake-sensitive-value"))))
              ((symbol-function 'make-process)
               (lambda (&rest _) (error "Launch exposed fake-sensitive-value"))))
      (let ((err (should-error
                  (bob/start-process-with-credentials
                   "/bin/true" nil '(("BOB_TEST_FIRST" "test" "user"))))))
        (should (equal (error-message-string err)
                       "Credential lookup or process launch failed"))))))

(ert-deftest bob/auth-process-invalid-mappings-do-not-lookup ()
  (cl-letf (((symbol-function 'auth-source-pick-first-password)
             (lambda (&rest _) (ert-fail "Unexpected credential lookup"))))
    (dolist (mappings '(nil (("BAD=NAME" "host" "user"))
                       (("GOOD" "" "user"))
                       (("GOOD" "host" "user") ("GOOD" "other" "user"))))
      (should-error (bob/start-process-with-credentials "/bin/true" nil mappings)
                    :type 'user-error))))

(ert-deftest bob/auth-process-adapter-output-and-exit ()
  (require 'auth-process)
  (let (result rejection cancel snapshots)
    (cl-letf (((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest _) "fake-only")))
      (unwind-protect
          (progn
            (funcall (mentat-run-process-with-credentials
                      "/bin/sh" '("-c" "printf stdout; printf stderr >&2; exit 7")
                      '(("BOB_TEST_FIRST" "test" "user")))
                     (lambda (value) (setq result value))
                     (lambda (reason) (setq rejection reason))
                     (lambda (cleanup) (setq cancel cleanup))
                     (lambda (text) (push text snapshots)))
            (bob/auth-process-test-wait (lambda () (or result rejection)))
            (should-not rejection)
            (should (= (mentat--registered-process-result-exit-code result) 7))
            (should (equal (mentat--registered-process-result-output result)
                           "stdoutstderr"))
            (should (equal (car snapshots) "stdoutstderr")))
        (when cancel (funcall cancel))))))

(ert-deftest bob/auth-process-adapter-output-is-bounded ()
  (require 'auth-process)
  (let ((mentat--async-process-output-limit 256) result rejection cancel)
    (cl-letf (((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest _) "fake-only")))
      (unwind-protect
          (progn
            (funcall (mentat-run-process-with-credentials
                      "/usr/bin/printf" (list "%s" (make-string 6000 ?λ))
                      '(("BOB_TEST_FIRST" "test" "user")))
                     (lambda (value) (setq result value))
                     (lambda (reason) (setq rejection reason))
                     (lambda (cleanup) (setq cancel cleanup)))
            (bob/auth-process-test-wait (lambda () (or result rejection)))
            (should-not rejection)
            (let ((output (mentat--registered-process-result-output result)))
              (should (<= (string-bytes output) 256))
              (should (string-suffix-p "… [output truncated]" output))))
        (when cancel (funcall cancel))))))

(ert-deftest bob/auth-process-adapter-cancellation ()
  (require 'auth-process)
  (let ((start (symbol-function 'bob/start-process-with-credentials))
        child cancel settled)
    (cl-letf (((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest _) "fake-only"))
              ((symbol-function 'bob/start-process-with-credentials)
               (lambda (&rest args) (setq child (apply start args)))))
      (unwind-protect
          (progn
            (funcall (mentat-run-process-with-credentials
                      "/bin/sleep" '("30") '(("BOB_TEST_FIRST" "test" "user")))
                     (lambda (_) (setq settled t))
                     (lambda (_) (setq settled t))
                     (lambda (cleanup) (setq cancel cleanup)))
            (should (process-live-p child))
            (funcall cancel)
            (should-not (process-live-p child))
            (should-not settled))
        (when (process-live-p child) (delete-process child))))))

;;; bob-auth-process-tests.el ends here
