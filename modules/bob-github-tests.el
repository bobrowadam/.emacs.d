;;; bob-github-tests.el --- GitHub review tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'bob-github)

(defconst bob/github-test--sha (make-string 40 ?a))
(defconst bob/github-test--comment
  '((path . "src/example.ts") (line . 7) (body . "A review comment.")))

(cl-defun bob/github-test--run
    (responses &key (event "APPROVE") body
               (comments (list bob/github-test--comment)))
  "Run a review against RESPONSES without contacting GitHub."
  (let (calls result failure)
    (cl-letf (((symbol-function 'bob/github--api)
               (lambda (endpoint payload _query success reject _directory)
                 (push (list endpoint payload) calls)
                 (let ((response (pop responses)))
                   (if (eq (car response) :error)
                       (funcall reject (cadr response))
                     (funcall success response)))
                 #'ignore)))
      (bob/github-submit-inline-review
       "owner/repo" 42 bob/github-test--sha event comments
       (lambda (value) (setq result value))
       (lambda (value) (setq failure value)) :body body))
    (list :calls (nreverse calls) :result result :failure failure)))

(ert-deftest bob/github-review-refuses-changed-head ()
  (let ((result (bob/github-test--run
                 '(((state . "open") (sha . "different"))))))
    (should (= (length (plist-get result :calls)) 1))
    (should-not (cadar (plist-get result :calls)))
    (should (string-match-p "PR head changed" (plist-get result :failure)))))

(ert-deftest bob/github-review-refuses-closed-pr ()
  (let ((result (bob/github-test--run
                 `(((state . "closed") (sha . ,bob/github-test--sha))))))
    (should (= (length (plist-get result :calls)) 1))
    (should (string-match-p "not open" (plist-get result :failure)))))

(ert-deftest bob/github-review-submits-once-with-commit-and-lines ()
  (let* ((result (bob/github-test--run
                  `(((state . "open") (sha . ,bob/github-test--sha))
                    ((id . 99) (state . "APPROVED")
                     (html_url . "https://github.com/review/99")
                     (commit_id . ,bob/github-test--sha))
                    (((path . "src/example.ts")
                      (html_url . "https://github.com/comment/1"))))))
         (calls (plist-get result :calls))
         (payload (cadr (nth 1 calls))))
    (should-not (plist-get result :failure))
    (should (= (length calls) 3))
    (should (equal (car (nth 1 calls)) "repos/owner/repo/pulls/42/reviews"))
    (should (equal (alist-get 'commit_id payload) bob/github-test--sha))
    (should (equal (alist-get 'event payload) "APPROVE"))
    (should (equal (alist-get 'comments payload)
                   [((path . "src/example.ts") (line . 7)
                     (side . "RIGHT") (body . "A review comment."))]))
    (should-not (alist-get 'body payload))
    (should (equal (alist-get 'state (plist-get result :result)) "APPROVED"))
    (should (equal (alist-get 'html_url
                             (car (alist-get 'comments (plist-get result :result))))
                   "https://github.com/comment/1"))))

(ert-deftest bob/github-review-preserves-decision-body-and-left-side ()
  (let* ((result (bob/github-test--run
                  `(((state . "open") (sha . ,bob/github-test--sha))
                    ((id . 99) (state . "CHANGES_REQUESTED")) nil)
                  :event "REQUEST_CHANGES" :body "Please address this issue."
                  :comments (list (cons '(side . "LEFT") bob/github-test--comment))))
         (payload (cadr (nth 1 (plist-get result :calls)))))
    (should (equal (alist-get 'event payload) "REQUEST_CHANGES"))
    (should (equal (alist-get 'body payload) "Please address this issue."))
    (should (equal (alist-get 'side (aref (alist-get 'comments payload) 0)) "LEFT"))))

(ert-deftest bob/github-review-validates-before-contacting-github ()
  (cl-letf (((symbol-function 'bob/github--api)
             (lambda (&rest _) (ert-fail "Unexpected GitHub request"))))
    (dolist (input '(("owner/repo" 42 "short-sha" "APPROVE")
                     ("wrong-repository" 42 nil "APPROVE")
                     ("owner/repo" 0 nil "APPROVE")
                     ("owner/repo" 42 nil "INVALID")
                     ("owner/repo" 42 nil "COMMENT")))
      (should-error
       (bob/github-submit-inline-review
        (nth 0 input) (nth 1 input) (or (nth 2 input) bob/github-test--sha)
        (nth 3 input) (list bob/github-test--comment) #'ignore #'ignore)
       :type 'user-error))
    (dolist (comments '(nil (((path . "a") (line . 0) (body . "text")))
                           (((path . "a") (line . 1) (body . " ")))
                           (((path . "a") (line . 1) (body . "text")
                             (side . "INVALID")))))
      (should-error
       (bob/github-submit-inline-review
        "owner/repo" 42 bob/github-test--sha "APPROVE" comments #'ignore #'ignore)
       :type 'user-error))))

(ert-deftest bob/github-review-never-retries-an-uncertain-write ()
  (let ((result (bob/github-test--run
                 `(((state . "open") (sha . ,bob/github-test--sha))
                   (:error "Connection closed")))))
    (should (= (length (plist-get result :calls)) 2))
    (should (string-match-p "may have reached GitHub" (plist-get result :failure)))
    (should-not (plist-get result :result))))

(ert-deftest bob/github-review-reports-submitted-review-on-readback-failure ()
  (let ((result (bob/github-test--run
                 `(((state . "open") (sha . ,bob/github-test--sha))
                   ((id . 99) (state . "APPROVED")
                    (html_url . "https://github.com/review/99"))
                   (:error "Read failed")))))
    (should (= (length (plist-get result :calls)) 3))
    (should (string-match-p "https://github.com/review/99" (plist-get result :failure)))
    (should (string-match-p "Do not resubmit" (plist-get result :failure)))))

(ert-deftest bob/github-review-cancellation-stops-next-request ()
  (let (head-ready cancelled completed)
    (cl-letf (((symbol-function 'bob/github--api)
               (lambda (_endpoint _payload _query success _failure _directory)
                 (setq head-ready success)
                 (lambda () (setq cancelled t)))))
      (let ((cancel (bob/github-submit-inline-review
                     "owner/repo" 42 bob/github-test--sha "APPROVE"
                     (list bob/github-test--comment)
                     (lambda (_) (setq completed t))
                     (lambda (_) (setq completed t)))))
        (funcall cancel)
        (should cancelled)
        (funcall head-ready `((state . "open") (sha . ,bob/github-test--sha)))
        (should-not completed)))))

(ert-deftest bob/github-api-sends-json-and-handles-process-completion ()
  (let ((original (symbol-function 'make-process))
        command result failure done cancel
        (payload '((event . "APPROVE")
                   (comments . [((body . "Policy’s structure"))]))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest options)
                       (setq command (plist-get options :command))
                       ;; Echo stdin through a real process, never GitHub.
                       (apply original
                              (plist-put options :command '("/bin/cat"))))))
            (setq cancel
                  (bob/github--api
                   "repos/owner/repo/pulls/42/reviews" payload #'identity
                   (lambda (value) (setq result value done t))
                   (lambda (value) (setq failure value done t)) nil)))
          (let ((deadline (+ (float-time) 3)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should done)
          (should-not failure)
          (should (member "POST" command))
          (should (member "--input" command))
          (should-not (member "--slurp" command))
          (should (equal (alist-get 'event result) "APPROVE"))
          (should (equal (alist-get 'body (car (alist-get 'comments result)))
                         "Policy’s structure")))
      (when cancel (funcall cancel)))))

(provide 'bob-github-tests)
;;; bob-github-tests.el ends here
