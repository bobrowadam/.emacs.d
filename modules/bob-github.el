;;; bob-github.el --- Submit GitHub inline reviews -*- lexical-binding: t; -*-

;;; Commentary:
;; Use the authenticated gh CLI to submit a review against an explicit commit.
;; This module does not depend on Mentat.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'rx)
(require 'subr-x)

(defun bob/github--api (endpoint payload query success failure directory)
  "Run a gh API request and return its cancellation function.
ENDPOINT and PAYLOAD select the request.  QUERY transforms its parsed JSON.
Call SUCCESS with parsed JSON, or FAILURE with an error string.
Run in DIRECTORY using gh's existing authentication."
  (let ((buffer (generate-new-buffer " *github-api*"))
        (default-directory (or directory default-directory))
        process finished)
    (cl-labels
        ((cleanup ()
           (when (buffer-live-p buffer) (kill-buffer buffer)))
         (cancel ()
           (setq finished t)
           (when (process-live-p process) (delete-process process))
           (cleanup))
         (finish (child _event)
           (when (and (not finished)
                      (memq (process-status child) '(exit signal)))
             (setq finished t)
             (let ((output (with-current-buffer buffer (buffer-string)))
                   result error-message)
               (cleanup)
               (if (and (eq (process-status child) 'exit)
                        (zerop (process-exit-status child)))
                   (condition-case err
                       (setq result (funcall query
                                            (json-parse-string
                                             output :object-type 'alist
                                             :array-type 'list :null-object nil
                                             :false-object :false)))
                     (error (setq error-message (error-message-string err))))
                 (setq error-message
                       (format "gh exited %s: %s"
                               (process-exit-status child)
                               (string-trim output))))
               (if error-message
                   (funcall failure error-message)
                 (funcall success result))))))
      (condition-case err
          (progn
            (setq process
                  (make-process
                   :name "github-api" :buffer buffer
                   :command (append
                             (list "gh" "api" endpoint "--method"
                                   (if payload "POST" "GET")
                                   "-H" "Accept: application/vnd.github+json")
                             (if payload '("--input" "-")
                               '("--paginate" "--slurp")))
                   :connection-type 'pipe :coding 'utf-8-unix
                   :noquery t :sentinel #'finish))
            (when payload
              (process-send-string process (json-serialize payload)))
            (process-send-eof process))
        (error
         (cancel)
         (funcall failure (error-message-string err))))
      #'cancel)))

(defun bob/github--comments (comments)
  "Validate COMMENTS and return a vector of GitHub line comment objects."
  (unless (and (listp comments) comments)
    (user-error "Provide at least one inline comment"))
  (vconcat
   (mapcar
    (lambda (comment)
      (let ((path (alist-get 'path comment))
            (line (alist-get 'line comment))
            (body (alist-get 'body comment))
            (side (or (alist-get 'side comment) "RIGHT")))
        (unless (and (stringp path) (not (string-blank-p path))
                     (integerp line) (> line 0)
                     (stringp body) (not (string-blank-p body))
                     (member side '("LEFT" "RIGHT")))
          (user-error "Each comment needs path, positive line, body, and optional LEFT/RIGHT side"))
        `((path . ,path) (line . ,line) (side . ,side) (body . ,body))))
    comments)))

(cl-defun bob/github-submit-inline-review
    (repository pull-number reviewed-commit event comments success failure
                &key body directory)
  "Submit an authorized inline review and return a cancellation function.
REPOSITORY is owner/repo, PULL-NUMBER is a positive integer, REVIEWED-COMMIT
is the full reviewed SHA, and EVENT is APPROVE, COMMENT or REQUEST_CHANGES.
COMMENTS is a list of alists with path, line, body and optional side keys.
BODY is required for COMMENT and REQUEST_CHANGES.  Run gh in DIRECTORY.
Call SUCCESS with the review metadata and comment links, or FAILURE with
an error string.  Refuse a changed head before posting.  Never retry a write;
a failed or cancelled submission may still have reached GitHub."
  (unless (and (stringp repository)
               (string-match-p
                (rx string-start (+ (any alnum "_.-")) "/"
                    (+ (any alnum "_.-")) string-end)
                repository)
               (integerp pull-number) (> pull-number 0)
               (stringp reviewed-commit)
               (string-match-p (rx string-start (= 40 xdigit) string-end)
                               reviewed-commit)
               (member event '("APPROVE" "COMMENT" "REQUEST_CHANGES")))
    (user-error "Provide owner/repo, positive PR number, full commit SHA and review event"))
  (unless (or (and (equal event "APPROVE") (null body))
              (and (stringp body) (not (string-blank-p body))))
    (user-error "Provide a nonblank review body for COMMENT or REQUEST_CHANGES"))
  (let* ((comments (bob/github--comments comments))
         (endpoint (format "repos/%s/pulls/%d" repository pull-number))
         (payload `((commit_id . ,reviewed-commit) (event . ,event)
                    (comments . ,comments)))
         cancel-current settled submitting review)
    (when body (push (cons 'body body) payload))
    (cl-labels
        ((fail (reason)
           (unless settled
             (setq settled t)
             (funcall failure
                      (cond
                       (review
                        (format "Review submitted: %s (%s), but comment verification failed: %s. Do not resubmit."
                                (alist-get 'html_url review)
                                (alist-get 'state review) reason))
                       (submitting
                        (format "Review submission failed: %s. It may have reached GitHub; inspect existing reviews before retrying." reason))
                       (t reason)))))
         (request (path data query callback)
           (setq cancel-current
                 (bob/github--api path data query callback #'fail directory)))
         (comments-ready (value)
           (unless settled
             (setq settled t)
             (funcall success (append review `((comments . ,value))))))
         (submitted (value)
           (unless settled
             (setq review value)
             (request
              (format "%s/reviews/%s/comments?per_page=100"
                      endpoint (alist-get 'id review))
              nil
              (lambda (pages)
                (mapcar (lambda (comment)
                          `((path . ,(alist-get 'path comment))
                            (html_url . ,(alist-get 'html_url comment))))
                        (apply #'append pages)))
              #'comments-ready)))
         (head-ready (pull)
           (unless settled
             (cond
              ((not (equal (alist-get 'state pull) "open"))
               (fail "The PR is not open; nothing was submitted"))
              ((not (equal (alist-get 'sha pull) reviewed-commit))
               (fail (format "PR head changed: reviewed %s, current %s. Nothing was submitted."
                             reviewed-commit (alist-get 'sha pull))))
              (t
               (setq submitting t)
               (request
                (concat endpoint "/reviews") payload
                (lambda (value)
                  (mapcar (lambda (key) (cons key (alist-get key value)))
                          '(id state html_url commit_id)))
                #'submitted))))))
      (request endpoint nil
               (lambda (pages)
                 (let ((pull (car pages)))
                   `((state . ,(alist-get 'state pull))
                     (sha . ,(alist-get 'sha (alist-get 'head pull))))))
               #'head-ready)
      (lambda ()
        (setq settled t)
        (when cancel-current (funcall cancel-current))))))

(provide 'bob-github)
;;; bob-github.el ends here
