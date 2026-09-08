;;; task-workflow-tests.el --- Tests for Bradwell task startup -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'task-workflow)

(defconst mentat-task-test--issue
  '((id . "issue-id")
    (identifier . "BRA-1484")
    (title . "Migrate Claude to AWS Bedrock")
    (description . "Use Bedrock for inference.")
    (url . "https://linear.app/example/BRA-1484")
    (team . ((states . ((nodes . (((id . "started-id")
                                    (name . "In Progress")
                                    (type . "started")))))))))
  "Representative Linear issue for task workflow tests.")

(defun mentat-task-test--run (identifier title description project)
  "Run a mocked workflow with IDENTIFIER or creation fields."
  (let (result failure cancelled fetched created-input handoff)
    (cl-letf (((symbol-function 'mentat-linear--starter)
               (lambda (start)
                 (lambda (resolve reject on-cancel)
                   (funcall on-cancel (lambda () (setq cancelled t)))
                   (funcall start "api-key" resolve reject))))
              ((symbol-function 'mentat-linear--get-issue)
               (lambda (_key issue-id success _failure)
                 (should (equal issue-id "BRA-1484"))
                 (funcall success mentat-task-test--issue)))
              ((symbol-function 'mentat-linear--create-issue)
               (lambda (_key new-title new-description team new-project
                        success _failure)
                 (setq created-input
                       (list new-title new-description team new-project))
                 (funcall success
                          `((success . t) (issue . ,mentat-task-test--issue)))))
              ((symbol-function 'mentat-task--fetch-main)
               (lambda (success _failure)
                 (setq fetched t)
                 (funcall success)
                 (lambda () (setq cancelled t))))
              ((symbol-function 'bob/create-worktree)
               (lambda (directory branch base success _failure)
                 (should (equal directory mentat-task-repository))
                 (should (equal branch "BRA-1484-migrate-claude-aws"))
                 (should (equal base "origin/main"))
                 (funcall success
                          '((directory . "/tmp/worktree/")
                            (branch . "BRA-1484-migrate-claude-aws")
                            (setup-processes . ("npm"))
                            (setup-buffers . ("*npm*"))))
                 (lambda () (setq cancelled t))))
              ((symbol-function 'mentat-session-manager--starter)
               (lambda (directory name prompt)
                 (should (equal directory "/tmp/worktree/"))
                 (should (string-prefix-p "BRA-1484 " name))
                 (setq handoff prompt)
                 (lambda (resolve _reject on-cancel)
                   (funcall on-cancel (lambda () (setq cancelled t)))
                   (funcall resolve '((session-id . "session-id")))))))
      (funcall (mentat-task--start identifier title description project)
               (lambda (value) (setq result value))
               (lambda (reason) (setq failure reason))
               (lambda (_cancel)))
      (list :result result :failure failure :cancelled cancelled
            :fetched fetched :created-input created-input :handoff handoff))))

(ert-deftest mentat-task-starts-existing-issue-end-to-end ()
  (let ((run (mentat-task-test--run "BRA-1484" nil nil nil)))
    (should (plist-get run :fetched))
    (should-not (plist-get run :failure))
    (should-not (plist-get run :created-input))
    (should (equal (alist-get 'directory (plist-get run :result))
                   "/tmp/worktree/"))
    (should (equal (alist-get 'id
                              (alist-get 'in-progress-state
                                         (plist-get run :result)))
                   "started-id"))
    (should (string-match-p "Wait for further user instructions"
                            (plist-get run :handoff)))))

(ert-deftest mentat-task-creates-confirmed-issue-before-starting ()
  (let ((run (mentat-task-test--run
              nil "Migrate Claude to AWS Bedrock"
              "Use Bedrock for inference." "Assistant")))
    (should-not (plist-get run :failure))
    (should (equal (plist-get run :created-input)
                   '("Migrate Claude to AWS Bedrock"
                     "Use Bedrock for inference." "BRA" "Assistant")))))

(ert-deftest mentat-task-linear-creation-resolves-team-project-and-viewer ()
  (let ((context
         (json-parse-string
          "{\"viewer\":{\"id\":\"viewer-id\"},\"teams\":{\"nodes\":[{\"id\":\"team-id\",\"key\":\"BRA\"}]},\"projects\":{\"nodes\":[{\"id\":\"other-project\",\"teams\":{\"nodes\":[{\"id\":\"other-team\"}]}},{\"id\":\"project-id\",\"teams\":{\"nodes\":[{\"id\":\"team-id\"}]}}]}}"
          :object-type 'alist :array-type 'list))
        request variables failure)
    (cl-letf (((symbol-function 'mentat-linear--request)
               (lambda (_key query input _success _failure)
                 (setq request query variables input))))
      (mentat-linear--create-issue-with-context
       "api-key" "Title" "Description" "BRA" "Assistant"
       #'ignore (lambda (reason) (setq failure reason)) context))
    (should-not failure)
    (should (string-match-p "issueCreate" request))
    (should (equal (alist-get 'teamId variables) "team-id"))
    (should (equal (alist-get 'projectId variables) "project-id"))
    (should (equal (alist-get 'assigneeId variables) "viewer-id"))))

(ert-deftest mentat-task-linear-creation-rejects-ambiguous-project ()
  (let ((context
         (json-parse-string
          "{\"viewer\":{\"id\":\"viewer-id\"},\"teams\":{\"nodes\":[{\"id\":\"team-id\",\"key\":\"BRA\"}]},\"projects\":{\"nodes\":[{\"id\":\"project-1\",\"teams\":{\"nodes\":[{\"id\":\"team-id\"}]}},{\"id\":\"project-2\",\"teams\":{\"nodes\":[{\"id\":\"team-id\"}]}}]}}"
          :object-type 'alist :array-type 'list))
        requested failure)
    (cl-letf (((symbol-function 'mentat-linear--request)
               (lambda (&rest _args) (setq requested t))))
      (mentat-linear--create-issue-with-context
       "api-key" "Title" "Description" "BRA" "Assistant"
       #'ignore (lambda (reason) (setq failure reason)) context))
    (should-not requested)
    (should (string-match-p "found 2" failure))))

(ert-deftest mentat-task-clean-worktree-passes-explicit-remote-policy ()
  (let (result failure cancellation policy directory)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_path) t))
              ((symbol-function 'file-truename) #'identity)
              ((symbol-function 'file-in-directory-p)
               (lambda (_path _parent) t))
              ((symbol-function 'bob/clean-worktree)
               (lambda (path selected success _failure)
                 (setq directory path policy selected)
                 (funcall success '((confirmation-required . t)))
                 (lambda () (setq cancellation 'called)))))
      (funcall (mentat-worktree-clean "/tmp/worktree" "check")
               (lambda (value) (setq result value))
               (lambda (reason) (setq failure reason))
               (lambda (cleanup) (setq cancellation cleanup))))
    (should-not failure)
    (should (eq policy 'check))
    (should (equal directory "/tmp/worktree"))
    (should (alist-get 'confirmation-required result))
    (should (functionp cancellation))))

(provide 'task-workflow-tests)
;;; task-workflow-tests.el ends here
