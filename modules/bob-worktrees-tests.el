;;; bob-worktrees-tests.el --- Worktree regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'bob-worktrees)

(defun bob/worktree-test-git (&rest args)
  "Run Git ARGS synchronously in the batch test process."
  (with-temp-buffer
    (should (zerop (apply #'process-file "git" nil t nil args)))
    (string-trim (buffer-string))))

(defmacro bob/with-worktree-test-repo (&rest body)
  "Run BODY with an isolated repository and project list."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "bob worktree test " t))
          (default-directory (file-name-as-directory root))
          (project-list-file (expand-file-name "projects" root))
          (project--list nil)
          (bob/worktree-setup-functions nil))
     (unwind-protect
         (progn
           (bob/worktree-test-git "init" "-b" "main")
           (bob/worktree-test-git "-c" "user.name=Worktree Test"
                                  "-c" "user.email=test@example.invalid"
                                  "-c" "commit.gpgsign=false"
                                  "-c" "core.hooksPath=/dev/null"
                                  "commit" "--allow-empty" "-m" "Initial")
           ,@body)
       (delete-directory root t))))

(defun bob/worktree-test-create (directory branch start-point)
  "Wait for creation from DIRECTORY using BRANCH and START-POINT."
  (let (result
        (deadline (+ (float-time) 10)))
    (let ((cancel (bob/create-worktree
                   directory branch start-point
                   (lambda (value) (setq result (cons 'ok value)))
                   (lambda (text) (setq result (cons 'error text))))))
      (unwind-protect
          (progn
            (while (and (not result) (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (should result)
            result)
        (unless result (funcall cancel))))))

(defun bob/worktree-test-clean (directory remote-policy)
  "Wait for cleanup of DIRECTORY using REMOTE-POLICY."
  (let (result
        (deadline (+ (float-time) 10)))
    (let ((cancel (bob/clean-worktree
                   directory remote-policy
                   (lambda (value) (setq result (cons 'ok value)))
                   (lambda (text) (setq result (cons 'error text))))))
      (unwind-protect
          (progn
            (while (and (not result) (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (should result)
            result)
        (unless result (funcall cancel))))))

(ert-deftest bob/worktree-nested-creation-stays-under-main ()
  (bob/with-worktree-test-repo
    (let* ((setups nil)
           (bob/worktree-setup-functions
            (list (lambda (path) (push path setups) nil)))
           (first (bob/worktree-test-create root "task/first" "main"))
           (first-path (alist-get 'directory (cdr first)))
           (second (bob/worktree-test-create first-path "task/second" "main"))
           (second-path (alist-get 'directory (cdr second))))
      (should (eq (car first) 'ok))
      (should (eq (car second) 'ok))
      (should (file-equal-p second-path (expand-file-name ".worktrees/task-second" root)))
      (should (= (length setups) 2))
      (should-not (file-exists-p (expand-file-name ".worktrees" first-path)))
      (let ((default-directory (file-name-as-directory second-path)))
        (should (equal (bob/worktree-test-git "branch" "--show-current")
                       "task/second"))))))

(ert-deftest bob/worktree-existing-destination-is-preserved ()
  (bob/with-worktree-test-repo
    (let ((target (expand-file-name ".worktrees/existing" root)))
      (make-directory (file-name-directory target) t)
      (with-temp-file target (insert "keep me"))
      (let ((result (bob/worktree-test-create root "existing" "main")))
        (should (eq (car result) 'error))
        (should (string-match-p "already exists" (cdr result))))
      (should (equal (with-temp-buffer (insert-file-contents target) (buffer-string))
                     "keep me")))))

(ert-deftest bob/worktree-git-failure-does-not-run-setup ()
  (bob/with-worktree-test-repo
    (let* ((called nil)
           (bob/worktree-setup-functions (list (lambda (_path) (setq called t)))))
      (dolist (case '(("new" "missing-revision") ("main" "main")
                      ("../escape" "main")))
        (should (eq (car (bob/worktree-test-create root (car case) (cadr case)))
                    'error)))
      (should-not called)
      (should-not (file-exists-p (expand-file-name ".worktrees/new" root))))))

(ert-deftest bob/worktree-clean-skips-prompt-for-missing-remote-branch ()
  (bob/with-worktree-test-repo
    (let ((remote (expand-file-name "remote.git" root))
          prompted)
      (bob/worktree-test-git "init" "--bare" remote)
      (bob/worktree-test-git "remote" "add" "origin" remote)
      (let* ((created (bob/worktree-test-create root "local-only" "main"))
             (path (alist-get 'directory (cdr created)))
             (cleaned
              (bob/worktree-test-clean
               path (lambda (_branch) (setq prompted t) t))))
        (should (eq (car cleaned) 'ok))
        (should-not prompted)
        (should-not (file-exists-p path))
        (should-not (string-match-p
                     "local-only"
                     (bob/worktree-test-git "branch" "--format=%(refname:short)")))))))

(ert-deftest bob/worktree-clean-check-preserves-existing-remote-branch ()
  (bob/with-worktree-test-repo
    (let ((remote (expand-file-name "remote.git" root)))
      (bob/worktree-test-git "init" "--bare" remote)
      (bob/worktree-test-git "remote" "add" "origin" remote)
      (let* ((created (bob/worktree-test-create root "needs-confirmation" "main"))
             (path (alist-get 'directory (cdr created))))
        (let ((default-directory (file-name-as-directory path)))
          (bob/worktree-test-git "push" "origin" "needs-confirmation"))
        (let ((checked (bob/worktree-test-clean path 'check)))
          (should (eq (car checked) 'ok))
          (should (alist-get 'confirmation-required (cdr checked)))
          (should (file-directory-p path))
          (should (string-match-p
                   "needs-confirmation"
                   (bob/worktree-test-git "branch" "--format=%(refname:short)")))
          (with-temp-buffer
            (should (zerop
                     (process-file
                      "git" nil t nil "ls-remote" "--exit-code" "--heads"
                      "origin" "refs/heads/needs-confirmation")))))))))

(ert-deftest bob/worktree-clean-prompts-for-existing-remote-branch ()
  (bob/with-worktree-test-repo
    (let ((remote (expand-file-name "remote.git" root))
          prompted)
      (bob/worktree-test-git "init" "--bare" remote)
      (bob/worktree-test-git "remote" "add" "origin" remote)
      (let* ((created (bob/worktree-test-create root "published" "main"))
             (path (alist-get 'directory (cdr created))))
        (let ((default-directory (file-name-as-directory path)))
          (bob/worktree-test-git "push" "origin" "published"))
        (let ((cleaned
               (bob/worktree-test-clean
                path (lambda (branch)
                       (setq prompted branch)
                       t))))
          (should (eq (car cleaned) 'ok))
          (should (equal prompted "published"))
          (let ((default-directory (file-name-as-directory root)))
            (with-temp-buffer
              (should (= (process-file
                          "git" nil t nil "ls-remote" "--exit-code" "--heads"
                          "origin" "refs/heads/published")
                         2)))))))))

(ert-deftest bob/worktree-setup-failure-reports-created-path ()
  (bob/with-worktree-test-repo
    (let* ((bob/worktree-setup-functions
            (list (lambda (_path) (error "Setup failed"))))
           (result (bob/worktree-test-create root "setup-error" "main")))
      (should (eq (car result) 'error))
      (should (string-match-p "Worktree created at" (cdr result)))
      (should (string-match-p "Do not recreate" (cdr result)))
      (should (file-exists-p (expand-file-name ".worktrees/setup-error/.git" root))))))

(provide 'bob-worktrees-tests)
;;; bob-worktrees-tests.el ends here
