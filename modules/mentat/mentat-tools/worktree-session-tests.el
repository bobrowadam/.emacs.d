;;; worktree-session-tests.el --- Worktree session lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bob-worktrees-tests)
(require 'worktree-session)

(defmacro bob/with-worktree-session-test (&rest body)
  "Run BODY with an isolated Git worktree and registered idle conversation."
  (declare (indent 0) (debug t))
  `(bob/with-worktree-test-repo
     (let* ((remote (expand-file-name "remote.git" root))
            (mentat-registry-file (expand-file-name "sessions.eld" root))
            (mentat-collection-directory (expand-file-name "collections/" root))
            (mentat--collection-buffers (make-hash-table :test #'equal))
            (history (expand-file-name "history.jsonl" root))
            (buffer (generate-new-buffer " *worktree-session-test*"))
            path view warnings cancel stopped)
       (unwind-protect
           (progn
             (bob/worktree-test-git "init" "--bare" remote)
             (bob/worktree-test-git "remote" "add" "origin" remote)
             (bob/worktree-test-git "push" "origin" "main")
             (bob/worktree-test-git "remote" "set-head" "origin" "main")
             (setq path (alist-get 'directory
                                  (cdr (bob/worktree-test-create root "task" "main"))))
             (with-temp-file history (insert "{\"type\":\"session\",\"id\":\"test-session\"}\n"))
             (mentat--registry-register
              path `((sessionId . "test-session") (sessionFile . ,history)))
             (setq view
                   (mentat--buffer-make
                    :buffer buffer :status "idle"
                    :tool-nodes (make-hash-table :test #'equal)
                    :session (mentat--session-make
                              :root path :session-id "test-session"
                              :session-file history :ready t)))
             (with-current-buffer buffer
               (setq major-mode 'mentat-buffer-mode)
               (setq-local mentat--buffer-root path)
               (setq-local mentat--buffer-view view)
               (add-hook 'kill-buffer-hook (lambda () (setq stopped t)) nil t))
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'display-warning)
                        (lambda (_type text &rest _) (push text warnings)))
                       ((symbol-function 'mentat--buffer-set-status)
                        (lambda (target status) (setf (mentat--buffer-status target) status))))
               ,@body))
         (when cancel (funcall cancel))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun bob/worktree-session-test-start ()
  "Start cleanup in the current fixture conversation."
  (bob/mentat-kill-session-and-worktree))

(defun bob/worktree-session-test-await (finished)
  "Wait up to ten seconds for FINISHED in the batch test process."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall finished)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall finished))))

(ert-deftest bob/worktree-session-removes-worktree-and-session-but-keeps-history ()
  (bob/with-worktree-session-test
    (setq cancel (with-current-buffer buffer (bob/worktree-session-test-start)))
    (bob/worktree-session-test-await (lambda () (or stopped warnings)))
    (should-not warnings)
    (should stopped)
    (should-not (file-exists-p path))
    (should-not (buffer-live-p buffer))
    (should-not (mentat--registry-list path))
    (should (file-exists-p history))
    (should-not (member "task" (split-string (bob/worktree-test-git "branch" "--format=%(refname:short)") "\n" t)))))

(ert-deftest bob/worktree-session-dirty-worktree-keeps-session ()
  (bob/with-worktree-session-test
    (with-temp-file (expand-file-name "uncommitted" path) (insert "keep me"))
    (setq cancel (with-current-buffer buffer (bob/worktree-session-test-start)))
    (bob/worktree-session-test-await (lambda () warnings))
    (should (file-exists-p (expand-file-name "uncommitted" path)))
    (should (buffer-live-p buffer))
    (should-not stopped)
    (should (equal "idle" (mentat--buffer-status view)))
    (should (mentat--registry-list path))))

(ert-deftest bob/worktree-session-primary-checkout-is-preserved ()
  (bob/with-worktree-session-test
    (let ((outside-history (make-temp-file "worktree-session-history-")))
      (unwind-protect
          (progn
            (mentat--registry-register
             root `((sessionId . "test-session") (sessionFile . ,outside-history)))
            (setf (mentat--session-session-file (mentat--buffer-session view))
                  outside-history)
            (with-current-buffer buffer (setq mentat--buffer-root root))
            (setq cancel (with-current-buffer buffer (bob/worktree-session-test-start)))
            (bob/worktree-session-test-await (lambda () warnings))
            (should (string-match-p "primary worktree" (car warnings)))
            (should (file-directory-p root))
            (should (buffer-live-p buffer))
            (should-not stopped))
        (delete-file outside-history)))))

(ert-deftest bob/worktree-session-declining-preserves-everything ()
  (bob/with-worktree-session-test
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-not (with-current-buffer buffer (bob/worktree-session-test-start))))
    (should (file-directory-p path))
    (should (mentat--registry-list path))
    (should-not stopped)))

(ert-deftest bob/worktree-session-rechecks-busy-state-before-removal ()
  (bob/with-worktree-session-test
    (setq cancel (with-current-buffer buffer (bob/worktree-session-test-start)))
    (setf (mentat--buffer-status view) "running")
    (bob/worktree-session-test-await (lambda () warnings))
    (should (file-directory-p path))
    (should (buffer-live-p buffer))
    (should (equal "running" (mentat--buffer-status view)))
    (should-not stopped)))

(ert-deftest bob/worktree-session-rechecks-sharing-before-removal ()
  (bob/with-worktree-session-test
    (setq cancel (with-current-buffer buffer (bob/worktree-session-test-start)))
    (let ((other-history (expand-file-name "other.jsonl" root)))
      (with-temp-file other-history (insert "{}"))
      (mentat--registry-register
       path `((sessionId . "other-session") (sessionFile . ,other-history))))
    (bob/worktree-session-test-await (lambda () warnings))
    (should (file-directory-p path))
    (should (= (length (mentat--registry-list path)) 2))
    (should-not stopped)))

(ert-deftest bob/worktree-session-unsaved-buffer-prevents-cleanup ()
  (bob/with-worktree-session-test
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "unsaved" path))
      (insert "do not discard")
      (should-error (with-current-buffer buffer (bob/worktree-session-test-start))
                    :type 'user-error))
    (should (file-directory-p path))
    (should-not stopped)))

(ert-deftest bob/worktree-session-origin-deletion-needs-separate-confirmation ()
  (dolist (delete-remote '(nil t))
    (bob/with-worktree-session-test
      (bob/worktree-test-git "push" "origin" "task")
      (let ((answers (list t delete-remote))
            (prompts 0))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (cl-incf prompts)
                     (pop answers))))
          (setq cancel (with-current-buffer buffer (bob/worktree-session-test-start)))
          (bob/worktree-session-test-await (lambda () (or stopped warnings)))
          (should (= prompts 2))))
      (should-not warnings)
      (should stopped)
      (with-temp-buffer
        (should (= (process-file "git" nil t nil "ls-remote" "--exit-code" "--heads"
                                 "origin" "refs/heads/task")
                   (if delete-remote 2 0)))))))

(provide 'worktree-session-tests)
;;; worktree-session-tests.el ends here
