;;; bob-worktrees.el --- Private worktree creation -*- lexical-binding: t; -*-

;;; Commentary:
;; Noninteractive worktree creation and shared post-creation setup.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(defvar bob/worktree-setup-functions nil
  "Functions called with a newly created worktree directory.
Functions may return a background process; creation does not wait for setup.")

(defun bob/update-graphify-worktree (directory)
  "Asynchronously update an existing Graphify graph for DIRECTORY."
  (when-let* ((program (executable-find "graphify-worktree-update")))
    (let ((process-connection-type nil))
      (start-process "graphify-worktree-update" nil
                     program (expand-file-name directory)))))

(add-hook 'bob/worktree-setup-functions #'bob/update-graphify-worktree)

(defun bob/prepare-worktree (directory)
  "Remember DIRECTORY and return processes started by its setup functions."
  (when-let* ((project (project-current nil directory)))
    (project-remember-project project))
  (seq-filter #'processp
              (mapcar (lambda (setup) (funcall setup directory))
                      bob/worktree-setup-functions)))

(defun bob/create-worktree (directory branch start-point success failure)
  "Create BRANCH from START-POINT in DIRECTORY's main repository.
Place it under the main checkout's .worktrees directory, replacing slashes
in BRANCH with dashes for the directory name only.  DIRECTORY may be a
linked worktree or a subdirectory.  Never visit a buffer or overwrite a path.
Call SUCCESS with an alist containing directory, branch, and setup processes.
Call FAILURE with error text.  Return a cancellation function.  Cancellation
stops Git but does not undo a branch or worktree already created."
  (unless (and (stringp directory) (file-directory-p directory)
               (not (file-remote-p directory)))
    (user-error "Worktree source must be an existing local directory"))
  (dolist (value (list branch start-point))
    (unless (and (stringp value) (not (string-blank-p value))
                 (not (string-prefix-p "-" value)))
      (user-error "Branch and starting revision must be explicit names")))
  (let ((source (file-name-as-directory (expand-file-name directory)))
        process cancelled target)
    (cl-labels
        ((fail (text)
           (unless cancelled (funcall failure text)))
         (run (args next)
           (let ((output "")
                 (default-directory source)
                 (process-environment (cons "LEFTHOOK=0" process-environment)))
             (setq process
                   (make-process
                    :name "worktree-create" :connection-type 'pipe :noquery t
                    :command (cons "git" args)
                    :filter (lambda (_child chunk)
                              (setq output (truncate-string-to-width
                                            (concat output chunk) 16000)))
                    :sentinel
                    (lambda (child _event)
                      (when (and (not cancelled)
                                 (memq (process-status child) '(exit signal)))
                        (if (and (eq (process-status child) 'exit)
                                 (zerop (process-exit-status child)))
                            (condition-case err
                                (funcall next output)
                              (error (fail (error-message-string err))))
                          (fail (format "Git failed (%s): %s"
                                        (process-exit-status child) output)))))))))
         (created (_output)
           (condition-case err
               (let ((setup (bob/prepare-worktree target)))
                 (funcall success
                          `((directory . ,target) (branch . ,branch)
                            (setup-processes . ,(mapcar #'process-name setup))
                            (setup-buffers . ,(delq nil
                              (mapcar (lambda (child)
                                        (when-let* ((buffer (process-buffer child)))
                                          (buffer-name buffer))) setup))))))
             (error (fail (format "Worktree created at %s, but setup failed: %s. Do not recreate it."
                                  target (error-message-string err))))))
         (listed (output)
           (let* ((fields (split-string output "\0"))
                  (first (car fields)))
             (unless (and (string-prefix-p "worktree " first)
                          (not (member "bare" (seq-take-while
                                                (lambda (s) (not (string-empty-p s)))
                                                fields))))
               (error "Worktree creation requires a non-bare main checkout"))
             (setq target (expand-file-name
                           (concat ".worktrees/" (string-replace "/" "-" branch))
                           (substring first (length "worktree "))))
             (when (or (file-exists-p target) (file-symlink-p target))
               (error "Worktree destination already exists: %s" target))
             (run (list "worktree" "add" "-b" branch "--" target start-point)
                  #'created))))
      (run (list "check-ref-format" "--branch" branch)
           (lambda (_output)
             (run '("worktree" "list" "--porcelain" "-z") #'listed)))
      (lambda ()
        (setq cancelled t)
        (when (process-live-p process) (delete-process process))))))

(defun bob/clean-worktree (directory remote-policy success failure
                                     &optional remove-function)
  "Remove worktree DIRECTORY and its associated branches.
REMOTE-POLICY is `check', `delete', `keep', or a function called with the
branch name when that branch exists on origin.  `check' calls SUCCESS with a
confirmation-required result without changing anything when the remote branch
exists.  DELETE removes it; KEEP preserves it.  A policy function should return
non-nil to delete it.

Use REMOVE-FUNCTION, when non-nil, instead of `git worktree remove'.  This lets
interactive callers retain their own confirmation behavior.  Call SUCCESS with
cleanup metadata or FAILURE with error text.  Return a cancellation function."
  (unless (and (stringp directory) (file-directory-p directory)
               (not (file-remote-p directory)))
    (user-error "Worktree must be an existing local directory"))
  (unless (or (memq remote-policy '(check delete keep))
              (functionp remote-policy))
    (user-error "Remote policy must be check, delete, keep, or a function"))
  (let ((target (file-name-as-directory (file-truename directory)))
        source branch process settled remote-exists delete-remote)
    (cl-labels
        ((fail (text)
           (unless settled
             (setq settled t)
             (funcall failure text)))
         (succeed (value)
           (unless settled
             (setq settled t)
             (funcall success value)))
         (run (working-directory args accepted next)
           (let ((output "")
                 (default-directory working-directory))
             (setq process
                   (make-process
                    :name "worktree-clean" :connection-type 'pipe :noquery t
                    :command (cons "git" args)
                    :filter (lambda (_child chunk)
                              (setq output (truncate-string-to-width
                                            (concat output chunk) 16000)))
                    :sentinel
                    (lambda (child _event)
                      (when (and (not settled)
                                 (memq (process-status child) '(exit signal)))
                        (let ((status (process-exit-status child)))
                          (if (memq status accepted)
                              (condition-case err
                                  (funcall next output status)
                                (error (fail (error-message-string err))))
                            (fail (format "Git failed (%s): %s" status output))))))))))
         (finish ()
           (succeed `((directory . ,target) (branch . ,branch)
                      (remote-branch-existed . ,(and remote-exists t))
                      (remote-branch-deleted . ,(and delete-remote t)))))
         (delete-remote-branch (_output _status)
           (finish))
         (delete-local-branch (_output _status)
           (if delete-remote
               (run source
                    (list "push" "origin" "--delete"
                          (concat "refs/heads/" branch))
                    '(0) #'delete-remote-branch)
             (finish)))
         (removed ()
           (project-forget-projects-under target t)
           (if branch
               (run source (list "branch" "-D" "--" branch)
                    '(0) #'delete-local-branch)
             (finish)))
         (remove-worktree ()
           (if remove-function
               (progn (funcall remove-function target) (removed))
             (run source (list "worktree" "remove" "--" target)
                  '(0) (lambda (_output _status) (removed)))))
         (apply-remote-policy ()
           (cond
            ((not remote-exists) (remove-worktree))
            ((eq remote-policy 'check)
             (succeed `((directory . ,target) (branch . ,branch)
                        (remote-branch-existed . t)
                        (confirmation-required . t))))
            (t
             (setq delete-remote
                   (if (functionp remote-policy)
                       (funcall remote-policy branch)
                     (eq remote-policy 'delete)))
             (remove-worktree))))
         (remote-checked (_output status)
           (setq remote-exists (zerop status))
           (apply-remote-policy))
         (origin-checked (_output status)
           (if (zerop status)
               (run source
                    (list "ls-remote" "--exit-code" "--heads" "origin"
                          (concat "refs/heads/" branch))
                    '(0 2) #'remote-checked)
             (setq remote-exists nil)
             (apply-remote-policy)))
         (listed (output _status)
           (let* ((records (split-string output "\0\0" t))
                  (main-fields (split-string (car records) "\0" t))
                  (main-field (car main-fields))
                  (record
                   (seq-find
                    (lambda (entry)
                      (let* ((fields (split-string entry "\0" t))
                             (field (car fields)))
                        (and (string-prefix-p "worktree " field)
                             (file-equal-p
                              target
                              (substring field (length "worktree "))))))
                    records)))
             (unless (and (string-prefix-p "worktree " main-field) record)
               (error "Directory is not a registered Git worktree: %s" target))
             (setq source
                   (file-name-as-directory
                    (substring main-field (length "worktree "))))
             (when (file-equal-p target source)
               (error "Cannot clean the primary worktree: %s" target))
             (setq branch
                   (when-let* ((field
                                (seq-find
                                 (lambda (value)
                                   (string-prefix-p "branch refs/heads/" value))
                                 (split-string record "\0" t))))
                     (substring field (length "branch refs/heads/"))))
             (if branch
                 (run source '("config" "--get" "remote.origin.url")
                      '(0 1) #'origin-checked)
               (apply-remote-policy)))))
      (run target '("worktree" "list" "--porcelain" "-z") '(0) #'listed)
      (lambda ()
        (setq settled t)
        (when (process-live-p process) (delete-process process))))))

(provide 'bob-worktrees)
;;; bob-worktrees.el ends here
