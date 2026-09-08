;;; task-workflow.el --- Start Bradwell tasks -*- lexical-binding: t; -*-

;;; Commentary:
;; Create or fetch a Linear issue, prepare its Bradwell worktree, and hand the
;; task to a new native Mentat session.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'bob-worktrees)
(require 'bradwell-utils)
(require 'linear-api)
(require 'session-manager)
(require 'mentat-elisp-library)
(declare-function mentat-linear--create-issue
                  "linear-api"
                  (api-key title description team-key project-name success error))

(defconst mentat-task-repository
  (expand-file-name "~/source/gist/bradwell-monorepo/")
  "Main Bradwell checkout used by the private task workflow.")

(defconst mentat-task-team-key "BRA"
  "Linear team key used by the private task workflow.")

(defun mentat-task--slug (title)
  "Return a compact branch slug derived from TITLE."
  (let* ((ignored '("a" "an" "and" "for" "in" "of" "on" "the" "to" "with"))
         (words (seq-take
                 (seq-remove
                  (lambda (word) (member word ignored))
                  (split-string
                   (string-trim
                    (replace-regexp-in-string
                     "[^[:alnum:]]+" " " (downcase title)))
                   " +" t))
                 3))
         (slug (mapconcat #'identity words "-")))
    (if (string-empty-p slug)
        "task"
      (truncate-string-to-width slug 24))))

(defun mentat-task--branch-name (issue)
  "Return the compact branch name for Linear ISSUE."
  (format "%s-%s" (alist-get 'identifier issue)
          (mentat-task--slug (alist-get 'title issue))))

(defun mentat-task--in-progress-state (issue)
  "Return ISSUE's In Progress state, or nil."
  (let ((states (alist-get 'nodes
                           (alist-get 'states (alist-get 'team issue)))))
    (or (seq-find (lambda (state)
                    (string-equal-ignore-case
                     (or (alist-get 'name state) "") "In Progress"))
                  states)
        (let ((started (seq-filter
                        (lambda (state)
                          (equal (alist-get 'type state) "started"))
                        states)))
          (and (= (length started) 1) (car started))))))

(defun mentat-task--handoff (issue worktree)
  "Return the waiting-session handoff for ISSUE and WORKTREE."
  (format
   (concat "Linear task %s: %s\n\n%s\n\n"
           "Issue: %s\nBranch: %s\nWorktree: %s\n\n"
           "Dependency setup is running asynchronously in: %s\n"
           "Read the repository instructions and this task context, but do not "
           "start implementation. Wait for further user instructions.")
   (alist-get 'identifier issue)
   (alist-get 'title issue)
   (or (alist-get 'description issue) "No description supplied.")
   (alist-get 'url issue)
   (alist-get 'branch worktree)
   (alist-get 'directory worktree)
   (if-let* ((buffers (alist-get 'setup-buffers worktree)))
       (string-join buffers ", ")
     "no output buffer")))

(defun mentat-task--result (issue worktree session)
  "Combine ISSUE, WORKTREE, and SESSION metadata."
  (let ((state (mentat-task--in-progress-state issue)))
    `((issue . ,issue)
      (branch . ,(alist-get 'branch worktree))
      (directory . ,(alist-get 'directory worktree))
      (setup-processes . ,(alist-get 'setup-processes worktree))
      (setup-buffers . ,(alist-get 'setup-buffers worktree))
      (session . ,session)
      (in-progress-state . ,state))))

(defun mentat-task--fetch-main (success failure)
  "Fetch Bradwell origin/main, calling SUCCESS or FAILURE.
Return a cancellation function."
  (let ((output "")
        process
        cancelled
        (default-directory mentat-task-repository))
    (setq process
          (make-process
           :name "mentat-task-fetch-main"
           :command '("git" "fetch" "origin" "main")
           :connection-type 'pipe
           :noquery t
           :filter (lambda (_process chunk)
                     (setq output
                           (truncate-string-to-width
                            (concat output chunk) 16000)))
           :sentinel
           (lambda (child _event)
             (when (and (not cancelled)
                        (memq (process-status child) '(exit signal)))
               (if (and (eq (process-status child) 'exit)
                        (zerop (process-exit-status child)))
                   (funcall success)
                 (funcall failure
                          (format "Git fetch failed (%s): %s"
                                  (process-exit-status child) output)))))))
    (lambda ()
      (setq cancelled t)
      (when (process-live-p process)
        (delete-process process)))))

(defun mentat-task--start (identifier title description project)
  "Return a callback starter for a confirmed Bradwell task.
Use existing Linear IDENTIFIER when non-nil.  Otherwise create an issue from
confirmed TITLE, DESCRIPTION, and PROJECT fields."
  (lambda (resolve reject on-cancel)
    (let (cancel-current issue worktree created-issue settled)
      (cl-labels
          ((set-cancel (cancel)
             (setq cancel-current cancel))
           (cancel ()
             (setq settled t)
             (when cancel-current (funcall cancel-current)))
           (fail (reason)
             (unless settled
               (setq settled t)
               (funcall
                reject
                (cond
                 (worktree
                  (format "Worktree created at %s, but task startup failed: %s. Do not recreate it."
                          (alist-get 'directory worktree) reason))
                 (created-issue
                  (format "Linear issue %s was created, but task startup failed: %s. Inspect it before retrying."
                          (alist-get 'identifier issue) reason))
                 (t reason)))))
           (start-callback (starter success)
             (setq cancel-current nil)
             (funcall starter success #'fail #'set-cancel))
           (session-ready (session)
             (unless settled
               (setq settled t)
               (funcall resolve (mentat-task--result issue worktree session))))
           (worktree-ready (value)
             (unless settled
               (setq worktree value)
               (start-callback
                (mentat-session-manager--starter
                 (alist-get 'directory worktree)
                 (format "%s %s" (alist-get 'identifier issue)
                         (alist-get 'title issue))
                 (mentat-task--handoff issue worktree))
                #'session-ready)))
           (fetched ()
             (unless settled
               (condition-case err
                   (set-cancel
                    (bob/create-worktree
                     mentat-task-repository
                     (mentat-task--branch-name issue)
                     "origin/main" #'worktree-ready #'fail))
                 (error (fail (error-message-string err))))))
           (issue-ready (value)
             (unless settled
               (setq issue (or (alist-get 'issue value) value)
                     created-issue (and (alist-get 'issue value) t))
               (set-cancel (mentat-task--fetch-main #'fetched #'fail)))))
        (funcall on-cancel #'cancel)
        (start-callback
         (mentat-linear--starter
          (lambda (api-key success failure)
            (if identifier
                (mentat-linear--get-issue
                 api-key identifier success failure)
              (mentat-linear--create-issue
               api-key title description mentat-task-team-key project
               success failure))))
         #'issue-ready)))))

(mentat--elisp-register-library
 'task-workflow 'user
 "Start a Bradwell Linear task: create or fetch its issue, prepare a non-nested worktree from origin/main, and create a waiting Mentat session."
 "task-workflow.md")

(mentat-defun mentat-task-start
    (&optional identifier title description project)
  "Start a confirmed Bradwell task and resolve after its Mentat handoff.

When IDENTIFIER is nonblank, fetch that existing Linear issue and ignore the
creation fields.  Otherwise TITLE, DESCRIPTION, and PROJECT must be the values
Bob already confirmed for a new BRA issue.  Fetch origin/main, derive a compact
branch from the issue identifier and title, create a non-nested worktree under
the main checkout, start Bradwell setup asynchronously, and create an
undisplayed Mentat session whose first prompt tells it to wait.

This operation does not move the issue to In Progress.  Ask Bob first, then use
the returned in-progress state with mentat-linear-set-state."
  (:execution async
   :arguments ((identifier "Existing Linear issue identifier, or omit to create one")
               (title "Confirmed title for a new issue")
               (description "Confirmed description for a new issue")
               (project "Confirmed Linear project name for a new issue")))
  (setq identifier (and (stringp identifier)
                        (not (string-blank-p identifier))
                        identifier))
  (unless (file-directory-p mentat-task-repository)
    (user-error "Bradwell checkout does not exist: %s" mentat-task-repository))
  (unless (or identifier
              (and (stringp title) (not (string-blank-p title))
                   (stringp description) (not (string-blank-p description))
                   (stringp project) (not (string-blank-p project))))
    (user-error
     "Provide an existing issue identifier or confirmed title, description, and project"))
  (mentat-task--start identifier title description project))

(mentat-defun mentat-worktree-clean (directory &optional remote-action)
  "Clean a Bradwell worktree and its associated local branch.
REMOTE-ACTION is `check', `delete', or `keep'.  CHECK first queries origin; if
the branch exists, return confirmation-required without changing anything.
After Bob confirms, call again with DELETE or KEEP.  The operation refuses the
primary checkout and dirty worktrees, and never forces removal."
  (:execution async
   :arguments ((directory "Bradwell worktree directory")
               (remote-action "Remote branch action: check, delete, or keep")))
  (unless (and (stringp directory) (file-directory-p directory)
               (file-in-directory-p
                (file-truename directory)
                (expand-file-name ".worktrees/" mentat-task-repository)))
    (user-error "Expected a Bradwell worktree under the main .worktrees directory"))
  (let ((policy (intern (or remote-action "check"))))
    (unless (memq policy '(check delete keep))
      (user-error "Remote action must be check, delete, or keep"))
    (lambda (resolve reject on-cancel)
      (let (settled)
        (funcall
         on-cancel
         (bob/clean-worktree
          directory policy
          (lambda (result)
            (unless settled
              (setq settled t)
              (funcall resolve result)))
          (lambda (reason)
            (unless settled
              (setq settled t)
              (funcall reject reason)))))))))

(provide 'task-workflow)
;;; task-workflow.el ends here
