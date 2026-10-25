;;; worktree-session.el --- Close a Mentat worktree session -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive cleanup using the private Git worktree lifecycle.

;;; Code:

(require 'cl-lib)
(require 'bob-worktrees)
(require 'mentat)
(require 'mentat-directory)
(require 'mentat-collection)
(require 'seq)

(defun bob/mentat--check-worktree-session (view directory)
  "Refuse cleanup unless VIEW alone uses DIRECTORY and has no pending work."
  (unless (buffer-live-p (mentat--buffer-buffer view))
    (user-error "Mentat conversation has already closed"))
  (unless (and (equal "idle" (mentat--buffer-status view))
               (mentat--directory-idle-p view)
               (zerop (mentat--buffer-background-agent-count view)))
    (user-error "Mentat conversation must be idle with no queued or background work"))
  (let* ((session (mentat--buffer-session view))
         (session-id (mentat--session-session-id session))
         (entries (seq-filter
                   (lambda (entry)
                     (let ((root (mentat--registry-entry-root entry)))
                       (or (file-equal-p root directory)
                           (file-in-directory-p root directory))))
                   (mentat--registry-list))))
    (unless (and (= (length entries) 1)
                 (equal session-id
                        (mentat--registry-entry-session-id (car entries))))
      (user-error "Worktree must belong only to this registered Mentat session"))
    (when (mentat--collection-pending-p session-id)
      (user-error "Wait for the saved prompt collection to finish sending"))
    (when (file-in-directory-p (mentat--session-session-file session) directory)
      (user-error "Cannot remove a worktree containing its Pi transcript")))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name (buffer-modified-p)
                 (file-in-directory-p buffer-file-name directory))
        (user-error "Worktree has an unsaved buffer: %s" (buffer-name))))))

;;;###autoload
(defun bob/mentat-kill-session-and-worktree ()
  "Remove the current session's clean linked worktree, then close the session.
Preserve the Pi transcript.  Confirm removal of the worktree and local branch,
and ask separately before deleting a branch that still exists on origin.
Refuse busy sessions, other sessions sharing the worktree, unsaved buffers,
and the primary checkout.  Git cleanup failures leave the conversation open
unless the worktree has already been removed."
  (interactive)
  (mentat--with-live-conversation
    (let* ((view mentat--buffer-view)
           (buffer (mentat--buffer-buffer view))
           (directory (file-name-as-directory (file-truename mentat--buffer-root)))
           (session-id (mentat--session-session-id (mentat--buffer-session view))))
      (bob/mentat--check-worktree-session view directory)
      (when (yes-or-no-p
             (format "Remove worktree %s and its local branch, and close this session? Saved prompt collection items (%d) will be deleted; Pi history will remain. "
                     directory (mentat--collection-count session-id)))
        (cl-labels
            ((close-session ()
               (condition-case err
                   (progn
                     (when (buffer-live-p buffer)
                       (unless (kill-buffer buffer)
                         (error "Worktree removed, but the session buffer refused to close")))
                     (mentat--registry-forget session-id)
                     (mentat--collection-forget session-id)
                     (message "Removed worktree %s and closed its session; Pi history preserved"
                              directory))
                 (error (display-warning 'bob-mentat (error-message-string err) :error))))
             (failed (reason)
               (if (file-directory-p directory)
                   (when (and (buffer-live-p buffer)
                              (equal "cleaning worktree" (mentat--buffer-status view)))
                     (mentat--buffer-set-status view "idle"))
                 (close-session))
               (display-warning 'bob-mentat (format "Worktree cleanup failed: %s" reason)
                                :error)))
          (bob/clean-worktree
           directory
           (lambda (branch)
             (yes-or-no-p (format "Also delete remote branch origin/%s? " branch)))
           (lambda (_result) (close-session))
           #'failed nil
           (lambda (target)
             (bob/mentat--check-worktree-session view target)
             (mentat--buffer-set-status view "cleaning worktree"))))))))

(provide 'worktree-session)
;;; worktree-session.el ends here
