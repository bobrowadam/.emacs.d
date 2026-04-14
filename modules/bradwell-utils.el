;;; bradwell-utils.el --- Bradwell worktree helpers -*- lexical-binding: t; -*-

(defconst bob/bradwell-main-worktree-directory
  (expand-file-name "~/source/bradwell-monorepo")
  "Path to the main Bradwell checkout.")

(defun bob/is-bradwell-project (path)
  "Check if PATH is a Bradwell project."
  (let ((default-directory path))
    (equal (bob/npm--project-name) "bradwell-monorepo")))

(defun bob/bradwell--ensure-shared-worktree-file (path file-name)
  "Ensure FILE-NAME in PATH points to the shared Bradwell root file."
  (let ((target (expand-file-name file-name bob/bradwell-main-worktree-directory))
        (link-path (expand-file-name file-name path)))
    (cond
     ((not (file-exists-p target))
      (message "Bradwell worktree setup skipped missing %s" target))
     ((file-symlink-p link-path)
      (unless (file-equal-p link-path target)
        (message "Bradwell worktree setup found unexpected symlink %s" link-path)))
     ((file-exists-p link-path)
      (message "Bradwell worktree setup left existing %s in place" link-path))
     (t
      (make-symbolic-link target link-path)
      (message "Linked %s -> %s" link-path target)))))

(defun bob/setup-bradwell-project-for-worktree (path)
  "Setup Bradwell worktree in PATH."
  (bob/bradwell--ensure-shared-worktree-file path ".pi")
  (bob/bradwell--ensure-shared-worktree-file path ".venv")
  (bob/bradwell--ensure-shared-worktree-file path "pyrightconfig.json")
  (let ((default-directory (file-name-as-directory path)))
    (async-shell-command
     "npm install && npm run build:services-common"
     (format "*bradwell-worktree-setup<%s>*" (file-name-nondirectory (directory-file-name path))))))

(defun bob/setup-bradwell-worktree-checkout (orig-fun path worktree)
  "Setup Bradwell worktree after checking out WORKTREE to PATH."
  (funcall orig-fun path worktree)
  (when (bob/is-bradwell-project path)
    (bob/setup-bradwell-project-for-worktree path)))

(defun bob/setup-bradwell-worktree-branch (orig-fun path worktree &optional starting-point)
  "Setup Bradwell worktree after creating WORKTREE at PATH."
  (if starting-point
      (funcall orig-fun path worktree starting-point)
    (funcall orig-fun path worktree))
  (when (bob/is-bradwell-project path)
    (bob/setup-bradwell-project-for-worktree path)))

(advice-add 'magit-worktree-checkout :around #'bob/setup-bradwell-worktree-checkout)
(advice-add 'magit-worktree-branch :around #'bob/setup-bradwell-worktree-branch)
(advice-add 'magit-worktree-move :around
            (lambda (orig-fun worktree path)
              (funcall orig-fun worktree path)
              (when (bob/is-bradwell-project path)
                (bob/setup-bradwell-project-for-worktree path))))

(provide 'bradwell-utils)

;;; bradwell-utils.el ends here
