(defun bob/magit-message (message)
    (interactive "sCommit message: ")
    (magit-commit-create `("-am" ,message)))

(defun fetch-all-git-repos-in-directory (repos-dir)
  (cl-loop for dir
           in (directory-files repos-dir)
           when (and (file-directory-p (format "%s/%s" repos-dir dir))
                     (member ".git" (directory-files (format "%s/%s" repos-dir dir))))
           do (run-fetch-in-dir (format "%s/%s" repos-dir dir))))

(defun run-fetch-in-dir (dir)
  (setq default-directory dir)
  (magit-fetch-all-prune))

(defun bob/refresh-vc-state-in-changed-buffers ()
  (let* ((repo-dir (magit-toplevel))
         (changed-files (magit-changed-files
                         (concat (magit-rev-parse "HEAD~1")
                                 "..HEAD"))))
    (dolist (file (mapcar (lambda (f) (concat repo-dir f)) changed-files))
      (let ((buffer (get-file-buffer file)))
        (when buffer
          (with-current-buffer buffer
            (vc-refresh-state)
            (diff-hl-update)))))))

(use-package magit
  :demand t
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c s g" . bob/magit-buffers)
  :init
  (use-package with-editor :ensure t)
  (add-hook 'git-commit-post-finish-hook 'bob/refresh-vc-state-in-changed-buffers)
  (add-hook 'magit-post-stage-hook 'bob/refresh-vc-state-in-changed-buffers)
  (add-hook 'magit-post-unstage-hook 'bob/refresh-vc-state-in-changed-buffers)
  (add-hook 'magit-refresh-buffer-hook 'bob/refresh-vc-state-in-changed-buffers)
  :config
  (setq magit-diff-refine-hunk 'all
        transient-default-level 7
        magit-commit-show-diff nil
        magit-revert-buffers 1
        magit-display-buffer-function #'magit-display-buffer-traditional
        ;; #'magit-display-buffer-fullframe-status-v1
        )
  (magit-toggle-verbose-refresh)

  (put 'magit-diff-edit-hunk-commit 'disabled nil)

  ;; This is for making Emacs OK with defining the following variable locally per Repo
  (put 'magit-status-sections-hook 'safe-local-variable #'listp)
  (put 'magit-display-buffer-function 'safe-local-variable #'functionp)
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-message))

  (transient-append-suffix 'magit-file-dispatch
    "p"
    '("P" "Push" magit-push))
  (transient-append-suffix 'magit-file-dispatch
    "P"
    '("F" "Pull" magit-pull)))

(use-package forge
  :after magit
  :init (setq forge-bug-reference-hooks nil))

(use-package github-review
  :after magit)

(use-package diff-hl
  :demand t
  :config (global-diff-hl-mode))

(use-package denote)
(use-package vc
  :ensure nil
  :bind ("C-x v p" . vc-pull))

(use-package git-timemachine)

(defun create-github-repo ()
  "Create a new Github repo using the Github API."
  (interactive)
  (let ((repo-name (read-string "Repo name: "))
        (repo-description (read-string "Repo description: "))
        (repo-homepage (read-string "Repo homepage: "))
        (is-repo-private (yes-or-no-p "Is Repo private: "))
        (repo-is_template (yes-or-no-p "Is Repo is_template: ")))
    (ghub-post "/user/repos" (list  :name repo-name
                                    :description repo-description
                                    :homepage repo-homepage
                                    :private is-repo-private
                                    :is_template repo-is_template))
    (magit-remote-add "origin" (format "git@github.com:bobrowadam/%s.git" repo-name))
    (magit-push-current-to-pushremote `("-u" "origin" ,(magit-get-current-branch)))))

(use-package gh)

(provide 'setup-magit)

