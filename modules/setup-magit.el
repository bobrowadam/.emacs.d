(defun bob/magit-commit-message (message)
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

(defun bob/magit-buffers ()
  "Jump to a magit buffer."
  (interactive)
  (if-let* ((magit-buffers
             (bob/drop-buffer
              (set-last-magit-buffer-as-first
               (seq-filter
                (lambda (b) (or (equal (with-current-buffer b major-mode) 'magit-status-mode)))
                (mapcar (function buffer-name) (buffer-list))))))
            (magit-buffer (completing-read "Magit: " magit-buffers)))
      (progn
        (setq bob/last-magit-buffer magit-buffer)
        (switch-to-buffer magit-buffer))
    (message "No Magit buffers exists")))

(use-package magit
  :commands (magit-status bob/magit-buffers)
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
        magit-refresh-status-buffer nil
        ;; #'magit-display-buffer-fullframe-status-v1
        )
  (magit-toggle-verbose-refresh)

  (put 'magit-diff-edit-hunk-commit 'disabled nil)

  ;; This is for making Emacs OK with defining the following variable locally per Repo
  (put 'magit-status-sections-hook 'safe-local-variable #'listp)
  (put 'magit-display-buffer-function 'safe-local-variable #'functionp)
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-commit-message))

  (transient-append-suffix 'magit-file-dispatch
    "p"
    '("P" "Push" magit-push))
  (transient-append-suffix 'magit-file-dispatch
    "P"
    '("F" "Pull" magit-pull))
  :hook (magit-post-refresh . bob/update-node-modules-if-needed))

(use-package forge
  :after magit
  :custom
  (forge-topic-list-limit '(5 . -1))
  :init (setq forge-bug-reference-hooks nil))

(use-package github-review
  :after magit)

(use-package diff-hl
  :commands (diff-hl-update)
  :after magit
  :config (global-diff-hl-mode))

(use-package vc
  :ensure nil
  :bind
  ("C-x v p" . vc-pull)
  ("C-x v d" . vc-dir-root))

(use-package git-timemachine)

(defun bob/create-github-repo ()
  "Create a new Github repo using the Github API."
  (interactive)
  (let ((repo-name (read-string "Repo name: " (get-dir-name (project-root (project-current)))))
        (repo-description (read-string "Repo description: "))
        (repo-homepage (read-string "Repo homepage: "))
        (is-repo-private (yes-or-no-p "Is Repo private: "))
        (repo-is_template (yes-or-no-p "Is Repo is_template: "))
        (current-branch (magit-get-current-branch)))
    (ghub-post "/user/repos" (list  :name repo-name
                                    :description repo-description
                                    :homepage repo-homepage
                                    :private is-repo-private
                                    :is_template repo-is_template))
    (magit-remote-add "origin" (format "git@github.com:bobrowadam/%s.git" repo-name))
    (magit-run-git-async "push"
                         "-u"
                         "origin"
                         (format "refs/heads/%s:refs/heads/%s"
                                 current-branch
                                 current-branch))))

(defun bob/magit-list-gone-branches ()
  ;; Get a list of local branches where the remote tracking branch is gone.
  (let ((branches (magit-git-lines "branch" "-vv")))
    (delq nil
          (mapcar (lambda (branch)
                    (when (string-match "\\[origin/.+? gone\\]" branch)
                      (car (split-string branch))))
                  branches))))

(defun bobs/magit-delete-gone-branches ()
  (interactive)
  (magit-git-fetch nil '("--prune"))
  (let ((gone-branches (bob/magit-list-gone-branches)))
    (dolist (branch gone-branches)
      (message "Deleting branch %s" branch)
      (magit-call-git "branch" "-D" branch))))

(defun bob/delete-merged-local-branches ()
  (interactive)
  (magit-fetch-all-prune)

  (let* ((local-branches (magit-list-local-branch-names))
         (current-branch (magit-get-current-branch))
         (filtered-local-branches (seq-remove (lambda (branch)
                                                (or (equal branch current-branch)
                                                    (equal branch "master")
                                                    (magit-get-upstream-branch branch)))
                                              local-branches)))
    (dolist (branch filtered-local-branches)
      (magit-run-git "branch" "-D" branch)))
  (message "Clean-up of merged local branches complete."))

(use-package gh)

(provide 'setup-magit)
