(use-package magit
  :init (setq with-editor-emacsclient-executable nil)
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
  :hook
  (before-save-hook . magit-wip-commit-initial-backup)
  :bind  ("C-c s g" . bob/magit-buffers)
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq transient-default-level 7)
  (setq magit-commit-show-diff nil
      magit-revert-buffers 1)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-message))

  (transient-append-suffix 'magit-file-dispatch
    "p"
    '("P" "Push" magit-push))
  (transient-append-suffix 'magit-file-dispatch
    "P"
    '("F" "Pull" magit-pull))
  (magit-wip-before-change-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (setq magit-wip-merge-branch t))

(use-package forge
  :init (setq forge-bug-reference-hooks nil))

(use-package github-review
  :init (setq github-review-fetch-top-level-and-review-comments t))

(use-package git-timemachine)
(use-package diff-hl
  :init (global-diff-hl-mode))

(provide 'setup-magit)
