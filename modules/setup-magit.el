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

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c s g" . bob/magit-buffers)
  :custom
  (magit-git-executable "/usr/local/bin/git")
  :init
  (use-package with-editor :ensure t)
  :config
  (setq magit-diff-refine-hunk 'all
        transient-default-level 7
        magit-commit-show-diff nil
        magit-revert-buffers 1
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (put 'magit-diff-edit-hunk-commit 'disabled nil)

  ;; This is for making Emacs OK with defining the following variable locally per Repo
  (put 'magit-status-sections-hook 'safe-local-variable #'listp)
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-message))

  (transient-append-suffix 'magit-file-dispatch
    "p"
    '("P" "Push" magit-push))
  (transient-append-suffix 'magit-file-dispatch
    "P"
    '("F" "Pull" magit-pull))
  ;; (setq magit-status-sections-hook
  ;;       '(magit-insert-unpushed-to-pushremote magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes))
)

(use-package forge
  :init (setq forge-bug-reference-hooks nil))

(use-package gh-notify
  :commands (gh-notify))

(use-package github-review
  :after forge
  :bind
  (:map magit-mode-map ("C-c g r" . github-review-forge-pr-at-point))
  (:map diff-mode-map ("C-c g s" . my/github-review-kill-suggestion))

  :config
  (setq github-review-fetch-top-level-and-review-comments t)
  (defun my/github-review-kill-suggestion ()
    ;; kill a region of diff+ as a review suggestion template
    (interactive)
    (setq deactivate-mark t)
    (let ((s-region
           (buffer-substring-no-properties
            (region-beginning)
            (region-end))))
      (kill-new
       (format "# ```suggestion\n%s\n# ```\n"
               (replace-regexp-in-string "^\\+" "# " s-region))))))

(use-package git-timemachine)
(use-package diff-hl
  :init (global-diff-hl-mode))

(provide 'setup-magit)
