(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-git-executable "/usr/local/bin/git")
  :init
  (use-package with-editor :ensure t)
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
  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  ;; (defadvice magit-status (around magit-fullscreen activate)
  ;;   (window-configuration-to-register :magit-fullscreen)
  ;;   ad-do-it
  ;;   (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
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
  :config
  (setq magit-diff-refine-hunk 'all
        transient-default-level 7
        magit-commit-show-diff nil
        magit-revert-buffers 1
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-wip-before-change-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (setq magit-wip-merge-branch t)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

;; (use-package magit
;;   :init (setq with-editor-emacsclient-executable nil)
;;   (defun bob/magit-message (message)
;;     (interactive "sCommit message: ")
;;     (magit-commit-create `("-am" ,message)))

;;   (defun fetch-all-git-repos-in-directory (repos-dir)
;;     (cl-loop for dir
;;              in (directory-files repos-dir)
;;              when (and (file-directory-p (format "%s/%s" repos-dir dir))
;;                        (member ".git" (directory-files (format "%s/%s" repos-dir dir))))
;;              do (run-fetch-in-dir (format "%s/%s" repos-dir dir))))

;;   (defun run-fetch-in-dir (dir)
;;     (setq default-directory dir)
;;     (magit-fetch-all-prune))
;;   :hook
;;   (before-save-hook . magit-wip-commit-initial-backup)
;;   :bind  ("C-c s g" . bob/magit-buffers)
;;   :config
;;   ;; (magit-toggle-verbose-refresh)
;;   (setq magit-diff-refine-hunk 'all
;;         transient-default-level 7
;;         magit-commit-show-diff nil
;;         magit-revert-buffers 1
;;         magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
;;   (put 'magit-diff-edit-hunk-commit 'disabled nil)
;;   (transient-append-suffix 'magit-commit
;;     "c"
;;     '("m" "Quick commit using minibuffer for commit message." bob/magit-message))

;;   (transient-append-suffix 'magit-file-dispatch
;;     "p"
;;     '("P" "Push" magit-push))
;;   (transient-append-suffix 'magit-file-dispatch
;;     "P"
;;     '("F" "Pull" magit-pull))
;;   (magit-wip-before-change-mode)
;;   (magit-wip-after-apply-mode)
;;   (magit-wip-after-save-mode)
;;   (setq magit-wip-merge-branch t))

(use-package forge
  :init (setq forge-bug-reference-hooks nil))

(use-package github-review
  :init (setq github-review-fetch-top-level-and-review-comments t))

(use-package git-timemachine)
(use-package diff-hl
  :init (global-diff-hl-mode))

(provide 'setup-magit)
