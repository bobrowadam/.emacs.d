(defun bob/magit-message (message)
  (interactive "sCommit message: ")
  (magit-commit-create `("-m" ,message)))

(use-package magit
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  (setq transient-default-level 7)
  (global-magit-file-mode 1)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-message)))

(use-package forge)

(use-package github-review
  :init (setq github-review-fetch-top-level-and-review-comments t))

(provide 'setup-magit)
