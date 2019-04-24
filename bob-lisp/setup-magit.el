(use-package magit
  :bind ("C-x g" . magit-status)
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  (setq transient-default-level 7)
  (global-magit-file-mode 1)
  (put 'magit-diff-edit-hunk-commit 'disabled nil))

(use-package forge)

(use-package github-review
  :init (setq github-review-fetch-top-level-and-review-comments t))

(provide 'setup-magit)
