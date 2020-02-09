(defun bob/magit-message (message)
  (interactive "sCommit message: ")
  (magit-commit-create `("-m" ,message)))

(use-package magit
  :init (setq with-editor-emacsclient-executable "/Applications/emacs-plus/26.3/bin/emacsclient")
  :hook
  (before-save-hook . magit-wip-commit-initial-backup)
  :config
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  (setq transient-default-level 7)
  (global-magit-file-mode 1)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (transient-append-suffix 'magit-commit
    "c"
    '("m" "Quick commit using minibuffer for commit message." bob/magit-message))
  (magit-wip-before-change-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (setq magit-wip-merge-branch t))

(use-package forge)

(use-package github-review
  :init (setq github-review-fetch-top-level-and-review-comments t))

(use-package diff-hl
  :demand t
  :init (global-diff-hl-mode))

(provide 'setup-magit)
