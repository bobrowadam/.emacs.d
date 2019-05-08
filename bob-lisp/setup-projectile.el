(use-package projectile
  :demand t
  :init
  (defun my-run-eshell (&optional arg)
    "Create an interactive Eshell buffer.
 if in project use `projectile-run-eshell"
    (interactive "P")
    (if (projectile-project-p)
        (projectile-run-eshell)
      (eshell arg)))
  :config (projectile-global-mode 1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :config
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'setup-projectile)
