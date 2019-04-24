(use-package projectile
  :demand t
  :config (projectile-global-mode 1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :config
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'setup-projectile)
