(use-package projectile
  :demand
  :init
  (defun my-run-eshell (&optional arg)
    "Create an interactive Eshell buffer.
 if in project use `projectile-run-eshell"
    (interactive "P")
    (if (projectile-project-p)
        (projectile-run-eshell arg)
      (eshell arg)))
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-global-mode 1)
  :bind
  (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(provide 'setup-projectile)
