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
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-global-mode 1))

(provide 'setup-projectile)
