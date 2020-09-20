(use-package typescript-mode
  :hook
  (typescript-mode . yas-minor-mode)
  (typescript-mode . origami-mode) 
  (typescript-mode . setup-tide-mode)
  :config
  (setq typescript-indent-level 2))

(use-package ts-comint :disabled t)

(provide 'setup-ts)
