(use-package inf-mongo
  :after startup
  :after org)

(use-package shell-defuns
  :after startup
  :if (window-system)
  :load-path "./site-lisp"
  :demand t)

(use-package vterm
  :after startup
  :if (window-system)
  :after shell-defuns
  :config
  (setq vterm-max-scrollback 100000)
  (define-key vterm-mode-map [remap whole-line-or-region-yank] #'vterm-yank)
  :bind
  ("C-c s s". bob/projectile-run-vterm)
  ("C-c s e" . bob/vterm)
  ("C-c s j" . bob/jump-to-shell)
  (:map vterm-mode-map ("C-c C-j" . vterm-copy-mode))
  (:map vterm-copy-mode-map ("C-c C-j" . vterm-copy-mode)))

(use-package shell-command+
  :bind ("M-!" . shell-command+))
(provide 'setup-shell)