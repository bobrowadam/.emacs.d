(use-package inf-mongo :after startup)

(use-package shell-defuns
  :demand t  
  :after startup
  :if (window-system)
  :load-path "./site-lisp")

(use-package vterm
  :disabled t
  :demand t
  :after (startup shell-defuns)
  :if (window-system)
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

(use-package aweshell
  :disabled t
  :load-path "~/source/aweshell"
  :bind ("C-!" . aweshell-toggle))

(use-package eat
  :commands (eat bob/jump-to-shell eat-project)
  :init
  (setq eat-term-name "xterm-256color")
  :bind
  ("C-!" . eat)
  ("C-c s j" . bob/jump-to-shell)
  ("C-x p s" . eat-project)
  :hook
  (eat-mode . bobs/hide-mode-line))

(provide 'setup-shell)
