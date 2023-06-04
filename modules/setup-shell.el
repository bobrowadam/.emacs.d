(use-package inf-mongo
  :after startup
  :demand t
  :load-path "~/source/inf-mongo/"
  :hook
  (inf-mongo-mode-hook . (lambda ()
                      (progn
                        (setq comint-input-ring-file-name "~/.dbshell")
                        (setq-local comint-input-history-ignore "\\(^#\\)\\|\\(^INFMONGO.+\\)")
                        (comint-read-input-ring 'silent)))))

(use-package shell-defuns
  :after startup
  :if (window-system)
  :load-path "./site-lisp"
  :demand t)

(use-package vterm
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
  :demand t
  :load-path "~/source/aweshell"
  :bind ("C-!" . aweshell-toggle))

(provide 'setup-shell)
