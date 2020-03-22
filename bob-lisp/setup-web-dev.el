;;; 'setup-web-dev.el --- summary -*- lexical-binding: t -*-

(use-package web-mode
  :mode "\\.html\\'"
  :hook
  (web-mode . yas-minor-mode)
  :config
  (setq web-mode-enable-auto-pairing t))

(use-package skewer-mode)
(use-package impatient-mode)

(provide 'setup-web-dev)

;;; web-dev.el ends here
