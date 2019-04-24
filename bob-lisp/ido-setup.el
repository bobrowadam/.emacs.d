(require 'ido)
(ido-mode 1)
(flx-ido-mode 1)
(use-package ido-completing-read+
  :init
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-at-point
  :demand
  :config (ido-at-point-mode 1))

(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  :init (smex-initialize))

(provide 'ido-setup)
