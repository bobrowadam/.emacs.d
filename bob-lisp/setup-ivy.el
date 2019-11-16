(use-package smex
  :ensure t)

(use-package ivy
  :demand t
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d%d) ")
  (ivy-mode 1))

(use-package counsel
  :if (window-system)
  :init
  (setq counsel-rg-base-command
        "rg --heading --context-separator \" \" -i -M 120 --line-number --color never %s ."))

(provide 'setup-ivy)
