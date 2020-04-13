(use-package smex
  :disabled t
  :ensure t)

(use-package amx
  :after ivy
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/.emacs.d/amx-items")
  (amx-history-length 50)
  (amx-show-key-bindings t)
  :config
  (amx-mode 1))

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
  :after ivy
  :init
  (setq counsel-rg-base-command
        "rg --heading --context-separator \" \" -i -M 120 --line-number --color never %s .")
  :config (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

(provide 'setup-ivy)
