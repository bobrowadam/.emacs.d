;;; package --- Summary
;;; Commentary:
;;;; Python setup
;;; Code:

(use-package jedi
  :ensure t
  :init
  (setq jedi:complete-on-dot t)
  (setq python-shell-interpreter "/usr/bin/python2.7")
  :hook
  (python-mode-hook . jedi:setup))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package anaconda-mode
  :disabled t
  :ensure t
  :hook
  (python-mode-hook . anaconda-mode)
  (python-mode-hook . anaconda-eldoc-mode))

;;; setup-python.el ends here
