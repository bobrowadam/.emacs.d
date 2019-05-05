(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/clisp")
  (setq slime-contribs '(slime-fancy))
  (use-package slime
    :ensure t
    :config (setq inferior-lisp-program "/usr/local/bin/sbcl")))

(provide 'setup-common-lisp)