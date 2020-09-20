(use-package paredit
  :hook
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode))

(use-package smartparens
  :demand t
  :init
  (setq sp-ignore-modes-list '(minibuffer-inactive-mode emacs-lisp-mode eval-expression-minibuffer-setup))
  :config
  (require 'smartparens-config)
  ;; (smartparens-global-strict-mode t)
  (smartparens-global-mode t)
  ;; (sp-local-pair 'js2-mode "{ " " }" :trigger-wrap "{")
  )

(provide 'setup-parens)
