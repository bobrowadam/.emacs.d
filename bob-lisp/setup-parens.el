(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package smartparens
  :bind (:map smartparens-mode-map
              ("M-(" . sp-wrap-round)
              ("M-s" . sp-unwrap-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-'" . sp-rewrap-sexp)
              ("M-S" . sp-split-sexp)
              ("M-J" . sp-join-sexp)
              ("M-W" . sp-copy-sexp))
  :config
  (show-smartparens-global-mode t)
  :hook
  (js2-mode . smartparens-strict-mode)
  (comint-mode . smartparens-mode)
  (scala-mode . smartparens-mode))

(provide 'setup-parens)
