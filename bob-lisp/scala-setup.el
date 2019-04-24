(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  (defun my/toggle-ui-show-doc ()
    (interactive)
    (if (and (boundp 'ui-show-on/?) ui-show-on/?)
        (progn
          (setq-local ui-show-on/? nil)
          (lsp-ui-doc-hide))
      (progn
        (setq-local ui-show-on/? t)
        (lsp-ui-doc-show)))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-peek-enable t
   lsp-ui-sideline-enable t))

(use-package lsp-scala
  :after scala-mode
  :demand t
  :hook ((scala-mode . lsp) (scala-mode . hs-minor-mode))
  :bind
  (:map scala-mode-map
        ("C-c C-." . my/toggle-ui-show-doc)
        ("C-c C-r" . lsp-find-references))
  :init
  (setq lsp-scala-server-command "/usr/local/bin/metals-emacs")
  (setq lsp-print-io t))

(use-package sbt-mode
  :commands sbt-start sbt-command)

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (defun sbt-compile ()
    (interactive)
    (sbt-command "compile"))
  :bind
  (:map scala-mode-map
        ("C-c C-b C-c" . sbt-command)
        ("C-c C-b C-b" . sbt-compile)
        ("C-c C-b C-s". sbt-switch-to-active-sbt-buffer))
  :config
  (setq scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:indent-value-expression t
        scala-indent:default-run-on-strategy
        scala-indent:operator-strategy)
  :hook (scala-mode . smartparens-mode))

(provide 'scala-setup)
