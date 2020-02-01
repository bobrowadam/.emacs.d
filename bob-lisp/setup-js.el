(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  :hook
  (js2-mode . js2-imenu-extras-mode)
  (js2-mode . js2-mode-hide-warnings-and-errors)
  (js2-mode . electric-indent-mode)
  (js2-mode . yas-minor-mode)
  (js2-mode . my/use-eslint-from-node-modules)
  (js2-mode . flycheck-mode)
  (js2-mode . origami-mode)
  ;; (js2-mode . my-load-js2-snippets)
  :bind (:map js2-mode-map
              ("C-<tab>" . js2-indent-bounce)
              ("C-c C-s" . nil)
              ("C-=" . origami-toggle-node)
              ("C-x C-e" . js-send-last-sexp)
              ("C-c C-c C-b" . tide-compile-file))
  :config
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package highlight-indent-guides
  :hook (js2-mode . highlight-indent-guides-mode))

(defun setup-tide-mode ()
  (interactive)
  (when (not (tramp-tramp-file-p (buffer-file-name (current-buffer))))
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-check-syntax-automatically '(idle-change))
    (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)))

(use-package tide
  :if (window-system)
  :after (js2-mode)
  :hook
  (js2-mode . setup-tide-mode)
  (before-save . tide-format-before-save)
  :bind (:map tide-mode-map
              ("C-c C-n" . tide-rename-symbol)
              ("C-c C-r" . tide-references)
              ("C-c M-i" . lsp-ui-imenu))
  :config
  (setq company-tooltip-align-annotations t)
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (setq tide-tsserver-process-environment nil)
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))

(use-package nodejs-repl :disabled)
(use-package js-comint)

(use-package json-mode
  :hook (json-mode . origami-mode)
  :bind (:map json-mode-map ("C-=" . origami-toggle-node)))

(provide 'setup-js)
