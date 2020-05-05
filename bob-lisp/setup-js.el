(use-package js2-mode
  :after nvm
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
  (js2-mode . tide-setup)
  (js2-mode . origami-mode)
  (js2-mode . highlight-indent-guides-mode)
  :bind (:map js2-mode-map
              ("C-<tab>" . js2-indent-bounce)
              ("C-c C-s" . nil)
              ("C-=" . origami-toggle-node)
              ("C-x C-e" . js-send-last-sexp))
  :config
  (setenv "BOB_DIR" (format "%s%s" (getenv "HOME") "/source/bob"))
  (nvm-use "v12.14.0")
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2)
  (eldoc-mode +1))

(use-package nvm :demand t)

(use-package typescript-mode
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . highlight-indent-guides-mode)
  (typescript-mode . tide-hl-identifier-mode)
  (typescript-mode . yas-minor-mode)
  (typescript-mode . flycheck-mode)
  (typescript-mode . origami-mode)
  :config
  (eldoc-mode +1)
  (origami-mode +1)
  :bind (:map typescript-mode-map ("C-=" . origami-toggle-node)))

(use-package tide
  :if (window-system)
  :bind (:map tide-mode-map
              ("C-c C-n" . tide-rename-symbol)
              ("C-c C-r" . tide-references)
              ;; ("C-c M-i" . lsp-ui-imenu)
              ("C-c C-c C-b" . tide-compile-file))
  :hook (before-save . tide-format-before-save)
  :init
  (setq tide-node-executable "node")
  (setq company-tooltip-align-annotations t)
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  ;; (setq tide-format-options
  ;;       '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  )

(use-package js-comint
  :after js2-mode
  :init (js-do-use-nvm))

;; (use-package ts-comint :disabled t)
(use-package json-mode
  :hook (json-mode . origami-mode)
  :bind (:map json-mode-map ("C-=" . origami-toggle-node)))

(use-package indium
  :hook
  (js2-mode . indium-interaction-mode)
  (js-mode . indium-interaction-mode)
  :bind
  
  (:map indium-debugger-mode-map ("C-c C-SPC" . indium-debugger-step-over)))

(provide 'setup-js)
