(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook
  (js2-mode . tide-setup)
  (js2-mode . js2-imenu-extras-mode)
  (js2-mode . js2-mode-hide-warnings-and-errors)
  (js2-mode . electric-indent-mode)
  (js2-mode . yas-minor-mode)
  (js2-mode . bob/setup-js2-flycheck)
  (js2-mode . origami-mode)
  (js2-mode . highlight-indent-guides-mode)
  (js2-mode . eldoc-mode)
  :bind (:map js2-mode-map
              ("C-<tab>" . js2-indent-bounce)
              ("C-c C-s" . nil)
              ("C-=" . origami-toggle-node)
              ("C-x C-e" . js-send-last-sexp)
              ("C-c d" . dap-hydra))
  :config
  (setenv "BOB_DIR" (format "%s%s" (getenv "HOME") "/source/bob"))
  (exec-path-from-shell-copy-envs '("WHATSAPP_NUMBER"))
  (exec-path-from-shell-copy-envs '("LOCAL_WHATSAPP_NUMBER"))
  ;; (nvm-use "v12.14.0")
  (setq-default js2-auto-indent-p nil)
  (setq-default unset-electric-indent)
  (setq-default js-indent-level 2))

(use-package tide
  :if (window-system)
  :demand t
  :bind (:map tide-mode-map
              ("C-c C-n" . tide-rename-symbol)
              ("C-c C-r" . tide-references)
              ("C-c C-c C-b" . tide-compile-file))
  :hook (before-save . tide-format-before-save)
  :init
  (setq tide-node-executable "node")
  (setq company-tooltip-align-annotations t)
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log")))

(defun bob/setup-js2-flycheck ()
  (progn
    ;; (lsp)
    (dap-mode)
    (add-node-modules-path)
    (flycheck-mode +1)
    ;; (lsp-diagnostics-mode)
    (flycheck-select-checker 'javascript-eslint)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide)
    )
  )

(use-package nodejs-repl)
(use-package json-mode
  :hook (json-mode . origami-mode)
  :bind (:map json-mode-map ("C-=" . origami-toggle-node)))

(use-package jq-format
  :demand t
  :after json-mode)

;; ################# DISABLED #########################


(use-package js-comint :disabled t :after js2-mode)
(use-package ts-comint :disabled t)

(use-package indium
  :disabled t
  :hook
  (js2-mode . indium-interaction-mode)
  (js-mode . indium-interaction-mode)
  :bind
  (:map indium-debugger-mode-map ("C-c C-SPC" . indium-debugger-step-over)))

(provide 'setup-js)
