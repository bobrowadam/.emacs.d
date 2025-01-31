(use-package gptel
  :custom
  (gptel-max-tokens 8192)
  :config
  (setq
   gptel-model 'claude-3-5-sonnet-20241022
   gptel-backend (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                                           car
                                           (plist-get :secret)
                                           funcall)))
                   (gptel-make-anthropic
                       "Claude"
                     :stream t
                     :key credentials)))
  :custom
  (gptel-default-mode 'org-mode)
  :bind
  ("C-c g g" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g a d" . gptel-context-add)
  ("C-c g a f" . gptel-context-add-file)
  (:map gptel-mode-map ("C-c g s" . gptel-menu)))

(use-package claude-shell
  :disabled t
  :commands (claude-shel)
  :custom
  (claude-shell-streaming t)
  :config
  (setq claude-shell-api-token (exec-path-from-shell-copy-env "CLAUDE_SHELL_API_TOKEN"))
  :hook
  (claude-shell-mode . (lambda () (corfu-mode -1))))

;; (package-vc-install '(aider :url "https://github.com/tninja/aider.el"))
(use-package aider
  :config
  (setq
   aider-args (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                                           car
                                           (plist-get :secret)
                                           funcall)))
                   `("--model" "anthropic/claude-3-5-sonnet-20241022" "--anthropic-api-key" ,credentials)))
  :bind ("C-c g c" . aider-transient-menu))

(provide 'setup-llm)
