(use-package gptel
  :custom
  (gptel-model 'gpt-4o)
  (gptel-default-mode 'org-mode)
  (gptel-max-tokens 8192)
  ;; (gptel-max-tokens nil)
  :config
  (load "./llm-tools.el")
  (add-to-list 'gptel-directives (cons 'ai-assitant "You are a large language model living in Emacs and a helpful assistant. Respond concisely. When using tools, tell me what you are about to do. don't ever apologize if some error happened or if you were wrong in working with the tool. If you are not able to use the tool let me know what you think is the problem and let me debug it."))
  (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                            car
                            (plist-get :secret)
                            funcall)))
    (gptel-make-anthropic
        "Claude"
      :stream t
      :key credentials))
  :bind
  ("C-c g g" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g a d" . gptel-context-add)
  ("C-c g a f" . gptel-context-add-file)
  (:map gptel-mode-map ("C-c g s" . gptel-menu)))

(use-package helm)
(use-package aidermacs
  :ensure (:fetcher github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-subtree-only nil)
  :config
  (setq
   aidermacs-args  (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                                             car
                                             (plist-get :secret)
                                             funcall)))
                     `("--anthropic-api-key" ,credentials)))
  :bind ("C-c g c" . aidermacs-transient-menu))

(provide 'setup-llm)
