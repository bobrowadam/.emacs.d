(defvar ai-assistant-prompt "You are a large language model living in Emacs and a helpful assistant. Respond concisely and as short as possible. When using tools, tell me what you are about to do. don't ever apologize if some error happened or if you were wrong in working with the tool. If you are not able to use the tool let me know what you think is the problem and let me debug it.
Be very aware of the tool API and the arguments it needs. failing to do so will cause an unrecoverable error in the flow.")

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  ;; (gptel-max-tokens 8192)
  (gptel-max-tokens nil)
  :config
  (defun bob/reset-tools ()
    (interactive)
    (setq gptel--known-tools nil)
    (setq gptel-tools nil))
  (add-to-list 'gptel-directives (cons 'default ai-assistant-prompt))
  (add-to-list 'gptel-directives (cons 'ai-assitant ai-assistant-prompt))
  (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                            car
                            (plist-get :secret)
                            funcall)))
    (setq
     gptel-model 'claude-3-7-sonnet-20250219
     gptel-backend (gptel-make-anthropic
                       "Claude"
                     :stream t
                     :key credentials)))
  (load "./llm-tools.el")
  :bind
  ("C-c g g" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g a d" . gptel-context-add)
  ("C-c g a f" . gptel-context-add-file)
  (:map gptel-mode-map ("C-c g s" . gptel-menu)))

;; helm is an aidermacs dependency
(use-package helm)
(use-package vterm)
(use-package aidermacs
  :ensure (:fetcher github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-subtree-only t)
  (aidermacs-auto-commits nil)
  (aidermacs-architect-model "o1")
  (aidermacs-backend 'vterm)
  :config
  (aidermacs-setup-minor-mode)
  (setq
   aidermacs-args  (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                                             car
                                             (plist-get :secret)
                                             funcall)))
                     `("--anthropic-api-key" ,credentials)))
  :bind ("C-c g c" . aidermacs-transient-menu))

(provide 'setup-llm)
