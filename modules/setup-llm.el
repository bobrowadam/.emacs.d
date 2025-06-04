;;; setup-llm.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;; commentary

;;; Code:
(defvar ai-assistant-prompt "You are a large language model living in Emacs and a helpful assistant. Respond concisely and as short as possible. When using tools, tell me what you are about to do. don't ever apologize if some error happened or if you were wrong in working with the tool. If you are not able to use the tool let me know what you think is the problem and let me debug it.
Be very aware of the tool API and the arguments it needs. failing to do so will cause an unrecoverable error in the flow.")

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  ;; (gptel-max-tokens 8192)
  ;; (gptel-max-tokens nil)
  :config
  (exec-path-from-shell-initialize)
  (use-package emacs-agent
    :demand t
    :load-path "~/source/emacs-agent/"
    :ensure nil)
  (add-to-list 'gptel-directives (cons 'ai-assitant ai-assistant-prompt))
  (defun bob/gptel-switch-to-gptel-buffer ()
    "Switch to a buffer with `gptel-mode' active."
    (interactive)
    (if-let ((gptel-buffers
              (seq-filter
               (lambda (buff)
                 (with-current-buffer buff
                   (bound-and-true-p gptel-mode)))
               (buffer-list)))
             (gptel-buffer (completing-read "GPT buffer: " (mapcar 'buffer-name gptel-buffers))))
        (switch-to-buffer gptel-buffer)
      (message "No GPTel buffers found.")))
  (when-let ((credentials (setenv "ANTHROPIC_API_KEY"
                                  (-some-> (plist-get (car (auth-source-search :host "claude.ai")) :secret) funcall))))
    (setq
     gptel-model 'claude-opus-4-20250514
     gptel-backend (gptel-make-anthropic
                       "Claude"
                     :stream t
                     :key credentials)))

  :bind
  ("C-c g g" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g a d" . gptel-context-add)
  ("C-c g a f" . gptel-context-add-file)
  ("C-c g z" . bob/gptel-switch-to-gptel-buffer)
  (:map gptel-mode-map ("C-c g s" . gptel-menu))
  :hook
  (org-mode . (lambda ()
                (when (-some->> (buffer-file-name) (s-match "^.+gptel\.org$" ))
                  (gptel-mode 1)))))

;; helm is an aidermacs dependency
(use-package helm)
(use-package vterm)
(use-package aidermacs
  :disabled t
  :ensure (:fetcher github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-subtree-only nil)
  (aidermacs-auto-commits nil)
  (aidermacs-architect-model "o3")
  ;; (aidermacs-backend 'vterm)
  :config
  (aidermacs-setup-minor-mode)
  (setenv "ANTHROPIC_API_KEY"
          (-some-> (plist-get (car (auth-source-search :host "claude.ai")) :secret) funcall))
  (setenv "OPENAI_API_KEY"
          (plist-get (car (auth-source-search :host "api.openai.com")) :secret))
  :bind ("C-c g c" . aidermacs-transient-menu))

(use-package aider
  :ensure (:repo "tninja/aider.el" :fetcher github :files ("*.el"))
  :custom
  (aider-args '("--model" "sonnet" "--no-auto-accept-architect" "--no-auto-commits"))
  :config
  (setenv "ANTHROPIC_API_KEY"
          (-some-> (plist-get (car (auth-source-search :host "claude.ai")) :secret) funcall))
  :bind ("C-c g c" . 'aider-transient-menu))

(use-package minuet
  :custom
  (minuet-provider 'claude)
  :bind
  (:map prog-mode-map ("C-M-i" . #'minuet-complete-with-minibuffer))
  :config
  (setenv "ANTHROPIC_API_KEY"
          (-some-> (plist-get (car (auth-source-search :host "claude.ai")) :secret) funcall))

  (defvar minuet-claude-options
    `(:model "claude-3-haiku-20240307"
             :max_tokens 512
             :api-key "ANTHROPIC_API_KEY"
             :system
             (:template minuet-default-system-template
                        :prompt minuet-default-prompt
                        :guidelines minuet-default-guidelines
                        :n-completions-template minuet-default-n-completion-template)
             :fewshots minuet-default-fewshots
             :chat-input
             (:template minuet-default-chat-input-template
                        :language-and-tab minuet--default-chat-input-language-and-tab-function
                        :context-before-cursor minuet--default-chat-input-before-cursor-function
                        :context-after-cursor minuet--default-chat-input-after-cursor-function)
             :optional nil)
    "config options for Minuet Claude provider"))

(use-package llm
  :custom
  (llm-warn-on-nonfree nil))

(use-package llm-tool-collection
  :ensure (:repo "skissue/llm-tool-collection" :fetcher github :files ("*.el")))

(use-package ai-org-chat
  :ensure (:repo "ultronozm/ai-org-chat.el" :fetcher github :files ("*.el"))
  :custom
  (ai-org-chat-user-name "Adam")
  (ai-org-chat-dir "~/ai-chats")
  :config
  (dolist (tool-config (llm-tool-collection-get-all))
    (ai-org-chat-register-tool (apply #'llm-make-tool tool-config)))
  (setenv
   "ANTHROPIC_KEY"  (-some-> (plist-get (car (auth-source-search :host "claude.ai")) :secret) funcall))
  (ai-org-chat-select-model "sonnet 4")
  :bind
  (:map global-map
        ("C-c /" . ai-org-chat-new))
  (:map ai-org-chat-minor-mode-map
        ("C-c C-m" . ai-org-chat-respond)))

(use-package mcp
  :ensure (:repo "lizqwerscott/mcp.el" :fetcher github :files ("*.el"))
  :config
  (require 'gptel-integrations)
  (setq mcp-hub-servers
        `(("linear" . (
                       :command "npx"
                       :args ("-y" "mcp-remote" "https://mcp.linear.app/sse")
                       ))
          ("postgres" . (
                         :command  "postgres-mcp"
                         :args ("--access-mode=unrestricted")
                         :env (:DATABASE_URI "postgresql://postgres:grain@localhost:5432/grain")
                         )))))

(provide 'setup-llm)
;;; setup-llm.el ends here
