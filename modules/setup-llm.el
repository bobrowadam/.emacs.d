;;; setup-llm.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;; commentary

;;; Code:
(defvar ai-assistant-prompt "You are a large language model living in Emacs and a helpful assistant. Respond concisely and as short as possible. When using tools, tell me what you are about to do. don't ever apologize if some error happened or if you were wrong in working with the tool. If you are not able to use the tool let me know what you think is the problem and let me debug it.
Be very aware of the tool API and the arguments it needs. failing to do so will cause an unrecoverable error in the flow.")

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-max-tokens 8192)
  ;; (gptel-max-tokens nil)
  :config
  (exec-path-from-shell-initialize)
  (use-package emacs-agent
    :demand t
    :load-path "~/source/emacs-agent/"
    :ensure nil)
  (defun bob/reset-toolsδ ()
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
  (:map gptel-mode-map ("C-c g s" . gptel-menu))
  :hook
  (org-mode . (lambda ()
                (when (-some-> (buffer-file-name)
                           (s-match ".+\.gptel\.org$"))
                    (gptel-mode)))))

;; helm is an aidermacs dependency
(use-package helm)
(use-package vterm)
(use-package aidermacs
  :ensure (:fetcher github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-subtree-only t)
  (aidermacs-auto-commits nil)
  (aidermacs-architect-model "o1")
  ;; (aidermacs-backend 'vterm)
  :config
  (aidermacs-setup-minor-mode)
  (setenv "ANTHROPIC_API_KEY" (-some-> (auth-source-search :host "claude.ai" :max 1)
              car
              (plist-get :secret)
              funcall))
  (setenv "OPENAI_API_KEY" (-some-> (auth-source-search :host "api.openai.com" :max 1)
                             car
                             (plist-get :secret)
                             funcall))
  :bind ("C-c g c" . aidermacs-transient-menu))

(use-package minuet
  :custom
  (minuet-provider 'claude)
  :bind
  (:map prog-mode-map ("C-M-i" . #'minuet-complete-with-minibuffer))
  :config
  (setenv
   "ANTHROPIC_API_KEY"  (-some-> (auth-source-search :host "claude.ai" :max 1)
                          car
                          (plist-get :secret)
                          funcall))
  (defvar minuet-claude-options
    `(:model "claude-3-5-haiku-20241022"
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

(use-package claude-code
  :disabled t
  :after eat
  :ensure ( :repo "stevemolitor/claude-code.el"
            :fetcher github
            :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c g C" . claude-code-command-map)
  :hook ((claude-code-start . sm-setup-claude-faces))
  :config
  (defun sm-setup-claude-facesδ ()
    (let ((attrs '(:family "JuliaMono" :weight light :height 140)))
      (dolist (face '(eat-shell-prompt-annotation-running
                      eat-shell-prompt-annotation-success
                      eat-shell-prompt-annotation-failure
                      eat-term-bold
                      eat-term-faint
                      eat-term-italic
                      eat-term-slow-blink
                      eat-term-fast-blink))
        (apply 'set-face-attribute face nil attrs))
      (dotimes (i 10)
        (apply 'set-face-attribute (intern (format "eat-term-font-%d" i)) nil attrs))
      (dotimes (i 255)
        (apply 'set-face-attribute (intern (format "eat-term-color-%d" i)) nil attrs))
      (apply 'buffer-face-set attrs))
    (setq line-spacing 0.25))
  (claude-code-mode))

(provide 'setup-llm)
;;; setup-llm.el ends here
