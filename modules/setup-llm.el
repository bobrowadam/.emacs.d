;;; setup-llm.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;; commentary

;;; Code:
(defvar ai-assistant-prompt "
You are a large language model living in Emacs and a helpful assistant. Respond concisely and as short as possible.

TOOL USAGE STRATEGY:
When handling code-related requests, follow this analysis approach:

1. CONTEXT GATHERING:
   - When the context is >~500 lines, use the `summarize-chat-buffer` to compact the context. This is very important!
   - Use `get_project_root` and `get_buffers_name_in_project` to understand project structure
   - Use `run_rg` to search for patterns, functions, or specific code constructs
   - Use `find_files_by_regex_in_project` to locate relevant files

2. CODE ANALYSIS:
   - Use `eglot_context` for precise symbol analysis (definitions, references, documentation)
   - Always specify buffer_name, symbol, and line_number when available
   - Use `get_flymake_diagnostics` to check for errors/warnings in relevant buffers

3. INVESTIGATION WORKFLOW:
   - Start broad (search patterns with rg)
   - Narrow down (examine specific files/buffers)
   - Go deep (use LSP for symbol-level analysis)
   - Cross-reference (check related symbols and dependencies)

4. RESPONSE STRUCTURE:
   - State what you're investigating
   - Show the analysis steps taken
   - Provide findings with LSP-backed evidence
   - Suggest next actions if applicable

When code issues arise, prioritize LSP-based analysis over assumptions. Use the structured knowledge from eglot_context to provide precise, compiler-grade insights rather than generic advice.

[rest of original prompt about conciseness, error handling, etc.]
Respond concisely and as short as possible.
When using tools, tell me what you are about to do. don't ever apologize if some error happened or if you were wrong in working with the tool. If you are not able to use the tool let me know what you think is the problem and let me debug it.
Be very aware of the tool API and the arguments it needs. failing to do so will cause an unrecoverable error in the flow.
Since I'm paying for the LLM usage and my workplace doesn't help me, try to use as little tokens as you can. thanks!")
(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-max-tokens 400)
  :config

  (exec-path-from-shell-initialize)
  (add-to-list 'gptel-directives (cons 'ai-assitant ai-assistant-prompt))
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all))
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
  (when-let ((credentials
              (setenv "ANTHROPIC_API_KEY"
                      (-some->
                          (plist-get (car (auth-source-search :host "claude.ai")) :secret)
                        funcall))))
    (gptel-make-anthropic
        "Claude"
      :stream t
      :key credentials))
  (setq
     gptel-model 'llama3.2:latest
     gptel-backend
     (gptel-make-ollama "Ollama"
       :host "localhost:11434"
       :stream t
       :models '(mistral:latest llama3.2:latest)))

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
  :demand t
  :ensure (:repo "skissue/llm-tool-collection" :fetcher github :files ("*.el"))
  :config
  (require 'llm-tools))

(defun bob/tool-find-files-by-regex (regex)
    "Find files in the current project matching REGEX pattern.
Returns a list of absolute file paths that match the provided regex."
    (if-let ((proj (project-current)))
        (let* ((default-directory (project-root proj))
               (all-files (project-files proj))
               (matching-files (seq-filter (lambda (file)
                                             (string-match-p regex (file-name-nondirectory file)))
                                           all-files)))
          (if matching-files
              matching-files
            "No files matching the pattern were found."))
      "No project found. Please open a file within a project first."))

(defun bob/tool-run-rg (pattern &optional file-pattern)
  "Run ripgrep (rg) on the current project.
PATTERN is the search pattern.
FILE-PATTERN is an optional file pattern to filter files."
  (if-let ((proj (project-current)))
      (let* ((default-directory (project-root proj))
             (cmd (if file-pattern
                      (format "rg --no-heading --line-number --with-filename %s %s"
                              (shell-quote-argument pattern)
                              (shell-quote-argument file-pattern))
                    (format "rg --no-heading --line-number --with-filename %s"
                            (shell-quote-argument pattern))))
             (result (shell-command-to-string cmd)))
        (if (string-empty-p result)
            "No matches found."
          result))
    "No project found. Please open a file within a project first."))

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
  (add-to-list 'ai-org-chat-models  '("llama 3.2" :package llm-ollama :provider make-llm-ollama
                                    :chat-model "llama3.2:latest"))
  (ai-org-chat-select-model "llama 3.2")
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
