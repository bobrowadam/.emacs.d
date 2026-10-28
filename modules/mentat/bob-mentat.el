;;; bob-mentat.el --- Mentat configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and small integrations for Mentat, the Emacs interface to Pi.

;;; Code:

(require 'ansi-color)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defun bob/mentat--read-agent-instructions-file (file)
  "Read and trim Mentat agent instructions FILE."
  (cond
   ((not (file-exists-p file))
    (error "Missing Mentat agent instructions file: %s" file))
   ((not (file-readable-p file))
    (error "Unreadable Mentat agent instructions file: %s" file))
   (t
    (with-temp-buffer
      (condition-case err
          (insert-file-contents file)
        (error
         (error "Unable to read Mentat agent instructions file %s: %s"
                file (error-message-string err))))
      (let ((instructions (string-trim (buffer-string))))
        (if (string-empty-p instructions)
            (error "Empty Mentat agent instructions file: %s" file)
          instructions))))))

(defun bob/mentat-load-agent-and-common-instructions (agent)
  "Load role-specific and shared instructions for Mentat AGENT."
  (let* ((directory (file-name-directory
                     (or load-file-name
                         (symbol-file
                          'bob/mentat-load-agent-and-common-instructions))))
         (files (mapcar (lambda (name)
                          (expand-file-name (format "agents/%s.md" name)
                                            directory))
                        (list agent "common"))))
    (mapconcat #'bob/mentat--read-agent-instructions-file files "\n\n")))

(defvar mentat--buffer-model-provider)
(declare-function bob/elpaca-package-dir "init-generated" (package))
(declare-function fnm-auto-use-mode "fnm" (&optional arg))
(declare-function mentat-extension-status "mentat" (extension))
(declare-function mentat--register-extension "mentat-extensions" (name source tools))
(declare-function mentat-reset-extensions "mentat-extensions" ())
(declare-function mentat-reset-subagent-definitions "mentat" ())
(declare-function mentat--register-subagent "mentat" (&rest args))
(declare-function mentat-refresh-mode-lines "mentat" ())
(declare-function mentat-footer-mode "mentat-footer" (&optional arg))
(declare-function mentat-gpu-cards-mode "mentat-gpu-cards" (&optional arg))
(declare-function gpu-decoration-supported-p "mtlfns.m" (&optional frame))
(declare-function gpu-enable-for-frame "mtlfns.m" (frame))

(defun bob/mentat-enable-gpu-frame (frame)
  "Enable card-capable Metal rendering on graphical NS FRAME."
  (when (and (eq (framep frame) 'ns)
             (fboundp 'gpu-decoration-supported-p)
             (not (gpu-decoration-supported-p frame)))
    (condition-case err
        (gpu-enable-for-frame frame)
      (error (message "Mentat GPU rendering unavailable: %s"
                      (error-message-string err))))))

(defun bob/mentat-enable-gpu-cards ()
  "Enable quiet GPU cards, retaining normal borders on other backends."
  (when (fboundp 'gpu-decoration-supported-p)
    (mentat-gpu-cards-mode 1)))

(defun bob/mentat-enable-footer ()
  "Enable the atomic footer in a Mentat conversation buffer."
  (mentat-footer-mode 1))

(defun bob/mentat-prompt-disable-corfu-auto ()
  "Open Corfu in Mentat prompts only on request."
  (setq-local corfu-auto nil))

(defvar jinx--dicts)
(declare-function jinx--load-module "jinx" ())
(declare-function jinx--load-dicts "jinx" ())
(declare-function jinx--mod-suggest "jinx" (dict word))
(declare-function jinx--word-valid-p "jinx" (word))

(defun bob/mentat-word-corrections (word _context)
  "Return Jinx corrections for misspelled WORD in suggestion order."
  (require 'jinx)
  (jinx--load-module)
  (jinx--load-dicts)
  (unless (jinx--word-valid-p word)
    (seq-take
     (apply #'append
            (mapcar (lambda (dict) (jinx--mod-suggest dict word)) jinx--dicts))
     10)))

(defun bob/mentat-initialize-fnm ()
  "Load FNM and select the Node environment used by Mentat."
  (unless (featurep 'fnm)
    (when-let* ((directory (bob/elpaca-package-dir "fnm")))
      (add-to-list 'load-path directory))
    (require 'fnm))
  (fnm-auto-use-mode 1))

(defmacro bob/mentat-define-subagent (name instructions &rest properties)
  "Define Mentat subagent NAME with explicit INSTRUCTIONS and PROPERTIES.

INSTRUCTIONS is evaluated when the declaration runs.  PROPERTIES are literal
Mentat subagent properties."
  (declare (indent 2) (debug (symbol form &rest form)))
  `(mentat--register-subagent
    ',name
    (append ',properties (list :instructions ,instructions))))

(bob/mentat-initialize-fnm)

(transient-define-prefix bob/mentat-codex-menu ()
  "Open Codex controls."
  ["Codex"
   ("s" "Show usage" (lambda ()
                       (interactive)
                       (mentat-run-extension-command "/codex")))
   ("R" "Reset usage" (lambda ()
                        (interactive)
                        (mentat-run-extension-command "/codex reset")))])

(use-package mentat
  :demand t
  :ensure nil
  :load-path "/Users/bob/source/mentat"
  :commands mentat
  :hook ((mentat-buffer-mode . bob/mentat-enable-footer)
         (mentat-buffer-mode . bob/mentat-enable-gpu-cards))
  :custom
  (mentat-show-injected-reminders nil)
  (mentat-tool-default-display-state 'summary)
  (mentat-streaming-tool-display-state 'expanded)
  (mentat-pi-directory (expand-file-name "~/.pi/agent"))
  (mentat-diagnostic-capture-enabled t)
  (mentat-enabled-extensions
   '(codex worktree-skills claude-bridge))
  (mentat-pi-disabled-tools nil)
  (mentat-emacs-tool-instructions
   (concat
    "Prefer functional Emacs Lisp. Keep transformations explicit and limit "
    "mutation to stateful boundaries. Use Dash for list processing, s.el for "
    "strings, f.el for files and paths, and ht.el for hash tables when they "
    "make the code clearer. Use aio or request for asynchronous work or HTTP "
    "when appropriate. Prefer an existing helper over reimplementing it. "
    "Inspect package documentation before use and declare dependencies with "
    "`require`."))
  (mentat-default-provider "azure")
  (mentat-default-model "gpt-6.1-sol")
  (mentat-default-effort "medium")
  (mentat-org-skill-global-directory
   (expand-file-name "modules/mentat/skills/" user-emacs-directory))
  (mentat-supervisor-instructions
   (concat
    "Do not use subagents unless the user explicitly tells you to.\n"
    "Keep reasoning, investigation, implementation, and validation in the "
    "parent session.\n"
    "When explicitly requested, delegate only small, bounded tasks, such as "
    "watching CI. For CI watching, use ci-watcher with background=true.\n"
    "A matching role, potential parallelism, or specialist expertise does "
    "not authorize delegation.\n\n"
    "Keep framing, design decisions, reconciliation, and the final answer in "
    "the parent session. Keep shared-worktree edits sequential. When "
    "delegating, give the child a self-contained objective, relevant files, "
    "constraints, expected deliverable, and validation requirements. Verify "
    "the handoff against concrete evidence."))

  (mentat-pi-profiles
   '(("Work"
      :directory "~/.pi/agent"
      :environment (("MENTAT_LEDGER_BACKEND" . "decisions")
                    ("MENTAT_LEDGER_DECISIONS_ENDPOINT"
                     . "https://credits-eu-resource.services.ai.azure.com/openai/v1")
                    ("MENTAT_LEDGER_DECISIONS_DEPLOYMENT" . "gpt-6-luna")
                    ("AZURE_DECISIONS_KEY"
                     . (lambda ()
                         (auth-source-pick-first-password
                          :host "credits-eu-resource.services.ai.azure.com"
                          :user "apikey"))))
      :extensions (codex worktree-skills claude-bridge)
      :arguments ("--extension" "builtin:codemode")
      :subagent-enabled-models ("azure/gpt-6.1-sol"
                                "azure/gpt-6-astra"
                                "azure/gpt-6-luna"
                                "openai-codex/gpt-6.1-sol"
                                "openai-codex/gpt-6-astra"
                                "openai-codex/gpt-6-luna"
                                "claude-bridge/claude-opus-5-5")
      :subagents (explorer pr-reviewer reviewer worker ci-watcher ui-manual-qa)
      :tools ("emacs_read"
              "emacs_read_buffer"
              "emacs_edit"
              "emacs_write"
              "emacs_grep"
              "emacs_find"
              "emacs_ls"
              "emacs_run_process"
              "emacs_eval_elisp"
              "emacs_eval_async"
              "emacs_elisp_call"
              "hindsight_recall"
              "hindsight_remember"
              "hindsight_reflect"
              "subagent"
              "subagent_progress"
              "ledger_recall"
              "ledger_search"
              "codemode")
      :disabled-tools ("grep" "find" "ls"))
     ("Private"
      :directory "~/.pi/agent-private"
      :environment (("CLAUDE_CONFIG_DIR" . "/Users/bob/.claude-private"))
      :extensions (codex worktree-skills claude-bridge)
      :arguments ("--extension" "builtin:codemode")
      :default-provider "openai-codex"
      :default-model "gpt-6.1-sol"
      :enabled-models ("openai-codex/gpt-6.1-sol"
                       "openai-codex/gpt-6-astra"
                       "claude-bridge/claude-opus-5-5")
      :subagent-enabled-models ("openai-codex/gpt-6.1-sol"
                                "openai-codex/gpt-6-luna"
                                "claude-bridge/claude-opus-5-5")
      :subagents (explorer pr-reviewer reviewer worker ci-watcher ui-manual-qa)
      :tools ("emacs_read"
              "emacs_read_buffer"
              "emacs_edit"
              "emacs_write"
              "emacs_grep"
              "emacs_find"
              "emacs_ls"
              "emacs_run_process"
              "emacs_eval_elisp"
              "emacs_eval_async"
              "emacs_elisp_call"
              "hindsight_recall"
              "hindsight_remember"
              "hindsight_reflect"
              "subagent"
              "subagent_progress"
              "ledger_recall"
              "ledger_search"
              "codemode")
      :disabled-tools ("grep" "find" "ls"))
     ("basic"
      :directory "~/.pi/agent-basic"
      :arguments ("--no-context-files" "--no-skills"
                  "--extension" "builtin:codemode")
      :agent-features nil
      :extensions (pi-update codex claude-bridge)
      :tools ("read" "bash" "edit" "write" "grep" "ledger_recall" "ledger_search" "codemode"))))

  (mentat-enabled-models
   '("azure/gpt-6.1-sol"
     "azure/gpt-6-astra"
     "openai-codex/gpt-6.1-sol"
     "openai-codex/gpt-6-astra"
     "claude-bridge/claude-opus-5-5"))

  (mentat-subagent-enabled-models
   '("azure/gpt-6.1-sol"
     "azure/gpt-6-astra"
     "azure/gpt-6-luna"
     "openai-codex/gpt-6.1-sol"
     "openai-codex/gpt-6-astra"
     "openai-codex/gpt-6-luna"
     "claude-bridge/claude-opus-5-5"))

  (mentat-extension-menu-commands
   '(("C" "Codex controls" bob/mentat-codex-menu)))

  (mentat-mode-line-extra-functions nil)

  (mentat-prompt-extra-completion-at-point-functions nil)
  (mentat-prompt-word-completion-min-prefix 2)

  (mentat-prompt-extra-word-candidate-functions
   '(bob/mentat-prose-word-candidates))

  (mentat-prompt-word-correction-functions
   '(bob/mentat-word-corrections))

  (mentat-prompt-word-candidate-score-function
   #'bob/mentat-prose-word-candidate-score)

  :config
  (require 'mentat-footer)
  (require 'worktree-session
           (expand-file-name "modules/mentat/mentat-tools/worktree-session"
                             user-emacs-directory))
  ;; Mentat previews its top-ranked word inline; M-TAB shows alternatives.
  (add-hook 'mentat-prompt-mode-hook #'bob/mentat-prompt-disable-corfu-auto)
  (add-hook 'mentat-prompt-mode-hook
            (lambda ()
              (setq-local completion-preview-minimum-symbol-length 2)))
  ;; Themes color the preview like text; keep it a dim ghost in every theme.
  (custom-set-faces
   '(completion-preview ((t :inherit shadow :foreground unspecified)))
   '(completion-preview-exact ((t :inherit completion-preview))))
  (add-hook 'after-make-frame-functions #'bob/mentat-enable-gpu-frame)
  (mapc #'bob/mentat-enable-gpu-frame (frame-list))
  (mentat-reset-extensions)
  ;; Registered only for child sessions.  Main Mentat sessions load this
  ;; internally, so it must not be included in `mentat-enabled-extensions'.
  (mentat-define-extension mentat-emacs
    :source "/Users/bob/source/mentat/pi-extensions/src/mentat-emacs.ts"
    :tools (emacs_read emacs_read_buffer emacs_edit emacs_write
            emacs_grep emacs_find emacs_ls emacs_run_process
            emacs_eval_elisp emacs_eval_async emacs_elisp_call))
  ;; Opt basic into notices only; ordinary profiles already load this internally.
  (mentat-define-extension pi-update
    :source mentat--pi-update-file)
  (mentat-define-extension codex
    :source "/Users/bob/.pi/agent/extensions/src/codex/controls.ts")
  (mentat-define-extension worktree-skills
    :source "/Users/bob/.pi/agent/extensions/src/worktree-skills.ts")
  (with-eval-after-load 'mentat-buffer
    (autoload 'bob/gpu-snapshot "bob-gpu-snapshot" nil t)
    (keymap-set mentat-buffer-mode-map "C" #'bob/gpu-snapshot))
  (mentat-define-extension claude-bridge
    :source "npm:pi-claude-bridge@0.9.2")
  (mentat-define-extension web-search
    :source "/Users/bob/.pi/agent/extensions/src/web-search/index.ts"
    :tools (exa_search jina_reader))

  (mentat-reset-subagent-definitions)

  (bob/mentat-define-subagent explorer
      (bob/mentat-load-agent-and-common-instructions "explorer")
    :description "Read-only project investigation"
    :model ("azure/gpt-6.1-sol" "openai-codex/gpt-6.1-sol")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 4)

  (bob/mentat-define-subagent reviewer
      (bob/mentat-load-agent-and-common-instructions "reviewer")
    :description "Review one code change and optionally run read-only validation"
    :model ("claude-bridge/claude-opus-5-5"
            "azure/gpt-6-astra" "openai-codex/gpt-6-astra")
    :thinking high
    :extensions (mentat-emacs claude-bridge)
    :concurrency 8)

  (bob/mentat-define-subagent pr-reviewer
      (bob/mentat-load-agent-and-common-instructions "pr-reviewer")
    :description "Review one assigned PR slice using the parallel-review finding format"
    :model ("claude-bridge/claude-opus-5-5"
            "azure/gpt-6-astra" "openai-codex/gpt-6-astra")
    :thinking high
    :extensions (mentat-emacs claude-bridge)
    :concurrency 8)

  (bob/mentat-define-subagent ci-watcher
      (bob/mentat-load-agent-and-common-instructions "ci-watcher")
    :description "Run and monitor project validation without changing files"
    :model ("azure/gpt-6-luna" "openai-codex/gpt-6-luna")
    :thinking low
    :extensions (mentat-emacs))

  (bob/mentat-define-subagent worker
      (bob/mentat-load-agent-and-common-instructions "worker")
    :description "Implement one bounded, well-understood delegated change"
    :model ("azure/gpt-6.1-sol" "openai-codex/gpt-6.1-sol")
    :thinking high
    :extensions (mentat-emacs))

  (bob/mentat-define-subagent effect-ts-backend-expert
      (bob/mentat-load-agent-and-common-instructions
       "effect-ts-backend-expert")
    :description "Handle one bounded Effect TypeScript task requiring specialist expertise"
    :model ("azure/gpt-6.1-sol" "openai-codex/gpt-6.1-sol")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 1
    :max-turns 50)

  (bob/mentat-define-subagent frontend-react-expert
      (bob/mentat-load-agent-and-common-instructions
       "frontend-react-expert")
    :description "Handle one bounded React frontend task requiring specialist expertise"
    :model ("azure/gpt-6.1-sol" "openai-codex/gpt-6.1-sol")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 1
    :max-turns 50)

  (bob/mentat-define-subagent ui-manual-qa
      (bob/mentat-load-agent-and-common-instructions "ui-manual-qa")
    :description "Test UI features in a web browser"
    :model ("claude-bridge/claude-opus-5-5"
            "azure/gpt-6.1-sol" "openai-codex/gpt-6.1-sol")
    :thinking medium
    :extensions (mentat-emacs claude-bridge))

  (remove-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines)
  (add-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines t))

(provide 'bob-mentat)

;;; bob-mentat.el ends here
