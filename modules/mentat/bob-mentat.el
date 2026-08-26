;;; bob-mentat.el --- Mentat configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and small integrations for Mentat, the Emacs interface to Pi.

;;; Code:

(require 'ansi-color)
(require 'subr-x)

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

(defun bob/mentat-load-agent-instructions (agent)
  "Load role-specific and shared instructions for Mentat AGENT."
  (let* ((directory (file-name-directory
                     (or load-file-name
                         (symbol-file 'bob/mentat-load-agent-instructions))))
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

(defun bob/mentat-initialize-fnm ()
  "Load FNM and select the Node environment used by Mentat."
  (unless (featurep 'fnm)
    (when-let* ((directory (bob/elpaca-package-dir "fnm")))
      (add-to-list 'load-path directory))
    (require 'fnm))
  (fnm-auto-use-mode 1))

(defmacro bob/mentat-define-subagent (name &rest properties)
  "Define Mentat subagent NAME with loaded role instructions.

PROPERTIES are literal Mentat subagent properties; the role-specific
`:instructions' value is loaded at runtime from NAME."
  (declare (indent 1) (debug (symbol &rest form)))
  `(mentat--register-subagent
    ',name
    (append ',properties
            (list :instructions
                  (bob/mentat-load-agent-instructions ,(symbol-name name))))))

(bob/mentat-initialize-fnm)

(defun bob/mentat-codex-weekly-usage ()
  "Return the Codex weekly allowance remaining for Mentat's mode line."
  (when (equal mentat--buffer-model-provider "openai-codex")
    (when-let* ((status (mentat-extension-status "codex"))
                (plain (ansi-color-filter-apply status)))
      (let (remaining)
        (cond
         ((or (and (string-match
                    "\\([0-9]+\\)% \\(?:7d\\|1w\\|wk\\)\\b" plain)
                   (setq remaining (string-to-number (match-string 1 plain))))
              (and (string-match
                    "\\(?:7d\\|1w\\|wk\\)[^\n]*?\\([0-9]+\\(?:\\.[0-9]+\\)?\\)% used"
                    plain)
                   (setq remaining
                         (round (- 100 (string-to-number
                                       (match-string 1 plain)))))))
          (let ((face (cond ((<= remaining 10) 'error)
                            ((<= remaining 30) 'warning)
                            (t 'success))))
            (propertize (format "%d%%%% wk" remaining)
                        'face face
                        'help-echo plain)))
         ((string-match "blocked \\(?:7d\\|1w\\|wk\\) until [^|]+" plain)
          (propertize (match-string 0 plain)
                      'face 'error
                      'help-echo plain)))))))

(defun bob/mentat-observational-memory-compaction-presentation (result)
  "Describe Observational Memory metadata in compaction RESULT."
  (let ((details (alist-get 'details result)))
    (when (equal "om.folded" (alist-get 'type details))
      (let ((observations (length (alist-get 'observations details)))
            (reflections (length (alist-get 'reflections details)))
            (full-fold (eq t (alist-get 'fullFold details))))
        (list
         :label "Observational memory compacted"
         :heading "Observational memory compaction"
         :inline-details
         (delq nil
               (list (and (> observations 0)
                          (format "%d observation%s"
                                  observations
                                  (if (= observations 1) "" "s")))
                     (and (> reflections 0)
                          (format "%d reflection%s"
                                  reflections
                                  (if (= reflections 1) "" "s")))
                     (and full-fold "full fold")))
         :inspection-details
         (delq nil
               (list (cons "Observations" (number-to-string observations))
                     (cons "Reflections" (number-to-string reflections))
                     (cons "Full fold" (if full-fold "yes" "no"))
                     (and (eq t (alist-get 'retainedBeyondRequestedCut details))
                          (cons "Unobserved history retained beyond requested cut"
                                "yes")))))))))

(use-package mentat
  :ensure nil
  :load-path "~/source/mentat"
  :demand t
  :custom
  (mentat-pi-directory nil)
  (mentat-diagnostic-capture-enabled t)
  (mentat-enabled-extensions
   '(check-elisp codex resolve-symlinks session-scripts web-search
                 worktree-skills observational-memory chrome-profile-bridge
                 agent-browser))
  (mentat-pi-disabled-tools nil)
  (mentat-emacs-advertised-libraries
   '((dash . "list-processing macros and functions")
     (s . "string manipulation")
     (f . "file and path manipulation")
     (ht . "hash-table helpers")
     (aio . "cooperative asynchronous workflows")
     (request . "HTTP requests")))
  (mentat-default-provider "openai-codex")
  (mentat-default-model "gpt-5.6-sol")
  (mentat-default-effort "high")
  (mentat-supervisor-instructions
   (concat
    "Act as Mentat's parent supervisor and orchestrate the work deliberately. "
    "For a small, focused, well-understood change with a clear test path, "
    "work directly and validate it. Delegate large, ambiguous, specialized, "
    "cross-system, or high-risk work to the relevant expert instead of "
    "guessing. Use explorer first when scope, architecture, or repository "
    "context is unclear. Use ci-check selectively after substantial changes, "
    "when validation is broad or slow, or when failures need isolated "
    "diagnosis. Use reviewer selectively for risky, public, protocol, or "
    "unfamiliar changes, or when review is requested; do not automatically "
    "review every implementation. Use ui-manual-qa only when browser-based "
    "UI behavior actually needs verification. Keep edits to a shared "
    "worktree sequential when multiple children would modify it. Every child "
    "prompt must be self-contained: state the objective, relevant files, "
    "constraints, and expected deliverable and validation. Reconcile child "
    "handoffs against the files, tests, and other concrete evidence; do not "
    "silently accept unsupported claims. If a handoff is missing or "
    "incomplete, recover by inspecting the current state or rerunning a "
    "targeted child rather than guessing. Preserve repository rules and keep "
    "role-specific detailed instructions confined to the selected child."))
  (mentat-pi-profiles
   '(("Work"
      :directory "~/.pi/agent"
      :disabled-tools ("agent_browser" "agent_browser_web_search"
                       "emacs_capture_screenshot"
                       "emacs_eval_elisp"
                       "emacs_eval_named_elisp"
                       "emacs_eval_async"
                       "emacs_run_process"
                       "emacs_elisp_search"
                       "emacs_elisp_get_symbol_data"
                       "emacs_elisp_info"))
     ("Private"
      :directory "~/.pi/agent-private"
      :disabled-tools ("agent_browser" "agent_browser_web_search"))
     ("Pure Emacs"
      :directory "~/.pi/agent-pure-emacs"
      :tools ("emacs_capture_screenshot"
              "emacs_eval_elisp"
              "emacs_eval_named_elisp"
              "emacs_eval_async"
              "emacs_run_process"
              "emacs_elisp_search"
              "emacs_elisp_get_symbol_data"
              "emacs_elisp_info"
              "hindsight_recall"
              "hindsight_remember"
              "hindsight_reflect"
              "recall"
              "exa_search"
              "jina_reader"
              "agent_browser")
      :disabled-tools ("grep" "find" "ls" "show_me"
                       "agent_browser" "agent_browser_web_search"))))
  (mentat-compaction-presentation-function
   #'bob/mentat-observational-memory-compaction-presentation)

  (mentat-enabled-models
   '("openai-codex/gpt-5.6-luna"
     "openai-codex/gpt-5.6-terra"
     "openai-codex/gpt-5.6-sol"
     "azure-openai-responses/gpt-5.6-luna"
     "azure-openai-responses/gpt-5.6-terra"
     "azure-openai-responses/gpt-5.6-sol"))

  (mentat-extension-command-bindings nil)
  (mentat-extension-menu-commands
   '(("C" "Chrome controls" "/chrome")))
  (mentat-mode-line-extra-functions
   '(bob/mentat-codex-weekly-usage))
  (mentat-prompt-extra-completion-at-point-functions nil)
  (mentat-prompt-extra-word-candidate-functions
   '(bob/mentat-prose-word-candidates))
  (mentat-prompt-word-candidate-score-function
   #'bob/mentat-prose-word-candidate-score)
  :config
  (mentat-reset-extensions)
  (mentat-define-extension check-elisp
    :source "/Users/bob/.pi/agent/extensions/src/check-elisp.ts"
    :tools (check_elisp))
  ;; Registered only for child sessions.  Main Mentat sessions load this
  ;; internally, so it must not be included in `mentat-enabled-extensions'.
  (mentat-define-extension mentat-emacs
    :source "/Users/bob/source/mentat/pi-extensions/src/mentat-emacs.ts"
    :tools (emacs_eval_elisp emacs_eval_named_elisp emacs_eval_async
                             emacs_run_process emacs_elisp_search
                             emacs_elisp_get_symbol_data emacs_elisp_info))
  (mentat-define-extension codex
    :source "/Users/bob/.pi/agent/extensions/src/codex/index.ts")
  (mentat-define-extension resolve-symlinks
    :source "/Users/bob/.pi/agent/extensions/src/resolve-symlinks.ts")
  (mentat-define-extension session-scripts
    :source "/Users/bob/.pi/agent/extensions/src/session-scripts.ts")
  (mentat-define-extension web-search
    :source "/Users/bob/.pi/agent/extensions/src/web-search/index.ts"
    :tools (exa_search jina_reader))
  (mentat-define-extension worktree-skills
    :source "/Users/bob/.pi/agent/extensions/src/worktree-skills.ts")
  (mentat-define-extension observational-memory
    :source "git:github.com/elpapi42/pi-observational-memory@ce9fc982b3a219a7839f07c9f4a3e054e81a2b21")
  (mentat-define-extension chrome-profile-bridge
    :source "/Users/bob/.pi/agent-private/npm/node_modules/pi-chrome/extensions/chrome-profile-bridge/index.ts")
  (mentat-define-extension agent-browser
    :source "/Users/bob/.pi/agent-private/npm/node_modules/pi-agent-browser-native/dist/extensions/agent-browser/index.js"
    :tools (agent_browser))

  (mentat-reset-subagent-definitions)

  (bob/mentat-define-subagent explorer
    :description "Read-only project investigation"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking medium
    :extensions (web-search)
    :tools (read grep find ls exa_search jina_reader)
    :concurrency 4)

  (bob/mentat-define-subagent reviewer
    :description "Read-only code review with validation commands"
    :model ("azure-openai-responses/gpt-5.6-sol" "openai-codex/gpt-5.6-sol")
    :thinking high
    :extensions (web-search)
    :tools (read bash grep find ls exa_search jina_reader)
    :concurrency 8)

  (bob/mentat-define-subagent ci-check
    :description "Run local CI checks and report failures"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking low
    :tools (read bash grep find ls))

  (bob/mentat-define-subagent worker
    :description "Implement one focused, verifiable change after the problem is understood; split broader work into separate runs."
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :tools (read bash edit write grep find ls))

  (bob/mentat-define-subagent elisp-expert
    :description "Expert Emacs Lisp implementation, debugging, design, review, and validation"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :extensions (check-elisp mentat-emacs)
    :tools (read bash edit write grep find ls check_elisp
                 emacs_eval_elisp emacs_eval_named_elisp emacs_eval_async
                 emacs_run_process emacs_elisp_search
                 emacs_elisp_get_symbol_data emacs_elisp_info)
    :concurrency 1
    :max-turns 50)

  (bob/mentat-define-subagent effect-ts-backend-expert
    :description "Expert Effect TypeScript backend implementation, debugging, design, review, and validation"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :extensions (web-search)
    :tools (read bash edit write grep find ls exa_search jina_reader)
    :concurrency 1
    :max-turns 50)

  (bob/mentat-define-subagent frontend-react-expert
    :description "Expert React frontend implementation, debugging, design, review, and validation"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :extensions (web-search agent-browser)
    :tools (read bash edit write grep find ls exa_search jina_reader agent_browser)
    :concurrency 1
    :max-turns 50)

  (bob/mentat-define-subagent ui-manual-qa
    :description "Test UI features in a web browser"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking medium
    :extensions (agent-browser)
    :tools (read grep find ls agent_browser))

  (remove-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines)
  (add-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines t))

(provide 'bob-mentat)

;;; bob-mentat.el ends here
