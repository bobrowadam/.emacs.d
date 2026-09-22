;;; bob-mentat.el --- Mentat configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and small integrations for Mentat, the Emacs interface to Pi.

;;; Code:

(require 'ansi-color)
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
  :load-path "~/source/mentat"
  :commands (mentat mentat-menu)
  :bind ("C-c C-;" . mentat-menu)
  :custom
  (mentat-tool-default-display-state 'summary)
  (mentat-streaming-tool-display-state 'expanded)
  (mentat-pi-directory (expand-file-name "~/.pi/agent"))
  (mentat-diagnostic-capture-enabled t)
  (mentat-enabled-extensions
   '(codex worktree-skills observational-memory))
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
  (mentat-default-provider "azure-openai-responses")
  (mentat-default-model "gpt-5.6-sol")
  (mentat-default-effort "low")
  (mentat-elisp-library-directory
   (expand-file-name "modules/mentat/mentat-tools/" user-emacs-directory))
  (mentat-supervisor-instructions
   (concat
    "Use the parent session for implementation by default.\n"
    "Work directly on small, routine, or well-understood changes and run "
    "their validation directly.\n"
    "Do not delegate merely because a matching subagent role exists. "
    "Delegate only when a child provides a concrete advantage, such as:\n"
    "- Substantial or parallel read-only investigation\n"
    "- Specialist expertise\n"
    "- A large, isolated implementation whose approach is already understood\n"
    "- Independent review of material risk\n"
    "- Long-running validation\n"
    "- Manual UI testing\n\n"
    "Keep framing, design decisions, reconciliation, and the final answer in "
    "the parent session. Keep shared-worktree edits sequential. When "
    "delegating, give the child a self-contained objective, relevant files, "
    "constraints, expected deliverable, and validation requirements. Verify "
    "the handoff against concrete evidence."))

  (mentat-pi-profiles
   '(("Work"
      :directory "~/.pi/agent"
      :subagents (explorer pr-reviewer reviewer worker ci-watcher ui-manual-qa)
      :tools ("emacs_eval_elisp"
              "emacs_eval_async"
              "emacs_elisp_call"
              "hindsight_recall"
              "hindsight_remember"
              "hindsight_reflect"
              "subagent"
              "recall")
      :disabled-tools ("grep" "find" "ls"))
     ("Private"
      :directory "~/.pi/agent-private"
      :subagents (explorer pr-reviewer reviewer worker ci-watcher ui-manual-qa)
      :tools ("emacs_eval_elisp"
              "emacs_eval_async"
              "emacs_elisp_call"
              "hindsight_recall"
              "hindsight_remember"
              "hindsight_reflect"
              "subagent"
              "recall")
      :disabled-tools ("grep" "find" "ls"))))
  (mentat-compaction-presentation-function
   #'bob/mentat-observational-memory-compaction-presentation)

  (mentat-enabled-models
   '("azure-openai-responses/gpt-5.6-luna"
     "azure-openai-responses/gpt-5.6-terra"
     "azure-openai-responses/gpt-5.6-sol"
     "azure-openai-responses/gpt-6-astra"
     "openai-codex/gpt-5.6-luna"
     "openai-codex/gpt-5.6-terra"
     "openai-codex/gpt-5.6-sol"
     "openai-codex/gpt-6-astra"))
  (mentat-extension-menu-commands
   '(("C" "Codex controls" bob/mentat-codex-menu)))
  (mentat-mode-line-extra-functions
   '(bob/mentat-codex-weekly-usage))
  (mentat-prompt-extra-completion-at-point-functions nil)
  (mentat-prompt-extra-word-candidate-functions
   '(bob/mentat-prose-word-candidates))
  (mentat-prompt-word-candidate-score-function
   #'bob/mentat-prose-word-candidate-score)
  :config
  (mentat-reset-extensions)
  ;; Registered only for child sessions.  Main Mentat sessions load this
  ;; internally, so it must not be included in `mentat-enabled-extensions'.
  (mentat-define-extension mentat-emacs
    :source "/Users/bob/source/mentat/pi-extensions/src/mentat-emacs.ts"
    :tools (emacs_eval_elisp emacs_eval_async emacs_elisp_call))
  (mentat-define-extension codex
    :source "/Users/bob/.pi/agent/extensions/src/codex/index.ts")
  (mentat-define-extension worktree-skills
    :source "/Users/bob/.pi/agent/extensions/src/worktree-skills.ts")
  (mentat-define-extension observational-memory
    :source "npm:pi-observational-memory@3.1.1")

  (mentat-reset-subagent-definitions)

  (bob/mentat-define-subagent explorer
      (bob/mentat-load-agent-and-common-instructions "explorer")
    :description "Read-only project investigation"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 4)

  (bob/mentat-define-subagent reviewer
      (bob/mentat-load-agent-and-common-instructions "reviewer")
    :description "Review one code change and optionally run read-only validation"
    :model ("azure-openai-responses/gpt-6-astra" "openai-codex/gpt-6-astra")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 8)

  (bob/mentat-define-subagent pr-reviewer
      (bob/mentat-load-agent-and-common-instructions "pr-reviewer")
    :description "Review one assigned PR slice using the parallel-review finding format"
    :model ("azure-openai-responses/gpt-6-astra" "openai-codex/gpt-6-astra")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 8)

  (bob/mentat-define-subagent ci-watcher
      (bob/mentat-load-agent-and-common-instructions "ci-watcher")
    :description "Run and monitor project validation without changing files"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking low
    :extensions (mentat-emacs))

  (bob/mentat-define-subagent worker
      (bob/mentat-load-agent-and-common-instructions "worker")
    :description "Implement one bounded, well-understood delegated change"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :extensions (mentat-emacs))

  (bob/mentat-define-subagent effect-ts-backend-expert
      (bob/mentat-load-agent-and-common-instructions
       "effect-ts-backend-expert")
    :description "Handle one bounded Effect TypeScript task requiring specialist expertise"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 1
    :max-turns 50)

  (bob/mentat-define-subagent frontend-react-expert
      (bob/mentat-load-agent-and-common-instructions
       "frontend-react-expert")
    :description "Handle one bounded React frontend task requiring specialist expertise"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking high
    :extensions (mentat-emacs)
    :concurrency 1
    :max-turns 50)

  (bob/mentat-define-subagent ui-manual-qa
      (bob/mentat-load-agent-and-common-instructions "ui-manual-qa")
    :description "Test UI features in a web browser"
    :model ("azure-openai-responses/gpt-5.6-luna" "openai-codex/gpt-5.6-luna")
    :thinking medium
    :extensions (mentat-emacs))

  (remove-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines)
  (add-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines t))

(provide 'bob-mentat)

;;; bob-mentat.el ends here
