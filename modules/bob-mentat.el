;;; bob-mentat.el --- Mentat configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and small integrations for Mentat, the Emacs interface to Pi.

;;; Code:

(require 'ansi-color)

(defvar mentat--buffer-model-provider)
(declare-function mentat-extension-status "mentat" (extension))
(declare-function mentat-reset-subagent-definitions "mentat" ())
(declare-function mentat--register-subagent "mentat" (&rest args))
(declare-function mentat-refresh-mode-lines "mentat" ())

(defface bob/mentat-behavior-status-face
  '((t :inherit mentat-mode-line-profile-face))
  "Face for the active behavior in Mentat's mode line."
  :group 'faces)

(defun bob/mentat-behavior-status ()
  "Return the active behavior for Mentat's mode line."
  (when-let* ((status (mentat-extension-status "behaviors-mode")))
    (propertize status 'face 'bob/mentat-behavior-status-face)))

(defun bob/mentat-codex-weekly-usage ()
  "Return the Codex weekly allowance remaining for Mentat's mode line."
  (when (equal mentat--buffer-model-provider "openai-codex")
    (when-let* ((status (mentat-extension-status "codex"))
                (plain (ansi-color-filter-apply status)))
      (cond
       ((string-match
         "\\([0-9]+\\)% \\(?:7d\\|1w\\|wk\\)\\b" plain)
        (let* ((remaining (string-to-number (match-string 1 plain)))
               (face (cond ((<= remaining 10) 'error)
                           ((<= remaining 30) 'warning)
                           (t 'success))))
          (propertize (format "%d%%%% wk" remaining)
                      'face face
                      'help-echo plain)))
       ((string-match "blocked \\(?:7d\\|1w\\|wk\\) until [^|]+" plain)
        (propertize (match-string 0 plain)
                    'face 'error
                    'help-echo plain))))))

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
  :commands (mentat mentat-menu mentat-open mentat-prompt)
  :custom
  (mentat-pi-directory nil)
  (mentat-pi-extensions
   '("/Users/bob/.pi/agent/extensions/src/behaviors/index.ts"
     "/Users/bob/.pi/agent/extensions/src/check-elisp.ts"
     "/Users/bob/.pi/agent/extensions/src/emacs/index.ts"
     "/Users/bob/.pi/agent/extensions/src/codex/index.ts"
     "/Users/bob/.pi/agent/extensions/src/resolve-symlinks.ts"
     "/Users/bob/.pi/agent/extensions/src/session-scripts.ts"
     "/Users/bob/.pi/agent/extensions/src/web-search/index.ts"
     "/Users/bob/.pi/agent/extensions/src/worktree-skills.ts"
     "/Users/bob/source/pi-observational-memory/src/index.ts"
     "/Users/bob/.pi/agent-private/npm/node_modules/pi-chrome/extensions/chrome-profile-bridge/index.ts"
     "/Users/bob/.pi/agent-private/npm/node_modules/@plannotator/pi-extension/index.ts"
     "/Users/bob/.pi/agent-private/npm/node_modules/pi-agent-browser-native/dist/extensions/agent-browser/index.js"))
  (mentat-pi-disabled-tools
   '("agent_browser" "agent_browser_web_search"))
  (mentat-default-provider "openai-codex")
  (mentat-default-model "gpt-5.6-luna")
  (mentat-default-effort "high")
  (mentat-pi-profiles
   '(("Work" . "~/.pi/agent")
     ("Private" . "~/.pi/agent-private")))
  (mentat-compaction-presentation-function
   #'bob/mentat-observational-memory-compaction-presentation)
  (mentat-enabled-models
   '("openai-codex/gpt-5.6-sol"
     "openai-codex/gpt-5.6-terra"
     "openai-codex/gpt-5.6-luna"
     "openai/gpt-5.6-terra"
     "openai/gpt-5.6-sol"
     "openai/gpt-5.6-luna"))
  (mentat-extension-command-bindings
   '(("C-M-i" . "/cycle-mode")))
  (mentat-extension-menu-commands
   '(("D" "Compare rewrite" "/mode-debug")
     ("A" "Annotate last response" "/plannotator-last")))
  (mentat-extension-progress-presentations
   '(("behaviors-rewrite" "running" "Rewriting")))
  (mentat-mode-line-extra-functions
   '(bob/mentat-codex-weekly-usage
     bob/mentat-behavior-status))
  (mentat-prompt-extra-completion-at-point-functions nil)
  (mentat-prompt-extra-word-candidate-functions
   '(bob/mentat-prose-word-candidates))
  (mentat-prompt-word-candidate-score-function
   #'bob/mentat-prose-word-candidate-score)
  :config
  (mentat-reset-subagent-definitions)

  (mentat-define-subagent explorer
    :description "Read-only project investigation"
    :instructions "Investigate the requested project area. Do not modify files or system state. Find relevant code, explain behavior, and report precise evidence with file locations."
    :model ("openai-codex/gpt-5.6-luna" "openai/gpt-5.6-luna")
    :thinking medium
    :tools (read grep find ls))

  (mentat-define-subagent reviewer
    :description "Read-only code review with validation commands"
    :instructions "Review the requested code change for correctness, security, performance, maintainability, and important test gaps.
Do not modify files. Use Bash only for non-mutating inspection and validation commands.
Return only actionable findings, or state that there are no findings.
Avoid overly defensive, over engineered, or unnecessarily complex suggestions.
For each suggestion, weigh the risk it prevents against the complexity it adds."
    :model ("openai-codex/gpt-5.6-sol" "openai/gpt-5.6-sol")
    :thinking high
    :tools (read bash grep find ls))

  (mentat-define-subagent ci-check
    :description "Run local CI checks and report failures"
    :instructions "Run the repository's applicable validation commands. Do not modify source files, install dependencies, or repair failures. Report each command, its result, and concise failure evidence."
    :model ("openai-codex/gpt-5.6-luna" "openai/gpt-5.6-luna")
    :thinking medium
    :tools (read bash grep find ls))

  (mentat-define-subagent worker
    :description "Implement focused changes and validate them"
    :instructions "Implement the delegated change in the current worktree. Keep edits focused, follow repository instructions, run applicable checks, and report changed files and verification results."
    :model ("openai-codex/gpt-5.6-luna" "openai/gpt-5.6-luna")
    :thinking high
    :tools (read bash edit write grep find ls))

  (remove-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines)
  (add-hook 'doom-modeline-mode-hook #'mentat-refresh-mode-lines t)
  :bind (("C-c C-;" . mentat-menu)))

(provide 'bob-mentat)

;;; bob-mentat.el ends here
