(use-package gptel
  :config
  (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                            car
                            (plist-get :secret)
                            funcall)))
    (gptel-make-anthropic
        "Claude"
      :stream t
      :key credentials))
  :custom
  (gptel-default-mode 'org-mode)
  :bind
  ("C-c g g" . gptel)
  ("C-c g s" . gptel-send)
  (:map gptel-mode-map ("C-c g s" . gptel-menu)))

(defun elysium-apply-code-changes--turn-on-smerge-mode (orig-fun &rest args)
  "Advise `elysium-apply-code-changes' to turn on smerge-mode."
  (let ((result (apply orig-fun args)))  ; Call the original function
    (smerge-mode 1)                       ; Turn on smerge-mode
    result))
(use-package elysium
  :commands (elysium-query elysium-toggle-window)
  :config
  (advice-add 'elysium-apply-code-changes :around #'elysium-apply-code-changes--turn-on-smerge-mode)
  :bind
  ("C-c g e" . elysium-query))

                              ; Return the result of the original function
(use-package chatgpt-shell
  :disabled t
  :commands (chatgpt-shell chatgpt-shell-start)
  :custom
  (chatgpt-shell-model-version 0)
  :config
  (setq chatgpt-shell-openai-key (exec-path-from-shell-copy-env "OPEN_AP_API_KEY"))
  (add-to-list 'chatgpt-shell-system-prompts
               '("Movie to TV Show" . "I want to split a movie into several “episodes” for a TV-like experience. Follow these steps:

1. Break the movie into episodes with varying lengths (they don't need to be the same).
2. For each episode:
   - Provide a title relevant to that part of the movie. (no spoilers)
   - Include the start and end time in seconds.
   - Total duration in minutes
3. Maintain the key plot points, while making sure each episode feels somewhat self-contained.
4. Don't add any information about the movie. No spoilers what so ever!
5. If you are not familiar with the movie just say so, don't give fake data, it's fine.

Here’s the example data structure output:
const episodes = [{ startSecond 0, endSecond: 1:  1740, title: \"Feather in the Wind\", totalDuration: 29 }]
  "))
  :hook
  (chatgpt-shell-mode . (lambda () (corfu-mode -1))))

(use-package claude-shell
  :disabled t
  :commands (claude-shel)
  :custom
  (claude-shell-streaming t)
  :config
  (setq claude-shell-api-token (exec-path-from-shell-copy-env "CLAUDE_SHELL_API_TOKEN"))
  :hook
  (claude-shell-mode . (lambda () (corfu-mode -1))))

(provide 'setup-llm)


