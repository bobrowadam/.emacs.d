(defcustom ammonite-executable-path "/usr/local/bin/amm"
  "Path for ammonite executable.")

(defvar ammonite-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-ammonite'")

(defvar amm-buffer-name "*ammonite*")

(define-derived-mode ammonite-mode comint-mode "AMM"
  "Major mode for `ammonite'.
\\<ammonite-mode-map>"
  nil "ammonite"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp ammonite-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (use-local-map ammonite-mode-map))

(defvar ammonite-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-ammonite'.")
(defvar amm-shell-prompt "\n@")

(defun amm--initialize ()
  "Helper function to initialize ammonite"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(defun run-ammonite ()
  "Run an inferior instance of `ammonite` inside Emacs"
  (interactive)
  (let* ((ammonite-program ammonite-executable-path)
         (buffer (comint-check-proc amm-buffer-name)))
    (pop-to-buffer-same-window
     (if (or buffer 
             (not (derived-mode-p 'ammonite-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer amm-buffer-name)))
     (current-buffer))
    (unless buffer
      (make-comint-in-buffer "ammonite" buffer ammonite-program))
    (ammonite-mode)))

(provide 'ammonite-mode)
