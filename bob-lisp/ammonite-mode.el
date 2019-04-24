(require 'scala-mode)
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
  "Major mode for `run-ammonite'.
\\<ammonite-mode-map>"
  nil "ammonite"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp ammonite-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
               'ammonite-completion-complete-at-point)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (use-local-map ammonite-mode-map))

(defvar ammonite-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-ammonite'.")

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

(defun ammonite-completion-complete-at-point ()
  "Perform completion at point in inferior-ammonite.
Most of this is borrowed from python.el"
  (let* ((start
          (save-excursion
            (with-syntax-table scala-syntax:syntax-table
              (let* ((syntax-list (append (string-to-syntax ".")
					  (string-to-syntax "_")
					  (string-to-syntax "w"))))
                (while (member
                        (car (syntax-after (1- (point)))) syntax-list)
                  (skip-syntax-backward ".w_")
                  (when (or (equal (char-before) ?\))
                            (equal (char-before) ?\"))
                    (forward-char -1)))
                (point)))))
         (end (point)))
    (list start end
          (completion-table-dynamic
           (apply-partially
            #'amm-complete-at-point)))))

(defun amm-complete-at-point (prefix)
  (if (equal prefix "")
      nil
    (amm-completing-read-at-point (split-string (ammonite-get-result-from-inf prefix) prefix))))

(defvar completion-buffer "")
(defun ammonite-get-result-from-inf (prefix)
  (let ((process (get-buffer-process amm-buffer-name))
        (comint-preoutput-filter-functions '(amm--shell-output-filter)))
    (comint-send-string process prefix)
    (comint-send-string process "\t")
    (accept-process-output process 2 nil t)
    (prog1 completion-buffer
      (setq completion-buffer ""))))

(defun amm--shell-output-filter (string)
  (let ((filterd-str (ansi-color-filter-apply string)))
       (setq completion-buffer (concat completion-buffer filterd-str " "))))

(defun amm-completing-read-at-point (arglist)
  (ido-completing-read+ "Completions:" arglist))

(seq-filter (lambda (n) (not (equal n "@"))) '("@" "1b"))

(provide 'ammonite-mode)
