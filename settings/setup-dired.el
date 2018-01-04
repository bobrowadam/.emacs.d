(use-package dired
  :init
  ;; Move files between split panes
  (setq dired-dwim-target t)

  ;; C-a is nicer in dired if it moves back to start of files
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 1)))

  ;; M-up is nicer in dired if it moves to the fourth line - the first file
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (diredp-next-line 2))

  ;; M-down is nicer in dired if it moves to the last file
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  
  :config
  ;; Make dired less verbose
  (use-package dired+
    :ensure t
    :config ())

  (use-package dired-details+
    :ensure t
    :init (setq-default dired-details-hidden-string "|--- ")
    :config (dired-details-install))

  ;; Reload dired after making changes
  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))

  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key dired-mode-map (kbd "k") 'dired-do-delete)

  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)

  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)
  (diredp-toggle-find-file-reuse-dir 1))


;; Delete with C-x C-k to match file buffers and magit
;; (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

;; (eval-after-load "wdired"
;;   '(progn
;;      (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
;;      (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
;;      (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

(provide 'setup-dired)
