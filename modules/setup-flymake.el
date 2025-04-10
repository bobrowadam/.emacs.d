;;; setup-flymake.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;; commentary

;;; Code:

(defun bob/elisp-function-naming-convention-δ (report-fn &rest _args)
  "Use REPORT-FN to create a naming convention flymake rule."
  (save-excursion
    (goto-char (point-min))
    (let ((diagnostics nil)
          (pure-suffix "𝝺")
          (impure-suffix "δ"))
      ;; Find all defun and defun-like forms
      (while (re-search-forward "(\\(cl-\\)?def\\(un\\|subst\\|macro\\|advice\\)\\s-+\\([^[:space:]\n]+\\)" nil t)
        (let* ((func-name (match-string 3))
               (has-pure-suffix (string-suffix-p pure-suffix func-name))
               (has-impure-suffix (string-suffix-p impure-suffix func-name)))

          (unless (or has-pure-suffix has-impure-suffix)
            (let* ((pos (match-beginning 3))
                   (diag (flymake-make-diagnostic
                          (current-buffer)
                          pos
                          (+ pos (length func-name))
                          :warning
                          (format "Function '%s' should end with '%s' (pure) or '%s' (impure/side-effecting)"
                                  func-name pure-suffix impure-suffix))))
              (push diag diagnostics)))))
      (funcall report-fn (nreverse diagnostics)))))

;;;###autoload
(defun bob/elisp-flymake-setup-δ ()
  "Setup flymake in `emacs-lisp-mode'."
  (add-hook 'flymake-diagnostic-functions #'bob/elisp-function-naming-convention-δ nil t)
  (unless
      (equal (-some-> (buffer-file-name) file-name-nondirectory)
             "init.el")
    (flymake-mode t)))

(provide 'setup-flymake)
;;; setup-flymake.el ends here
