;;; ob-bun.el --- Org-Babel support for Bun (JavaScript/TypeScript runtime)

;; Copyright (C) 2023 Bob

;; Author: Bob
;; Keywords: babel, bun, javascript, typescript
;; Version: 0.1.0

;;; Commentary:

;; Org-Babel support for executing Bun (JavaScript/TypeScript) code blocks.

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:bun
  '((:results . "output")
    (:cmd . "bun"))
  "Default header arguments for Bun code blocks.")

(defun org-babel-execute:bun (body params)
  "Execute a block of Bun code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((cmd (or (cdr (assq :cmd params)) "bun"))
         (full-body (org-babel-expand-body:generic body params))
         (script-file (org-babel-temp-file "bun-" ".js"))
         (cmd-line (format "%s run %s" cmd script-file)))
    
    ;; Write the full body to a temporary script file
    (with-temp-file script-file
      (insert full-body))
    
    ;; Execute the script and capture output
    (let ((results (org-babel-eval cmd-line "")))
      ;; Clean up the temporary script file
      (delete-file script-file)
      results)))

(defun org-babel-prep-session:bun (_session _params)
  "Prepare a Bun session.
Currently a no-op as we're running scripts directly."
  nil)

(defun org-babel-bun-initiate-session (&optional _session)
  "Initiate a Bun session.
Currently a no-op as we're running scripts directly."
  nil)

(provide 'ob-bun)
;;; ob-bun.el ends here