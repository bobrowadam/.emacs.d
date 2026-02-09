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
    (:cmd . "bun")
    (:flags . "run"))
  "Default header arguments for Bun code blocks.")

(defvar org-babel-default-header-args:ts
  '((:results . "output")
    (:cmd . "bun")
    (:flags . "run"))
  "Default header arguments for TypeScript code blocks.")

(defun org-babel-expand-body:ts (body _params)
  "Expand the body of a TypeScript source block.
This is a simple pass-through, but allows for future expansion."
  body)

(defun org-babel-execute:bun (body params)
  "Execute a block of Bun (JavaScript) code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((cmd (or (cdr (assq :cmd params)) "bun"))
         (flags (or (cdr (assq :flags params)) "run"))
         (full-body (org-babel-expand-body:generic body params))
         (script-file (org-babel-temp-file "bun-" ".js"))
         (cmd-line (format "%s %s %s" cmd flags script-file)))
    
    ;; Write the full body to a temporary script file
    (with-temp-file script-file
      (insert full-body))
    
    ;; Execute the script and capture output
    (let ((results (org-babel-eval cmd-line "")))
      ;; Clean up the temporary script file
      (delete-file script-file)
      results)))

(defun org-babel-execute:ts (body params)
  "Execute a block of TypeScript code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((cmd (or (cdr (assq :cmd params)) "bun"))
         (flags (or (cdr (assq :flags params)) "run"))
         (full-body (org-babel-expand-body:ts body params))
         (script-file (org-babel-temp-file "bun-ts-" ".ts"))
         (cmd-line (format "%s %s %s" cmd flags script-file)))
    
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

(defun org-babel-prep-session:ts (_session _params)
  "Prepare a TypeScript session.
Currently a no-op as we're running scripts directly."
  nil)

(defun org-babel-bun-initiate-session (&optional _session)
  "Initiate a Bun session.
Currently a no-op as we're running scripts directly."
  nil)

(defun org-babel-ts-initiate-session (&optional _session)
  "Initiate a TypeScript session.
Currently a no-op as we're running scripts directly."
  nil)

(provide 'ob-bun)
;;; ob-bun.el ends here