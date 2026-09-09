;;; reference-writing.el --- AAI reference-writing guidance for Mentat -*- lexical-binding: t; -*-

;;; Commentary:
;; Advertise Answer.AI's reference-writing guidance through Mentat's Emacs
;; capability catalog.

;;; Code:

(require 'mentat-elisp-library)

(mentat--elisp-register-library
 'reference-writing 'user
 "AAI reference-writing guidance. Read before writing docstrings, READMEs, API docs, PR descriptions, commit messages, or substantive explanations to the user."
 "reference-writing.md")

(provide 'reference-writing)
;;; reference-writing.el ends here
