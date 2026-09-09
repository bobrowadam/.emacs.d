;;; github.el --- GitHub workflow tools for Mentat -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep simple GitHub actions in gh.  Coordinate inline reviews here.

;;; Code:

(require 'bob-github)
(require 'mentat-elisp-library)

(mentat--elisp-register-library
 'github 'user
 "GitHub workflow guidance: use gh directly for simple actions; use the inline-review helper for commit checks, line comments, and a review decision. Read the guide before choosing a workflow."
 "github.md")

(mentat-defun mentat-github-submit-inline-review
    (repository pull-number reviewed-commit event comments &key body directory)
  "Submit an authorized GitHub review with inline comments.
Read the github package guide first.  Refuse a changed PR head and pin the
review to REVIEWED-COMMIT.  COMMENTS is an array of objects with path, line,
body and optional side (LEFT or RIGHT, default RIGHT).  Use new-file line
numbers for RIGHT and old-file line numbers for LEFT, not diff positions.
EVENT is APPROVE, COMMENT or REQUEST_CHANGES.  BODY is required for the latter
two.  Return review metadata and comment links.  Never automatically retry a
failed or cancelled submission.  Use gh directly for simple GitHub actions."
  (:execution async
   :arguments ((repository "GitHub repository as owner/repo")
               (pull-number "PR number")
               (reviewed-commit "Full SHA of the commit reviewed")
               (event "Authorized review decision: APPROVE, COMMENT or REQUEST_CHANGES")
               (comments "Inline comments: objects with path, line, body and optional side")))
  (lambda (resolve reject on-cancel)
    (funcall on-cancel
             (bob/github-submit-inline-review
              repository pull-number reviewed-commit event comments
              resolve reject :body body :directory directory))))

(provide 'github)
;;; github.el ends here
