;;; bob-auth-process.el --- Private authenticated processes -*- lexical-binding: t; -*-

;;; Commentary:
;; Launch local processes with credentials from auth-source.  No Mentat dependency.
;; Callers own output handling, stdin, and process cancellation.  Do not run
;; commands that print credentials: process output is not secret-filtered.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)

(cl-defun bob/start-process-with-credentials
    (program args credentials &key directory buffer filter sentinel)
  "Start PROGRAM with ARGS and CREDENTIALS; return its process.
CREDENTIALS is a list of (ENV HOST USER) string triples.  Resolve every
secret through auth-source before launching, without changing Emacs's
process environment.  DIRECTORY defaults to `default-directory' and must
be local.  BUFFER, FILTER, and SENTINEL follow `make-process'.

The child uses a pipe with combined stdout and stderr.  The caller must
close stdin with `process-send-eof' when done, and may cancel with
`delete-process'.  Lookup and launch errors do not include secret values."
  (unless (and (stringp program) (not (string-empty-p program))
               (proper-list-p args) (cl-every #'stringp args))
    (user-error "Expected a program and a list of string arguments"))
  (unless (and (proper-list-p credentials) credentials)
    (user-error "Expected credential mappings (ENV HOST USER)"))
  (let (names)
    (dolist (entry credentials)
      (unless (and (proper-list-p entry) (= (length entry) 3)
                   (cl-every (lambda (s) (and (stringp s)
                                             (not (string-empty-p s))))
                             entry)
                   (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" (car entry))
                   (not (member (car entry) names)))
        (user-error "Expected unique environment names and nonempty HOST/USER selectors"))
      (push (car entry) names)))
  (let ((working-directory (expand-file-name (or directory default-directory))))
    (when (or (file-remote-p working-directory)
              (not (file-directory-p working-directory)))
      (user-error "Credential process directory must be an existing local directory"))
    ;; Keep backend errors and resolved secrets inside this boundary.
    (or (condition-case nil
            (let ((process-environment (copy-sequence process-environment))
                  (default-directory working-directory)
                  (auth-source-debug nil)
                  (debug-on-error nil))
              (dolist (entry credentials)
                (let ((secret (auth-source-pick-first-password
                               :host (nth 1 entry) :user (nth 2 entry)
                               :create nil)))
                  (unless (and (stringp secret) (not (string-empty-p secret))
                               (not (string-search "\0" secret)))
                    (error "Credential unavailable"))
                  (setenv (car entry) secret)))
              (make-process :name "authenticated-process"
                            :command (cons program args)
                            :connection-type 'pipe :noquery t
                            :buffer buffer :filter filter :sentinel sentinel))
          (error nil))
        (user-error "Credential lookup or process launch failed"))))

(provide 'bob-auth-process)
;;; bob-auth-process.el ends here
