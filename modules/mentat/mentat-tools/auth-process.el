;;; auth-process.el --- Mentat authenticated process adapter -*- lexical-binding: t; -*-

;;; Commentary:
;; Agent entry point for the independent private bob-auth-process module.

;;; Code:

(require 'bob-auth-process)
(require 'mentat-emacs)
(require 'mentat-elisp-library)

(mentat--elisp-register-library
 'auth-process 'user
 "Run commands with auth-source credentials via mentat-run-process-with-credentials; pass environment, host, and user selectors, never secret values.")

(mentat-defun mentat-run-process-with-credentials
    (program args credentials &key directory)
  "Run PROGRAM with ARGS and auth-source CREDENTIALS asynchronously.
CREDENTIALS is a list of (ENV HOST USER) string triples, not secret values.
DIRECTORY is an optional local working directory.  Return bounded combined
output and exit status.  Never use this to print or inspect credentials;
child output is not secret-filtered.  Project authentication takes precedence."
  (:execution async :display "Run Authenticated Process")
  (lambda (resolve reject on-cancel &optional progress)
    (let ((output "") process finished)
      (cl-labels
          ((cancel ()
             (setq finished t)
             (when (process-live-p process)
               (delete-process process)))
           (collect (_process chunk)
             (unless finished
               (let ((next (mentat--utf8-truncate
                            (concat output chunk)
                            mentat--async-process-output-limit
                            "… [output truncated]")))
                 (unless (equal next output)
                   (setq output next)
                   (when progress (funcall progress output))))))
           (finish (child _event)
             (when (and (not finished)
                        (memq (process-status child) '(exit signal)))
               (setq finished t)
               (funcall resolve
                        (mentat--make-registered-process-result
                         :exit-code (process-exit-status child)
                         :output output)))))
        (funcall on-cancel #'cancel)
        (condition-case err
            (progn
              (setq process
                    (bob/start-process-with-credentials
                     program args credentials :directory directory
                     :filter #'collect :sentinel #'finish))
              (process-send-eof process))
          (error
           (unless finished
             (cancel)
             (funcall reject (error-message-string err)))))))))

(provide 'auth-process)
;;; auth-process.el ends here
