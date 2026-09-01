;;; file-list.el --- Compact recursive file listing -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'mentat-elisp-library)

(defconst mentat-list-files--output-limit (* 1024 1024)
  "Maximum file-list process output retained before rejection.")

(mentat-defun mentat-list-files
    (&key directory glob (max-results 200))
  "List files recursively as compact relative paths.
DIRECTORY defaults to `default-directory'.  GLOB is an optional ripgrep glob.
Return at most MAX-RESULTS paths while respecting ignore files."
  (:execution async :display "List Files")
  (let* ((base (file-name-as-directory
                (expand-file-name (or directory default-directory))))
         (limit (min 1000 (max 1 max-results)))
         (command
          (append
           (list (or (executable-find "rg")
                     (user-error "ripgrep is required for file listing"))
                 "--files" "--hidden" "--glob" "!.git")
           (when glob (list "--glob" glob)))))
    (unless (file-directory-p base)
      (user-error "Not a directory: %s" base))
    (lambda (resolve reject on-cancel)
      (let ((pending "")
            (paths nil)
            (output-bytes 0)
            (limited nil)
            (completed nil)
            (error-buffer (generate-new-buffer " *mentat-list-files-error*"))
            process)
        (cl-labels
            ((cleanup
              ()
              (when (buffer-live-p error-buffer)
                (kill-buffer error-buffer)))
             (reject-once
              (reason)
              (unless completed
                (setq completed t)
                (cleanup)
                (funcall reject reason)))
             (resolve-once
              ()
              (unless completed
                (setq completed t)
                (let* ((ordered (nreverse paths))
                       (body (if ordered (string-join ordered "\n") "No files."))
                       (result
                        (if limited
                            (concat body "\n\n"
                                    (format "[Showing first %d files.]" limit))
                          body)))
                  (cleanup)
                  (funcall resolve result))))
             (consume-line
              (line)
              (unless (or limited (string-empty-p line))
                (push line paths)
                (when (>= (length paths) limit)
                  (setq limited t)
                  (when (process-live-p process)
                    (delete-process process)))))
             (filter-output
              (_process chunk)
              (unless completed
                (setq output-bytes (+ output-bytes (string-bytes chunk)))
                (if (> output-bytes mentat-list-files--output-limit)
                    (progn
                      (reject-once "File listing exceeded the 1 MiB output limit")
                      (when (process-live-p process) (delete-process process)))
                  (setq pending (concat pending chunk))
                  (let ((start 0))
                    (while (string-match "\n" pending start)
                      (consume-line
                       (substring pending start (match-beginning 0)))
                      (setq start (match-end 0)))
                    (setq pending (substring pending start))))))
             (finish-process
              (finished-process _event)
              (unless completed
                (unless (string-empty-p pending)
                  (consume-line pending)
                  (setq pending ""))
                (let ((status (process-exit-status finished-process)))
                  (if (or limited (memq status '(0 1)))
                      (resolve-once)
                    (let ((diagnostic
                           (with-current-buffer error-buffer
                             (string-trim (buffer-string)))))
                      (reject-once
                       (if (string-empty-p diagnostic)
                           (format "File listing failed with exit status %s" status)
                         (format "File listing failed: %s" diagnostic)))))))))
          (condition-case err
              (setq process
                    (let ((default-directory base))
                      (make-process
                       :name "mentat-list-files"
                       :command command
                       :connection-type 'pipe
                       :coding '(utf-8-unix . utf-8-unix)
                       :noquery t
                       :buffer nil
                       :stderr error-buffer
                       :filter #'filter-output
                       :sentinel #'finish-process)))
            (error (reject-once err)))
          (funcall on-cancel
                   (lambda ()
                     (setq completed t)
                     (when (process-live-p process)
                       (delete-process process))
                     (cleanup))))))))

(provide 'file-list)
;;; file-list.el ends here
