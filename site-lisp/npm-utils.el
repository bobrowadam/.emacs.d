;; -*- lexical-binding: t; -*-

(use-package promise)

(defun bob/sleep-async-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (let ((buffer-content (buffer-string))
          (current-project (project-root (project-current))))
      (unwind-protect (condition-case err
                          (when (and (bob/node-modules-are-not-updated-p
                                      (json-parse-string buffer-content
                                                         :object-type 'alist))
                                     (y-or-n-p "node_modules are *not* up to date, Do you want to run `\"npm install\""))
                            (npm-install-project nil t))
                        (error
                         (message "Error in npm-outdated-sentinel: %s. buffer-content: %s" err buffer-content)))
        (kill-buffer (current-buffer))))))

(defun bob/npm-list-module-async (module-name)
  ())

(provide 'npm-utils)
