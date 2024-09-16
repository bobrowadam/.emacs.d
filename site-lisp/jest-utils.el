;;;###autoload
(defun bob/jest-run-integration-tests ()
  (interactive)
  (async-shell-command (jest-integation-command)
                       "*jest-integration-tests"))

(defun jest-integation-command (&optional test-file-name-and-pattern)
  (if-let ((jest-config (locate-dominating-file "./" "jest.config.ts")))
      (s-trim-right (format
                     "IN_MEMORY_DB=true node --inspect  ~/source/grain/node_modules/.bin/jest  --runInBand --detectOpenHandles --config %sjest.config.ts %s"
                     jest-config
                     (or (car test-file-name-and-pattern) "")))))

(jest-integation-command)

