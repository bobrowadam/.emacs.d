(defun bob/generate--run-service-command (service-name)
  (format "TS_NODE_PROJECT='./apps/backend/%s/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect -r ts-node/register -r tsconfig-paths/register ./apps/backend/%s/src/index.ts\""
          service-name service-name))

(defun run-service ()
  "Run a service in debug mode and run the other services separately"
  (interactive)
  (let* ((service-dir "/Users/bob/source/grain/apps/backend")
         (service-names (directory-files service-dir nil "^[^.]"))
         (service-name (completing-read "Enter service name: " service-names))
         (run-service-command (bob/generate--run-service-command service-name))
         (output-buffer-name (format "*%s* stdout" service-name))
         (default-directory "~/source/grain"))
    (async-shell-command run-service-command output-buffer-name)
    (async-shell-command (bob/generate--run-all-services-command service-name)
                         (format "*services except %s* stdout" service-name))))

(defun bob/generate--run-all-services-command (excluded-service-name)
  (format "npx nx run-many --target=start --parallel=20 --exclude=%s"
          excluded-service-name))

(ert-deftest generate-command ()
  (should (equal (bob/generate--run-service-command "mail-service") "TS_NODE_PROJECT='./apps/backend/mail-service/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect -r ts-node/register -r tsconfig-paths/register ./apps/backend/mail-service/src/index.ts\"")))

(provide 'grain-utils)
