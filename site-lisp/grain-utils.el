(defun bob/generate--run-service-command (service-name)
  (format "TS_NODE_PROJECT='./apps/backend/%s/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect -r ts-node/register -r tsconfig-paths/register ./apps/backend/%s/src/index.ts\""
          service-name service-name))

(defun bob/generate--run-all-services-command (excluded-service-name)
  (format "npx nx run-many --target=start --parallel=20 --exclude=%s"
          excluded-service-name))

(defconst grain-service-output-buffer-prefix "*grain/service*")
(defun grain/run-service ()
  "Run a service in debug mode and run the other services separately"
  (interactive)
  (let* ((default-directory "~/source/grain")
         (service-dir "/Users/bob/source/grain/apps/backend")
         (service-names (directory-files service-dir nil "^[^.]"))
         (service-name (completing-read "Enter service name: " service-names))
         (run-service-command (bob/generate--run-service-command service-name))
         (output-buffer-name (format "%s %s" grain-service-output-buffer-prefix service-name))
         (service-output-buffer-name (format "%s all but %s" grain-service-output-buffer-prefix service-name))
         (grain-service-active-processes (--filter
                                          (->> it process-buffer buffer-name (s-starts-with? grain-service-output-buffer-prefix))
                                          (process-list))))
    (dolist (process grain-service-active-processes)
      (interrupt-process process)
      (kill-buffer (process-buffer process)))

    (async-shell-command run-service-command output-buffer-name)
    (async-shell-command (bob/generate--run-all-services-command service-name)
                         service-output-buffer-name)))


(ert-deftest generate-command ()
  (should (equal (bob/generate--run-service-command "mail-service") "TS_NODE_PROJECT='./apps/backend/mail-service/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect -r ts-node/register -r tsconfig-paths/register ./apps/backend/mail-service/src/index.ts\"")))

(defun debug-migration ()
  (interactive)
  (let ((runOrRevert (completing-read "Command: " '("run" "revert")))
        (default-directory "/Users/bob/source/grain/packages/rdb/"))
    (async-shell-command (format "node --inspect --require ts-node/register ../../node_modules/typeorm/cli.js migration:%s -d src/data-source.ts"
                                 runOrRevert)
                         "*migration-shell*")))

(provide 'grain-utils)
