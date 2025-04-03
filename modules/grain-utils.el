(defun bob/generate--run-service-command (service-name &optional env inspect-port)
  (format "NODE_ENV=%s TS_NODE_PROJECT='./apps/backend/%s/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect%s -r ts-node/register -r tsconfig-paths/register ./apps/backend/%s/src/index.ts\""
          (or env "")
          service-name
          (if inspect-port (format "=%s" inspect-port) "")
          service-name))

(defun bob/generate--run-all-services-command (excluded-service-name)
  (format "npx nx run-many --target=start --parallel=20 --exclude=%s"
          excluded-service-name))

(defun bob/generate--run-all-services-e2e-command (excluded-service-name)
  (format "npm run start:services:test -- --exclude %s"
          excluded-service-name))

(defconst *grain-project-root* "~/source/grain")
(defconst *grain-services-root* "apps/backend/")

(defun grain/run--service (run-service-command service-output-buffer-name)
  "Run a service in debug mode and run the other services separately"
  (let ((default-directory *grain-project-root*)
        (process (get-buffer-process (get-buffer service-output-buffer-name)))
        (service-process-buffer (get-buffer-process (get-buffer service-output-buffer-name))))
    (when service-process-buffer
      (progn (interrupt-process service-process-buffer)
             (kill-buffer (process-buffer process))))

    (async-shell-command run-service-command service-output-buffer-name)))

(defun grain/get--service-name ()
  (completing-read "Enter service name: "
                   (directory-files (file-name-concat *grain-project-root* *grain-services-root*)
                                    nil
                                    "^[^.]")))
;;;###autoload
(defun grain/run-service (single-service)
  "Run a service in debug mode and run the other services separately"
  (interactive "P")
  (let ((service-name (grain/get--service-name)))
    (progn
      (grain/run--service (bob/generate--run-service-command service-name "" 9230)
                          (format "*SERVICE: %s*" service-name))
      (unless single-service
        (grain/run--service (bob/generate--run-all-services-command service-name)
                            (format "*[ALL] except %s*"
                                    service-name))))))

;;;###autoload
(defun grain/run-e2e (single-service)
  "Run a service in debug mode and run the other services separately."
  (interactive "P")
  (let ((service-name (grain/get--service-name)))
    (progn
      (grain/run--service (bob/generate--run-service-command service-name
                                                             "test"
                                                             9232)
                          (format "*SERVICE: %s*" service-name))
      (grain/run--service (bob/generate--run-all-services-e2e-command service-name)
                          (format "*[ALL] except %s*"
                                  service-name)))))

(defun get-inspected-node-processes ()
  (-flatten-n 1 (mapcar 'find-port-and-service-name-from-process
                        (process-list))))

(defun find-port-and-service-name-from-process (proc)
  (mapcar (𝝺 when
                  (string-match (rx (and 
                                     "./apps/backend/" (group (one-or-more (not (any "/" ".")))) "/"
                                     (zero-or-more anything)
                                     "node --inspect=" (group (one-or-more digit))))
                                %)
                  (let ((service-name (match-string 1 %))
                        (port (match-string 2 %)))
                    (list service-name port)))
               (process-command proc)))

(ert-deftest generate-command ()
  (should (equal (bob/generate--run-service-command "mail-service") "NODE_ENV= TS_NODE_PROJECT='./apps/backend/mail-service/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect -r ts-node/register -r tsconfig-paths/register ./apps/backend/mail-service/src/index.ts\"")))

;;;###autoload
(defun debug-migration ()
  (interactive)
  (let ((runOrRevert (completing-read "Command: " '("run" "revert")))
        (default-directory "/Users/bob/source/grain/packages/rdb/"))
    (async-shell-command (format "node --inspect --require ts-node/register ../../node_modules/typeorm/cli.js migration:%s -d src/data-source.ts"
                                 runOrRevert)
                         "*migration-shell*")))

(provide 'grain-utils)
