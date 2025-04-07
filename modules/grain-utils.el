;;; grain-utils.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;; commentary
;; Functions for running grain's services in local dev environment.

;;; Code:
(defun bob/generate--run-service-command-𝝺 (service-name &optional env inspect-port)
  "Generate a shell command to run SERVICE-NAME.
optionally, accept ENV and INSPECT-PORT arguments."
  (format "NODE_ENV=%s TS_NODE_PROJECT='./apps/backend/%s/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect%s -r ts-node/register -r tsconfig-paths/register ./apps/backend/%s/src/index.ts\""
          (or env "")
          service-name
          (if inspect-port (format "=%s" inspect-port) "")
          service-name))

(defun bob/generate--run-all-services-command-δ (excluded-service-name)
  "Run all off grain services except EXCLUDED-SERVICE-NAME."
  (format "npx nx run-many --target=start --parallel=20 --exclude=%s"
          excluded-service-name))

(defun bob/generate--run-all-services-e2e-command-δ (excluded-service-name)
  "Run all off grain services in e2e mode, except EXCLUDED-SERVICE-NAME."
  (format "npm run start:services:test -- --exclude %s"
          excluded-service-name))

(defconst *grain-project-root* "~/source/grain")
(defconst *grain-services-root* "apps/backend/")

(defun grain/run--serviceδ (run-service-command service-output-buffer-name)
  "Run RUN-SERVICE-COMMAND in SERVICE-OUTPUT-BUFFER-NAME buffer."
  (let ((default-directory *grain-project-root*)
        (process (get-buffer-process (get-buffer service-output-buffer-name))))
    (when process
      (interrupt-process process)
      (kill-buffer (process-buffer process)))
    (async-shell-command run-service-command service-output-buffer-name)))

(defun grain/get--service-nameδ ()
  "Use completing read to get a name of a known grain service."
  (completing-read "Enter service name: "
                   (directory-files (file-name-concat *grain-project-root* *grain-services-root*)
                                    nil
                                    "^[^.]")))
;;;###autoload
(defun grain/run-serviceδ (single-service-p)
  "Run a service in debug mode.
When SINGLE-SERVICE-P is nil, run all the other services as well."
  (interactive "P")
  (let* ((service-name (grain/get--service-nameδ))
         (service-output-buffer (format "*SERVICE: %s*" service-name))
         (all-services-output-buffer (format "*[ALL] except %s*" service-name)))
    (save-excursion (grain/run--serviceδ (bob/generate--run-service-command-𝝺 service-name "" 9230)
                          service-output-buffer))
    (unless single-service-p
      (grain/run--serviceδ (bob/generate--run-all-services-command-δ service-name)
                          all-services-output-buffer))
    (switch-to-buffer service-output-buffer)))

;;;###autoload
(defun grain/run-e2eδ (single-service-p)
  "Run a service in debug mode.
When SINGLE-SERVICE-P is nil, run all the other services as well."
  (interactive "P")
  (let ((service-name (grain/get--service-nameδ)))
    (progn
      (grain/run--serviceδ (bob/generate--run-service-command-𝝺 service-name
                                                             "test"
                                                             9232)
                          (format "*SERVICE: %s*" service-name))
      (grain/run--serviceδ (bob/generate--run-all-services-e2e-command-δ service-name)
                          (format "*[ALL] except %s*"
                                  service-name)))))

(defun pick-port-for-inspected-service-δ ()
  "Ask for a service and find it's debugging port for."
  (let ((inspected-services-map (get-inspected-node-processes-δ)))
   (-> (completing-read "Service to debug: "
                        inspected-services-map)
       (assocdr inspected-services-map)
       (or 9229))))

(defun get-inspected-node-processes-δ ()
  "Get node processes that is running using the --inspect flag."
  (-keep 'identity
         (-flatten-n 1 (mapcar 'find--port-and-service-name-from-process-δ
                               (process-list)))))

(defun find--port-and-service-name-from-process-δ (grain-service-proc)
  "Extract the service-name and port from GRAIN-SERVICE-PROC."
  (mapcar
   (𝝺 when
      (string-match (rx (and "./apps/backend/"
                             (group (one-or-more (not (any "/" "."))))
                             "/"
                             (zero-or-more anything)
                             "node --inspect=" (group (one-or-more digit))))
                    %)
      (let ((service-name (match-string 1 %))
            (port (match-string 2 %)))
        (cons service-name port)))
   (process-command grain-service-proc)))

(ert-deftest generate-command ()
  (should (equal (bob/generate--run-service-command-𝝺 "mail-service") "NODE_ENV= TS_NODE_PROJECT='./apps/backend/mail-service/tsconfig.app.json' TS_NODE_FILES=true nodemon --exec \"node --inspect -r ts-node/register -r tsconfig-paths/register ./apps/backend/mail-service/src/index.ts\"")))

;;;###autoload
(defun debug-migration-δ ()
  "Debug a grain migration script."
  (interactive)
  (let ((runOrRevert (completing-read "Command: " '("run" "revert")))
        (default-directory "/Users/bob/source/grain/packages/rdb/"))
    (async-shell-command (format "node --inspect --require ts-node/register ../../node_modules/typeorm/cli.js migration:%s -d src/data-source.ts"
                                 runOrRevert)
                         "*migration-shell*")))

(provide 'grain-utils)
;;; grain-utils.el ends here
