;; -*- lexical-binding: t; -*-

(defun bob/read-json-file (file &optional dir)
  "Deserialize JSON from FILE at DIR or at the current project root"
  (when-let* ((project-root-path (or dir (project-root (project-current))))
              (file-string (read-file (format "%s/%s" project-root-path file))))
    (json-parse-string file-string
                       :object-type 'alist)))

(defun version--breakdown (version)
  "Breaks down version strings into numeric components,
 ignoring non-numeric tail components."
  (let ((clean-version (car (string-split (s-chop-prefix "^" version) "-"))))
    (mapcar 'string-to-number (string-split clean-version "\\."))))

(defun bob/package-version-meet-requirements (required-version current-version)
  (unless (if (not (s-prefix? "^" required-version))
              (equal current-version required-version)
            (cl-destructuring-bind ((required-major required-minor required-patch)
                                    (current-major current-minor current-patch))
                (list (version--breakdown required-version)
                      (version--breakdown current-version))
              (and (equal current-major required-major)
                   (>= current-minor required-minor))))
    (debug)))

(ert-deftest version-requirements ()
  (should (equal (bob/package-version-meet-requirements "7.51.2" "7.117.0") t))
  (should (equal (bob/package-version-meet-requirements "8.11.5" "8.11.5") t))
  (should (equal (bob/package-version-meet-requirements "8.10.5" "8.11.5") nil))
  (should (equal (bob/package-version-meet-requirements "7.10.5" "8.10.5") nil))
  (should (equal (bob/package-version-meet-requirements "^8.11.5" "8.11.5") t))
  (should (equal (bob/package-version-meet-requirements "^8.11.5" "8.11.8") t))
  (should (equal (bob/package-version-meet-requirements "^8.12.5" "8.11.5") nil))
  (should (equal (bob/package-version-meet-requirements "^5.1.5" "6.1.5") nil))
  (should (equal (bob/package-version-meet-requirements "^3.100.0" "3.598.0") t)))

(defun node--module-version (module-name)
  (assocdr 'version
           (bob/read-json-file (format "node_modules/%s/package.json" module-name))))

(defun compare--version-to-module (name-to-version)
  (let* ((module (car name-to-version))
        (version (cdr name-to-version))
        (node-module-version (node--module-version module)))
    (bob/package-version-meet-requirements version node-module-version)))

(defun bob/node-modules-are-not-updated-p ()
  (let ((package-json-versions (bob/read-json-file "package.json")))
    (not (-every 'identity
                 (mapcar
                  'compare--version-to-module
                  (seq-concatenate 'list
                                   (assocdr 'dependencies package-json-versions)
                                   (assocdr 'devdependencies package-json-versions)))))))

(defvar bob/update-node-modules-if-needed-on nil)
(defun bob/toggle-update-node-modules-if-needed ()
  (interactive)
  (setq bob/update-node-modules-if-needed-on
        (not bob/update-node-modules-if-needed-on)))

;;;###autoload
(defun bob/update-node-modules-if-needed-sync ()
  (interactive)
  (when bob/update-node-modules-if-needed-on
    (unwind-protect
        (condition-case err
            (when (and (time (bob/node-modules-are-not-updated-p))
                       (y-or-n-p "node_modules are *not* up to date, Do you want to run `\"npm install\""))
              ;; TODO allow using PNPM and bun as well
              (npm-install-project nil t))
          (error
           (message "Error in npm-outdated-sentinel: %s." err))))))

;; (ert-run-tests-batch 'version-requirements)
(setq ert-quiet nil)

(defun check-types-command ()
  "Returns the command for running check-types NPM script if available"
  (when-let* ((default-directory (locate-dominating-file default-directory "package.json"))
            (package-json-raw (read-file "package.json"))
            (package-json (json-parse-string package-json-raw
                                             :object-type 'alist)))
      (assocdr 'check-types (assocdr 'scripts package-json))))

;;;###autoload
(defun npm-run-build ()
  "Build typescript project on watch mode"
  (interactive)
  (if-let* ((default-directory (locate-dominating-file default-directory "package.json"))
            (local-check-types-command (check-types-command))
            (comint-scroll-to-bottom-on-input t)
            (comint-scroll-to-bottom-on-output t)
            (comint-process-echoes t)
            (compilation-buffer-name (format "TS-COMPILE -- %s"
                                             (get-dir-name default-directory))))
      (cond ((and (not (eq major-mode 'comint-mode))
                  (car (memq (get-buffer compilation-buffer-name)
                             (buffer-list))))
             (switch-to-buffer (get-buffer compilation-buffer-name)))
            ((and (eq major-mode 'comint-mode)
                  (s-contains? (buffer-name (current-buffer)) compilation-buffer-name))
             (switch-to-prev-buffer))
            ((s-starts-with-p "nx" local-check-types-command)
             (compilation-start "npm run check-types"
                                t (lambda (_)
                                    compilation-buffer-name)))
            (t
             (compilation-start "npm run check-types -- -w"
                                t (lambda (_)
                                    compilation-buffer-name))))
    (error "probably not a typescript application")))

(defun bob/npm--project-name ()
  "Get the current project name from the package json file."
  (when-let ((project-root-path (project-root (project-current)))
             (package-json (json-parse-string (read-file (format "%s/package.json" project-root-path))
                                              :object-type 'alist)))
    (assocdr 'name package-json)))

(defun npm-run (&optional normal-mode)
  "Debug typescript project on watch mode
NORMAL-MODE is for not running with debugger"
  (interactive "P")
  (when (check-types-command)
    (let ((default-directory (project-root (project-current t)))
          (comint-scroll-to-bottom-on-input t)
          (comint-scroll-to-bottom-on-output t)
          (comint-process-echoes t)
          (compilation-buffer-name (bob/compilation-buffer-name))
          (project-main-file (bob/npm--project-name)))
      (cond ((and (not (eq major-mode 'comint-mode))
                  (car (memq (get-buffer compilation-buffer-name)
                             (buffer-list))))
             (switch-to-buffer (get-buffer compilation-buffer-name)))
            ((and (eq major-mode 'comint-mode)
                  (s-contains? (buffer-name (current-buffer)) compilation-buffer-name))
             (switch-to-prev-buffer))
            (t
             (let ((compilation-command (if normal-mode
                                            (format "./node_modules/typescript/bin/tsc -w& nodemon -d 2 -w ./dist -r source-map-support/register ./dist/%s.js"
                                                    project-main-file)
                                          (format "./node_modules/typescript/bin/tsc -w& nodemon -d 2 --inspect=%s -w ./dist -r source-map-support/register ./dist/%s.js"
                                                  (get--available-inspect-port)
                                                  project-main-file))))
               (with-temporary-node-version
                   (fnm-current-node-version)
                 (compilation-start compilation-command
                                    t (lambda (mode)
                                        compilation-buffer-name)))))))))

(defun npm-install-project (&optional force quiet)
  "NPM install in project.
If FORCE is non-nil, delete the \"package-lock.json\" and \"node_modules\"
directories and verify NPM cache before running `npm install`."
  (interactive "P")
  (when (read-file "package.json")
   (let* ((default-directory (project-root (project-current t)))
          (buffer-name (format "*npm install %s*" default-directory)))
     (message "local NPM executable version is %s" (s-trim-right (shell-command-to-string "npm -v")))
     (when force
       (message "removing package-lock.json")
       (unwind-protect (delete-file (concat default-directory "package-lock.json")))
       (message "removing node_modules")
       (unwind-protect (delete-directory (concat default-directory "node_modules") t))
       (message "verifying NPM's cache")
       (apply #'call-process "node" nil 0 nil '("verify")))
     (with-temporary-node-version (fnm-current-node-version)
       (if (not quiet)
           (compilation-start "npm i")
         (make-process :name buffer-name
                       :buffer buffer-name
                       :command '("npm" "i" "--no-color")
                       :sentinel (process-generic-sentinel)))))))

(defun bob/npm-outdated-sentinel (start-time)
  "Sentinel for handling npm outdated process termination."
  (lambda (process event)
    (with-current-buffer (process-buffer process)
      (message (format "bob/npm-outdated-sentinel eval time is %.06f"
                       (float-time (time-since start-time))))
      (when (and (eq (process-exit-status process) 1)
                 (bob/npm-list-problems-p (buffer-string))
                 (y-or-n-p "node_modules are *not* up to date, Do you want to run `\"npm install\"")
                 (npm-install-project nil t)))
      (kill-buffer (current-buffer)))))

(defun bob/update-node-modules-if-needed (&optional root)
  "Check if node_modules should be updated by using \"npm list\"."
  (interactive "P")
  (when-let* ((default-directory (or root (and (project-current)
                                               (project-root (project-current)))))
              (is-node-project (read-file "package.json"))
              (buffer-name (format "*npm-list*: %s %s" default-directory (random 100000))))
    (with-temporary-node-version (fnm-current-node-version)
      (make-process :name buffer-name
                    :buffer buffer-name
                    :command '("npm" "list" "--no-color" "--depth=0")
                    :sentinel (bob/npm-outdated-sentinel (current-time))))))

(defun bob/npm-list-problems-p (npm-list-results)
  (seq-remove
   (lambda (str) (s-starts-with? "npm ERR! peer dep missing" str))
   (seq-filter (lambda (str)
                 (s-starts-with? "npm ERR!" str))
               (split-string npm-list-results "\n"))))
(provide 'npm-utils)
