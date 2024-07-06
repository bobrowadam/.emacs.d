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
(provide 'npm-utils)
