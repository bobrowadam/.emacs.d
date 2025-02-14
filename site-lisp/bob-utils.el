(defun assocdr (prop cons-cell)
  (cdr (assoc prop cons-cell)))

(defun get-dir-name (&optional path)
  "Get the current directly name on PATH."
  (file-name-nondirectory
     (directory-file-name
      (file-name-directory path))))

(defun bob/read-file-content (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(defun bob/drop-buffer (buffers)
  (seq-remove (lambda (buf)
                (cond ((equal buf (buffer-name)) t)))
              buffers))

(defun bob/kill-this-buffer ()
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun bob/eat-top-project ()
  "Open an Eat shell on the highest project"
  (interactive)
  (if-let* ((project--root (bob/monorepo-root))
             (default-directory project--root))
      (eat-project)
    (error "Not in project")))


(defun set-last-shell-buffer-as-first (buffers)
  (seq-sort (lambda (a b)
              (equal a bob/last-shell-buffer))
            buffers))

(defvar bob/last-shell-buffer nil)
(defun bob/jump-to-shell ()
  "Jump to a shell buffer."
  (interactive)
  (if-let* ((shell-buffers
             (bob/drop-buffer
              (set-last-shell-buffer-as-first
               (seq-filter
                (lambda (b) (or (equal (with-current-buffer b major-mode) 'vterm-mode)
                                (equal (with-current-buffer b major-mode) 'eshell-mode)
                                (equal (with-current-buffer b major-mode) 'shell-mode)
                                (equal (with-current-buffer b major-mode) 'js-comint-mode)
                                (equal (with-current-buffer b major-mode) 'sly-mrepl-mode)
                                (equal (with-current-buffer b major-mode) 'comint-mode)
                                (equal (with-current-buffer b major-mode) 'compilation-mode)
                                (equal (with-current-buffer b major-mode) 'eat-mode)
                                (equal (with-current-buffer b major-mode) 'sql-interactive-mode)
                                (equal (with-current-buffer b major-mode) 'roc-ts-repl-mode)
                                (equal (with-current-buffer b major-mode) 'jest-ts-mode/compilation-mode)
                                ))
                (mapcar (function buffer-name)
                        (buffer-list))))))
            (shell-buffer (completing-read "Shell: " shell-buffers)))
      (progn
        (setq bob/last-shell-buffer shell-buffer)
        (switch-to-buffer shell-buffer))
    (message "No Shell bufers exists")))

(defun get--inspect-processes-port ()
  (cl-remove-if-not 'identity
                    (mapcar
                     (lambda (process)
                       (if-let ((match (s-match "inspect=\\([0-9]+\\)" (nth 2 (process-command process)))))
                           (string-to-number (cadr match))))
                     (cl-remove-if-not
                      (lambda (p) (s-contains? "comint" (process-name p)))
                      (process-list)))))

(defun get--available-inspect-port ()
  (if-let (inspect-processes (get--inspect-processes-port))
      (1+ (car (-sort '> inspect-processes)))
    9229))

(defun check-types-command ()
  "Returns the command for running check-types NPM script if available"
  (when-let* ((default-directory (locate-dominating-file default-directory "package.json"))
              (package-json-raw (bob/read-file-content "package.json"))
              (package-json (json-parse-string package-json-raw
                                               :object-type 'alist)))
    (assocdr 'check-types (assocdr 'scripts package-json))))

(defun bob/npm--project-name ()
  "Get the current project name from the package json file."
  (when-let ((project-root-path (project-root (project-current)))
             (package-json (json-parse-string (bob/read-file-content
                                               (format "%s/package.json" project-root-path))
                                              :object-type 'alist)))
    (assocdr 'name package-json)))

(defun bob/compilation-buffer-name ()
  (if-let ((projcet-path (nth 2 (project-current))))
      (format "TS-COMPILE -- %s"
              (get-dir-name projcet-path))))

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
(provide 'bob-utils)
