(defun assocdr (prop cons-cell)
  "Get the cdr of an association with key PROP in CONS-CELL.
This is a shorthand for (cdr (assoc prop cons-cell))."
  (cdr (assoc prop cons-cell)))

(defun get-dir-name (&optional path)
  "Get the current directory name from PATH.
If PATH is nil, use `default-directory'."
  (file-name-nondirectory
     (directory-file-name
      (file-name-directory (or path default-directory)))))

(defun bob/read-file-content (file-name)
  "Read the content of FILE-NAME and return it as a string."
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

(defvar bob/last-shell-buffer nil)

(defun set-last-shell-buffer-as-first (buffers)
  (seq-sort (lambda (a b)
              (equal a bob/last-shell-buffer))
            buffers))


(defun bob/jump-to-shell ()
  "Jump to a shell buffer."
  (interactive)
  (if-let* ((shell-buffers
             (bob/drop-buffer
              (set-last-shell-buffer-as-first
               (seq-filter
                (lambda (b) (or (equal (with-current-buffer b major-mode) 'vterm-mode)
                                (equal (with-current-buffer b major-mode) 'eshell-mode)
                                (with-current-buffer b (derived-mode-p 'comint-mode))
                                (with-current-buffer b (derived-mode-p 'compilation-mode))
                                (equal (with-current-buffer b major-mode) 'sly-mrepl-mode)
                                (equal (with-current-buffer b major-mode) 'eat-mode)
                                (equal (with-current-buffer b major-mode) 'sql-interactive-mode)
                                (equal (with-current-buffer b major-mode) 'pgmacs-mode)))
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

(defun read-file (file-name)
  "Return the contents of FILE-NAME as a lisp data type."
  (when (file-exists-p file-name)
   (with-temp-buffer
     (insert-file-contents file-name)
     (buffer-string))))

(defun bob/get-unix-timestamp ()
  "Return the current Unix timestamp as an integer."
  (floor (float-time (current-time))))

(defmacro bob/with-default-dir (directory &rest body)
  "Evaluate BODY with 'default-dir' as DIRECTORY. keymap is \\{typescript-ts-mode-map}"
  (declare (indent 1) (debug t))
  `(let ((default-directory ,directory))
     ,@body))

(defun get--processes-by-string (captured-string)
  (-let [lines (-filter
                (lambda (str)
                  (not (string-match-p (regexp-quote "rg") str)))
                (s-split "\n" (shell-command-to-string (format "ps aux | rg %s" captured-string)) t))]
    (mapcar (lambda (line)
              (nth 1 (s-split " " line t)))
            lines)))

(defun bob/kill-inspect-process ()
  (interactive)
  (-let ((process (get--processes-by-string "inspect")))
    (if process
      (progn (message "Found inspect processes: %s, killing them now" process)
             (when (equal (shell-command (format "kill %s" (s-join " " process)))
                          0)
               (message "Killed inspect processes: %s" process)))
      (message "No inspect processes found"))))

(provide 'bob-utils)
