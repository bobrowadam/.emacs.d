(defun gptel-apply-diff-to-buffer-cb (buffer-name diff)
  "Apply a unified diff to a specified file visiting BUFFER-NAME."
  (with-current-buffer buffer-name
    (let ((patch-buffer (get-buffer-create "*patch*")))
      (unwind-protect
          (progn
            (with-current-buffer patch-buffer
              (erase-buffer)
              (insert diff))
            (if (zerop (call-process-region (point-min) (point-max) "patch"
                                            nil nil nil
                                            "-o" "/dev/null"
                                            "--force" ;; Apply the patch regardless of comments
                                            "--input" "-"))
                (progn
                  (save-buffer)
                  (cons t "Diff applied successfully."))
              (cons nil "Failed to apply diff.")))
        ;; Cleanup the patch buffer
        (kill-buffer patch-buffer)))))

(defun pgtel-read-file-cb (path filename)
  (let ((full-path (expand-file-name filename path)))
    (with-temp-buffer
      (read-file full-path))))

(defun pgtel-open-file-buffer-cb (path filename &optional create-if-not-exists)
  "Open the given file into a buffer without switching. If CREATE-IF-NOT-EXISTS is non-nil, create the file if it doesn't exist. Provides the current directory in error messages."
  (let ((full-path (expand-file-name filename path)))
    (if (or (file-exists-p full-path) create-if-not-exists)
        (progn
          (find-file-noselect full-path)
          (if (file-exists-p full-path)
              "File opened in buffer without switching."
            "New file created and opened in buffer without switching."))
      (format "File '%s' does not exist and was not created. Current directory: %s" full-path default-directory))))

(defun pgtel-read-dir-cb (directory)
  (mapcar (lambda (file)
            (let ((full-path (expand-file-name file directory)))
              (if (file-directory-p full-path)
                  (format "%s/" file)
                file)))
          (directory-files directory nil "^[^.]")))

(defun gptel-ripgrep-cb (pattern)
  (when-let ((default-directory (project-root (project-current))))
    (shell-command-to-string (format "rg --no-heading --line-number %s" (shell-quote-argument pattern)))))

(defun gptel-find-files-by-pattern-cb (pattern)
  "Find files in DIRECTORY with names matching glob PATTERN,
and return the list of file paths."
  (split-string
   (shell-command-to-string (format "rg --files --glob %s"
                                    (shell-quote-argument pattern)))
   "\n" t))

(defun gptel-get-project-root-cb ()
  (-some-> (project-current) project-root))

(defun type-check-grain-repo-cb ()
  "Run type checking in Grain repo root"
  (let ((default-directory "~/source/grain"))
    (shell-command-to-string
      (format "npm run check-types"))))

(defun gptel-get-buffer-content-cb (buffer-name)
  "Return the content of the buffer specified by BUFFER-NAME as a string, or provide an error if it doesn't exist."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (buffer-substring-no-properties (point-min) (point-max)))
    (format "Error: Buffer '%s' not found. Current working directory: %s" buffer-name default-directory)))

(defun gptel-list-buffers-cb ()
  "List all buffers."
  (mapcar #'buffer-name (buffer-list)))

(defun gptel-list-file-buffers-cb ()
  "List all file-visiting buffers."
  (mapcar #'buffer-name
          (seq-filter #'buffer-file-name (buffer-list))))

(defun gptel-git-diff-cb (branch)
  "Get git diff with relation to BRANCH (default is 'main')."
  (let ((default-directory (locate-dominating-file default-directory ".git"))
        (branch-name (or branch "main")))
    (shell-command-to-string (format "git diff %s" branch-name))))

(defun gptel-git-log-cb ()
  "Get git log."
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (shell-command-to-string "git log")))

(defun gptel-edit-file-content-cb (filename content)
  "Edit FILENAME with new CONTENT."
  (with-temp-buffer
    (insert content)
    (write-file filename)))

(defun gptel-replace-cb (pattern replacement path)
  "Replace occurrences of PATTERN with REPLACEMENT in PATH."
  (let ((default-directory path))
    (shell-command-to-string (format "rg --files | xargs sed -i 's/%s/%s/g'"
                                     (shell-quote-argument pattern)
                                     (shell-quote-argument replacement)))))

(defun gptel-edit-buffer-content-cb (buffer-name new-content)
  "Edit the content of the buffer specified by BUFFER-NAME and save it."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (erase-buffer)
      (insert new-content)
      (save-buffer)
      "Content updated and file saved successfully.")))

(defun pgtel-kill-buffer-cb (buffer-name)
  "Kill the buffer specified by BUFFER-NAME silently,
 or provide an error if it doesn't exist."
  (if (get-buffer buffer-name)
      (progn
        (kill-buffer buffer-name)
        (format "Buffer '%s' killed silently." buffer-name))
    (format "Error: Buffer '%s' not found. Current working directory: %s" buffer-name default-directory)))

(use-package gptel
  :custom
  (gptel-model 'gpt-4o)
  (gptel-default-mode 'org-mode)
  (setq gptel-max-tokens 8192)
  ;; (gptel-max-tokens nil)
  :config
  (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                            car
                            (plist-get :secret)
                            funcall)))
    (gptel-make-anthropic
        "Claude"
      :stream t
      :key credentials))

  (gptel-make-tool
   :name "read_file"
   :function 'pgtel-read-file-cb
   :description "Read the given file and analyze it"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create"))
   :category "filesystem")

  (gptel-make-tool
   :name "read_directory"
   :function 'pgtel-read-dir-cb
   :description "List the directory"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where to create the file"))
   :category "filesystem")

  (gptel-make-tool
   :name "find_files_in_project"
   :function 'gptel-find-files-by-pattern-cb
   :description "Find files in DIRECTORY with names matching glob PATTERN, and return the list of file paths."
   :args (list '(:name "pattern"
                       :type string
                       :description "The pattern to look for in file name using glob pattern"))
   :category "filesystem")

  (gptel-make-tool
   :name "ripgrep_project"
   :function 'gptel-ripgrep-cb
   :description "Find all occurrences of PATTERN in the current Emacs project"
   :args (list '(:name "pattern"
                       :type string
                       :description "The glob pattern to grep"))
   :category "filesystem")

  ;; Get content off buffer
  (gptel-make-tool
   :name "get_buffer"
   :function 'gptel-get-buffer-content-cb
   :description "Get the content of buffer BUFFER-NAME"
   :args (list '(:name "buffer-name"
                       :type string
                       :description "The buffer name to get"))
   :category "buffers")

  ;; Tool to list all buffers
  (gptel-make-tool
   :name "list_buffers"
   :function 'gptel-list-buffers-cb
   :description "List all buffers"
   :args nil
   :category "buffers")

  ;; Tool to list all file-visiting buffers
  (gptel-make-tool
   :name "list_file_buffers"
   :function 'gptel-list-file-buffers-cb
   :description "List all file-visiting buffers"
   :args nil
   :category "buffers")

  ;; Tool to get git diff with relation to a branch (default branch is main)
  (gptel-make-tool
   :name "git_diff"
   :function 'gptel-git-diff-cb
   :description "Get git diff with relation to branch (default branch is main)"
   :args (list '(:name "branch"
                       :type string
                       :description "The branch to compare with, default is \"main\""))
   :category "version control")

  ;; Tool to get git log
  (gptel-make-tool
   :name "git_log"
   :function 'gptel-git-log-cb
   :description "Get git log"
   :args nil
   :category "version control")

  ;; Tool to edit file content
  (gptel-make-tool
   :name "edit_file_content"
   :function 'gptel-edit-file-content-cb
   :description "Edit file content"
   :args (list '(:name "filename"
                       :type string
                       :description "The path of the file to edit")
               '(:name "content"
                       :type string
                       :description "The new content for the file"))
   :category "filesystem")

  ;; Tool to replace text in project
  (gptel-make-tool
   :name "replace_project"
   :function 'gptel-replace-cb
   :description "Replace occurrences of PATTERN with REPLACEMENT in project"
   :args (list '(:name "pattern"
                       :type string
                       :description "The text to search for")
               '(:name "replacement"
                       :type string
                       :description "The text to replace with")
               '(:name "path"
                       :type string
                       :description "The directory root to look in"))
   :category "filesystem")

  (gptel-make-tool
   :name "check-types"
   :function 'type-check-grain-repo-cb
   :description "Run type checking in Grain repo root"
   :args nil
   :category "Grain")

  (gptel-make-tool
   :name "edit_buffer"
   :function 'gptel-edit-buffer-content-cb
   :description "Edit the content of buffer BUFFER-NAME and save it"
   :args (list '(:name "buffer-name"
                       :type string
                       :description "The buffer name to edit")
               '(:name "new-content"
                       :type string
                       :description "The new content for the buffer"))
   :category "buffers")
  
  (gptel-make-tool
   :name "open_file_buffer"
   :function 'pgtel-open-file-buffer-cb
   :description "Open the given file into a buffer, optionally creating it if it doesn't exist"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where the file is located.")
               '(:name "filename"
                       :type string
                       :description "The name of the file to open")
               '(:name "create-if-not-exists"
                       :type boolean
                       :description "If true, create the file if it doesn't exist"
                       :optional t))
   :category "filesystem")

  (gptel-make-tool
   :name "kill_buffer"
   :function 'pgtel-kill-buffer-cb
   :description "Kill the buffer specified by BUFFER-NAME"
   :args (list '(:name "buffer-name"
                       :type string
                       :description "The name of the buffer to kill"))
   :category "buffers")

  (gptel-make-tool
   :name "apply_diff_to_file_buffer"
   :function 'gptel-apply-diff-to-buffer-cb
   :description "Edit the content of buffer BUFFER-NAME and save it"
   :args (list '(:name "buffer-name"
                       :type string
                       :description "The buffer name to apply the diff in")
               '(:name "diff"
                       :type string
                       :description "The change diff to apply"))
   :category "buffers")

  (gptel-make-tool
   :name "get_project_root"
   :function 'gptel-get-project-root-cb
   :description "Get the current project root else return nil"
   :args ()
   :category "filesystem")

  :bind
  ("C-c g g" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g a d" . gptel-context-add)
  ("C-c g a f" . gptel-context-add-file)
  (:map gptel-mode-map ("C-c g s" . gptel-menu)))

                                        ; An arbitrary label for grouping

(unless (package-installed-p 'aider)
  (package-vc-install '(aider :url "https://github.com/tninja/aider.el")))
(use-package aider
  :config
  (setq
   aider-args (when-let ((credentials (-some-> (auth-source-search :host "claude.ai" :max 1)
                                        car
                                        (plist-get :secret)
                                        funcall)))
                `("--model" "anthropic/claude-3-5-sonnet-20241022" "--anthropic-api-key" ,credentials)))
  :bind ("C-c g c" . aider-transient-menu))

(provide 'setup-llm)
