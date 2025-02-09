;; TODO: improve this
(defun gptel-run-tree-cb (dir-path depth &optional sub-dirs)
  "Runs the tree command from DIR-PATH displaying up to DEPTH levels.
Optional SUB-DIRS restricts display to specific directories."
  (let* ((default-directory (or dir-path default-directory)) ; Replace <default_path> with a sensible default
         (depth (or depth 1)) ; Default depth if none provided
         (sub-dirs-arg (if (and sub-dirs (listp (cl-coerce sub-dirs 'list)))
                           (mapconcat 'identity sub-dirs " ")
                         "")))
    (if (and (stringp default-directory) (numberp depth))
        (shell-command-to-string (format "tree %s -L %d" sub-dirs-arg depth))
      (error "Invalid arguments: directory path must be a string and depth must be a number"))))

(defun buffer-content-with-line-numbers (&optional buffer)
  "Return content of BUFFER with line numbers prepended.
If BUFFER is nil, use the current buffer."
  (-> (with-current-buffer (or buffer (current-buffer))
     (let* ((lines (split-string (buffer-string) "\n"))
            (numbered-lines
             (cl-loop for line in lines
                      for line-number from 1
                      collect (cons line-number line))))
       (mapconcat
        (lambda (numbered-line)
          (format "%4d: %s" (car numbered-line) (cdr numbered-line)))
        numbered-lines
        "\n")))
      substring-no-properties))

(defun gptel-run-pwd-cb ()
  (shell-command-to-string "pwd"))

(defun changes--array-to-list (changes-array)
  (let ((changes-list (cl-coerce changes-array 'list)))
    (dolist (change changes-list)
        (parse--change-schema change))
    changes-list))

(defun parse--change-schema (change)
  (progn
    (unless (plist-get change :line-number)
      (error ":line-number is a mandatory filed in change"))
    (unless (plist-get change :action)
      (error ":action is a mandatory filed in change"))
    (unless (plist-get change :current-line-content)
      (error ":current-line-content is a mandatory filed in change"))
    (unless (plist-get change :updated-line-content)
      (error ":updated-line-content is a mandatory filed in change"))))

(defun gptel--validate-action (line-number line-expected-content)
  (let ((line-current-content (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- line-number))
                                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (unless (s-contains? line-expected-content line-current-content)
      (error "Validation failed on line %d. Expected content: '%s'. Actual content: '%s'."
             line-number
             line-expected-content
             line-current-content))))

(defun gptel-apply-changes-to-buffer-cb (buffer-name changes)
  "Apply CHANGES to the buffer named BUFFER-NAME.
CHANGES is an array of objects; each object includes line-number,
action, current-line-content, and updated-line-content."
  (let ((delete-offset 1))
    (with-current-buffer buffer-name
      ;; Wrap the save-excursion block with condition-case
      (condition-case err
          (progn
            (save-excursion
              (dolist (change (changes--array-to-list changes))
                (let ((line (- (plist-get change :line-number)
                               delete-offset))
                      (line-expected-content (plist-get change :current-line-content))
                      (action (intern (plist-get change :action)))
                      (updated-content (plist-get change :updated-line-content)))
                  (gptel--validate-action (1+ line) line-expected-content)
                  (goto-char (point-min))
                  (forward-line line)
                  (pcase action
                    ('replace
                     (kill-whole-line)
                     (insert updated-content))
                    ('insert
                     (insert updated-content "\n"))
                    ('delete
                     (kill-whole-line)
                     (setq delete-offset (1+ delete-offset)))))))
            ;; Save the buffer if no error occurs
            (save-buffer)
            ;; Return the buffer content with line numbers if no error occurs
            (buffer-content-with-line-numbers (current-buffer)))
        ;; Return the error string if it happens
        (error (error-message-string err))))))


(defun gptel-read-file-cb (path filename)
  (let ((full-path (expand-file-name filename path)))
    (with-temp-buffer
      (read-file full-path))))

(defun gptel-open-file-buffer-cb (path filename &optional create-if-not-exists)
  "Open the given file into a buffer without switching.
 If CREATE-IF-NOT-EXISTS is non-nil, create the file if it doesn't exist.
 Return buffer content if it exists, otherwise return a
 message specifying the buffer name."
  (let* ((full-path (expand-file-name filename path))
         (buffer (get-file-buffer full-path)))
    (if buffer
        (buffer-content-with-line-numbers buffer)
      (if (file-exists-p full-path)
          (with-current-buffer (find-file-noselect full-path)
            (buffer-content-with-line-numbers (buffer-name)))
        (if create-if-not-exists
            (let ((new-buffer (find-file-noselect full-path)))
              (format "Created a new file. Buffer name is: %s" (buffer-name new-buffer)))
          (format "Buffer for file '%s' does not exist. Current directory: %s"
                  (file-name-nondirectory full-path) default-directory))))))

(defun gptel-read-dir-cb (directory)
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
  "Return the content of the buffer specified by BUFFER-NAME as a string,
 or provide an error if it doesn't exist."
  (if-let ((this-buffer (get-buffer buffer-name)))
      (buffer-content-with-line-numbers this-buffer)
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
  "Get git log with the last 20 changes."
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (shell-command-to-string "git log -n 20")))

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

(defun gptel-kill-buffer-cb (buffer-name)
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
   :function 'gptel-read-file-cb
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
   :function 'gptel-read-dir-cb
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
   :function 'gptel-open-file-buffer-cb
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
   :function 'gptel-kill-buffer-cb
   :description "Kill the buffer specified by BUFFER-NAME"
   :args (list '(:name "buffer-name"
                       :type string
                       :description "The name of the buffer to kill"))
   :category "buffers")

  (gptel-make-tool
   :name "get_project_root"
   :function 'gptel-get-project-root-cb
   :description "Get the current project root else return nil"
   :args ()
   :category "filesystem")

  (gptel-make-tool
   :name "run_tree"
   :function 'gptel-run-tree-cb
   :args (list '(:path "dir-path"
                       :type string
                       :description "The directory path (required)"
                       :default "<default_path>") ; specify a sensible default if possible
               '(:depth "depth"
                        :type number
                        :description "The depth to run tree with (required)"
                        :default 1) ; a reasonable default depth
               '(:sub-dirs "sub-dirs"
                           :type array
                           :value (:type string)
                           :description "The sub dirs to map as an array of strings. If not specified, all subdirectories will be listed."))
   :category "filesystem"
   :description "Run the unix tree command in a directory. the function should get a dir-path, a depth and a sub-dirs optional string array")


  (gptel-make-tool
   :name "run_pwd"
   :function 'gptel-run-pwd-cb
   :description "Run the unix pwd command in a directory"
   :args nil
   :category "filesystem")

  (gptel-make-tool
   :name "apply_changes_to_buffer"
   :function 'gptel-apply-changes-to-buffer-cb
   :description "Apply a series of changes to a buffer and retrieve the final buffer content"
   :args (list '(:name "buffer-name"
                       :type string
                       :description "The name of the buffer to modify")
               '(:name "changes"
                       :type array
                       :value (:type 'object
                                     :properties (:line-number
                                                  (:type "number")
                                                  :action
                                                  (:type "enum"
                                                         :items
                                                         ["insert" "replace" "delete"])
                                                  :current-line-content
                                                  (:type "string")
                                                  :updated-line-content
                                                  (:type "string")))
                       :description "An array of change operations. Each operation is an object with
the change operation.
Change operation schema:
:line-number
    number
:action
    enum
    'delete for deleting the whole line
    'replace for replacing the whole line
    'insert for insert a line below the line
:current-line-content
    type: string
:updated-line-content
    type: string
"))
   :category "buffers")

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
