(defun buffer--content-with-line-numbers (&optional buffer)
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

(defun gptel-read-file-cb (path filename)
  (let ((full-path (expand-file-name filename path)))
    (with-temp-buffer
      (read-file full-path))))

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

(defun gptel-read-dir-cb (directory)
  (mapcar (lambda (file)
            (let ((full-path (expand-file-name file directory)))
              (if (file-directory-p full-path)
                  (format "%s/" file)
                file)))
          (directory-files directory nil "^[^.]*")))

(gptel-make-tool
 :name "read_directory"
 :function 'gptel-read-dir-cb
 :description "List the directory"
 :args (list '(:name "path"
                     :type string
                     :description "The directory where to create the file"))
 :category "filesystem")

(defun gptel-find-files-by-pattern-cb (pattern)
  "Find files in DIRECTORY with names matching glob PATTERN and return the list of file paths."
  (split-string
   (shell-command-to-string (format "rg --files --glob %s"
                                    (shell-quote-argument pattern)))
   "\n" t))

(gptel-make-tool
 :name "find_files_in_project"
 :function 'gptel-find-files-by-pattern-cb
 :description "Find files in DIRECTORY with names matching glob PATTERN, and return the list of file paths."
 :args (list '(:name "pattern"
                     :type string
                     :description "The pattern to look for in file name using glob pattern"))
 :category "filesystem")

(defun gptel-ripgrep-cb (pattern)
  (when-let ((default-directory (project-root (project-current))))
    (shell-command-to-string (format "rg --no-heading --line-number %s" (shell-quote-argument pattern)))))

(gptel-make-tool
 :name "ripgrep_project"
 :function 'gptel-ripgrep-cb
 :description "Find all occurrences of PATTERN in the current Emacs project"
 :args (list '(:name "pattern"
                     :type string
                     :description "The glob pattern to grep"))
 :category "filesystem")

(defun gptel-get-buffer-content-cb (buffer-name)
  "Return the content of the buffer specified by BUFFER-NAME as a string,
 or provide an error if it doesn't exist."
  (if-let ((this-buffer (get-buffer buffer-name)))
      (buffer--content-with-line-numbers this-buffer)
    (format "Error: Buffer '%s' not found. Current working directory: %s" buffer-name default-directory)))

(gptel-make-tool
 :name "get_buffer"
 :function 'gptel-get-buffer-content-cb
 :description "Get the content of buffer BUFFER-NAME"
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The buffer name to get"))
 :category "buffers")

(defun gptel-list-buffers-cb ()
  "List all buffers."
  (mapcar #'buffer-name (buffer-list)))

(gptel-make-tool
 :name "list_buffers"
 :function 'gptel-list-buffers-cb
 :description "List all buffers"
 :args nil
 :category "buffers")

(defun gptel-list-file-buffers-cb ()
  "List all file-visiting buffers."
  (mapcar #'buffer-name
          (seq-filter #'buffer-file-name (buffer-list))))

(gptel-make-tool
 :name "list_file_buffers"
 :function 'gptel-list-file-buffers-cb
 :description "List all file-visiting buffers"
 :args nil
 :category "buffers")

(defun gptel-git-diff-cb (branch)
  "Get git diff with relation to BRANCH (default is 'main')."
  (let ((default-directory (locate-dominating-file default-directory ".git"))
        (branch-name (or branch "main")))
    (shell-command-to-string (format "git diff %s" branch-name))))

(gptel-make-tool
 :name "git_diff"
 :function 'gptel-git-diff-cb
 :description "Get git diff with relation to branch (default branch is main)"
 :args (list '(:name "branch"
                     :type string
                     :description "The branch to compare with, default is \"main\""))
 :category "version control")

(defun gptel-git-log-cb ()
  "Get git log with the last 20 changes."
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (shell-command-to-string "git log -n 20")))
(gptel-make-tool
 :name "git_log"
 :function 'gptel-git-log-cb
 :description "Get git log"
 :args nil
 :category "version control")

(defun gptel-edit-file-content-cb (filename content)
  "Edit FILENAME with new CONTENT."
  (with-temp-buffer
    (insert content)
    (write-file filename)))
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

(defun gptel-replace-cb (pattern replacement path)
  "Replace occurrences of PATTERN with REPLACEMENT in PATH."
  (let ((default-directory path))
    (shell-command-to-string (format "rg --files | xargs sed -i 's/%s/%s/g'"
                                     (shell-quote-argument pattern)
                                     (shell-quote-argument replacement)))))
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

(defun type-check-grain-repo-cb ()
  "Run type checking in Grain repo root"
  (let ((default-directory "~/source/grain"))
    (shell-command-to-string
      (format "npm run check-types"))))

(gptel-make-tool
 :name "check-types"
 :function 'type-check-grain-repo-cb
 :description "Run type checking in Grain repo root"
 :args nil
 :category "Grain")

(defun gptel-edit-buffer-content-cb (buffer-name new-content)
  "Edit the content of the buffer specified by BUFFER-NAME and save it."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (erase-buffer)
      (insert new-content)
      (save-buffer)
      "Content updated and file saved successfully.")))

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

(defun gptel-open-file-buffer-cb (path filename &optional create-if-not-exists)
  "Open the given file into a buffer without switching.
 If CREATE-IF-NOT-EXISTS is non-nil, create the file if it doesn't exist.
 Return buffer content if it exists, otherwise return a
 message specifying the buffer name."
  (let* ((full-path (expand-file-name filename path))
         (buffer (get-file-buffer full-path)))
    (if buffer
        (buffer--content-with-line-numbers buffer)
      (if (file-exists-p full-path)
          (with-current-buffer (find-file-noselect full-path)
            (buffer--content-with-line-numbers (buffer-name)))
        (if create-if-not-exists
            (let ((new-buffer (find-file-noselect full-path)))
              (format "Created a new file. Buffer name is: %s" (buffer-name new-buffer)))
          (format "Buffer for file '%s' does not exist. Current directory: %s"
                  (file-name-nondirectory full-path) default-directory))))))

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

(defun gptel-kill-buffer-cb (buffer-name)
  "Kill the buffer specified by BUFFER-NAME silently,
 or provide an error if it doesn't exist."
  (if (get-buffer buffer-name)
      (progn
        (kill-buffer buffer-name)
        (format "Buffer '%s' killed silently." buffer-name))
    (format "Error: Buffer '%s' not found. Current working directory: %s" buffer-name default-directory)))

(gptel-make-tool
 :name "kill_buffer"
 :function 'gptel-kill-buffer-cb
 :description "Kill the buffer specified by BUFFER-NAME"
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to kill"))
 :category "buffers")

(defun gptel-get-project-root-cb ()
  (-some-> (project-current) project-root))

(gptel-make-tool
 :name "get_project_root"
 :function 'gptel-get-project-root-cb
 :description "Get the current project root else return nil"
 :args ()
 :category "filesystem")

(defun gptel-run-pwd-cb ()
  (shell-command-to-string "pwd"))

(gptel-make-tool
 :name "run_pwd"
 :function 'gptel-run-pwd-cb
 :description "Run the unix pwd command in a directory"
 :args nil
 :category "filesystem")

(defun apply-diff-to-buffer (buffer-name diff-content)
  "Apply a unified diff DIFF-CONTENT to BUFFER-NAME.
The diff should be in unified diff format, like:
@@ -1,3 +1,4 @@
 unchanged line
-removed line
+added line
 unchanged line
+another added line

Returns t if successful, nil otherwise with a message explaining why."
  (unless (get-buffer buffer-name)
    (error "Buffer %s does not exist" buffer-name))

  (with-current-buffer (get-buffer buffer-name)
    (save-excursion
      (let ((lines (split-string diff-content "\n" t))
            (current-line 1))
        ;; Validate diff format
        (unless (seq-some (lambda (line)
                            (string-match "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" line))
                          lines)
          (error "Invalid diff format - missing @@ header"))

        (dolist (line lines)
          (cond
           ;; Parse hunk header
           ((string-match "^@@ -\\([0-9]+\\),[0-9]+ \\+[0-9]+,[0-9]+ @@" line)
            (setq current-line (string-to-number (match-string 1 line)))
            (goto-char (point-min))
            (forward-line (1- current-line)))

           ;; Skip diff command lines
           ((string-match "^[+-][-+]" line) nil)

           ;; Handle context lines (lines without +/-)
           ((not (or (string-prefix-p "+" line)
                     (string-prefix-p "-" line)))
            (let ((expected-line (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
              (if (string= (string-trim line)
                           (string-trim expected-line))
                  (forward-line 1)
                (format "Error: Context mismatch at line %d. Expected '%s', got '%s'"
                        current-line expected-line line)))
            (cl-incf current-line))

           ;; Handle line removal
           ((string-prefix-p "-" line)
            (let ((to-delete (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
              (if (string= (string-trim (substring line 1)) (string-trim to-delete))
                  (delete-region (line-beginning-position)
                                 (min (point-max) (1+ (line-end-position))))
                (format "Error: Content mismatch for deletion at line %d" current-line))))

           ;; Handle line addition
           ((string-prefix-p "+" line)
            (insert (substring line 1) "\n")
            (cl-incf current-line))))

        (save-buffer)
        (buffer--content-with-line-numbers)))))

(gptel-make-tool
 :name "apply_diff_to_buffer"
 :function 'apply-diff-to-buffer
 :description "Apply a unified diff to a buffer. The diff should be in unified format with @@ headers, context lines, and +/- line changes. Returns t if successful, nil otherwise with error message."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The buffer name to apply the diff to.")
             '(:name "diff-content"
                     :type string
                     :description "The diff content to apply."))
 :category "buffers")

(defun gptel-run-make-cb (path &optional args)
  (let ((default-directory path))
    (shell-command-to-string (format "make %s" (string-join args " ")))))

(gptel-make-tool
 :name "run_make"
 :function 'gptel-run-make-cb
 :description "Run make with optional flags in the current project."
 :args (list
        '(:name "path"
               :type string
               :description "The path to run in")
        '(:name "arguments"
                     :type array
                     :value string
                     :description "Optional extra arguments"
                     :optional t))
 :category "buffers")
