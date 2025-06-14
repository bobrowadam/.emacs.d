;;; llm-tools.el --- An Emacs ai assistant -*- lexical-binding: t -*-
;;; Commentary:
;; Extending llm-tool-collection

;;; Code:
(require 'llm-tool-collection)
(require 'gptel)
(require 's)
(require 'dash)
(require 'project)
(require 'llama)
(require 'cl-lib)
(require 'eglot)
(require 'magit)

(llm-tool-collection-deftool read-file
  (:category "file-system" :tags (filesystem) :include t)
  ((file-name
    :type string
    :description "The full file name including the path"))
  "Returns the content of the given file if it exists, otherwise returns nil."
  (if (file-exists-p file-name)
    (with-current-buffer (find-file-noselect file-name nil nil)
      (buffer-substring-no-properties (point-min)
                                      (point-max)))
    (format "File %s does not exists. Maybe you meant to find a buffer with this name?" file-name)))

(llm-tool-collection-deftool create-file
  (:category "file-system" :tags (filesystem editing) :include t)
  ((filename
    :type string
    :description "The full filename including the path to create")
   (parents
    :type array
    :items (:type string)
    :optional t
    :description "Optional array of parents directories.
When you want to create a file and also it's parent directories you should use this function.
Example:
creating the file `router.ts' in directory `src/routes' you should pass parents as [\"src\", \"routes\"]"))
  "Create an empty file"
  (if (stringp filename)
      (condition-case err
          (progn (make-empty-file filename (cl-coerce parents 'list))
                 (format "Created the file %s successfully" filename))
        (error (format "got an unexpected error in create_file: %s" err)))
    (format "Error: filename must be a string. actual value: %s" filename)))

(llm-tool-collection-deftool create-dir
  (:category "file-system" :tags (filesystem editing) :include t)
  ((dir
    :type string
    :description "The directory path to create")
   (parents
    :type array
    :items (:type string)
    :optional t
    :description "Optional array of parent directories"))
  "Create the directory and optionally any nonexistent parent dirs."
  (if (stringp dir)
      (condition-case err
          (progn (make-directory dir (cl-coerce parents 'list))
                 (format "Created the directory %s successfully" dir))
        (error (format "got an unexpected error in create_dir: %s" err)))
    (format "Error: dir must be a string. actual value: %s" dir)))


(llm-tool-collection-deftool delete-file
  (:category "file-system" :tags (filesystem editing) :confirm t :include t)
  ((filename
    :type string
    :description "The full filename including the path to create"))
  "Create a new directory at the specified path if it does not already
exist."
  (if (file-exists-p filename)
      (progn (delete-file filename t)
             (format "Deleted file %s successfully." filename))
    (format "No such file: %s" filename)))

(llm-tool-collection-deftool delete-dir
  (:category "filesystem" :tags (filesystem editing) :confirm t :include t :description "Deletes directory recursively")
  ((dirname
    :type string
    ))
  "Create a new directory at the specified path if it does not already
exist."
  (if (file-exists-p dirname)
      (progn (delete-directory dirname t)
             (format "Deleted directory %s successfully." dirname))
    (format "No such directory %s." dirname)))

(llm-tool-collection-deftool get-buffer
  (:category "file-system" :tags (filesystem) :confirm t :include t)
  ((buffer-name
    :type string
    :description "The buffer name"))
  "Get a buffer content"
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (buffer-substring-no-properties (point-min)
                                        (point-max)))
    (format "buffer %s does not exists" buffer-name)))


(llm-tool-collection-deftool get-buffer-file-name
  (:category "file-system" :tags (filesystem) :include t)
  ((buffer-name
    :type string
    :description "The buffer name"))
  "Get the file name of the file visiting buffer."
  (if-let (buff (get-buffer buffer-name))
      (buffer-file-name buff)
    (format "buffer %s does not exists" buffer-name)))

(llm-tool-collection-deftool get-buffers-name-in-project
  (:category "file-system" :tags (filesystem project) :include t)
  ((file-visiting-p
    :type boolean
    :optional t
    :description "When true, return only the names of a file visiting buffers."))
  "Get the list of buffers in the current project.
 optionally get only file visiting buffers"
  (mapcar #'buffer-name
          (-filter (## if file-visiting-p (buffer-file-name %) t)
                   (project-buffers (project-current)))))

(llm-tool-collection-deftool find-files-by-regex
  (:category "file-system" :tags (filesystem project search) :include t)
  ((regex
    :type string
    :description "Emacs lisp regular expression pattern to match against file names"))
  "Search for files in the current project that match a regex pattern."
  (if-let ((proj (project-current)))
      (let* ((default-directory (project-root proj))
             (all-files (project-files proj))
             (matching-files (seq-filter (lambda (file)
                                          (string-match-p regex (file-name-nondirectory file)))
                                        all-files)))
        (if matching-files
            matching-files
          "No files matching the pattern were found."))
    "No project found. Please open a file within a project first."))

(llm-tool-collection-deftool get-project-root
  (:category "file-system" :tags (filesystem project) :include t)
  ()
  "Get the absolute path of the current project's root directory."
  (if-let ((proj (project-current)))
      (project-root proj)
    "No project found. Please open a file within a project first."))

(llm-tool-collection-deftool run-rg
  (:category "file-system" :tags (filesystem search project) :include t)
  ((pattern
    :type string
    :description "The search pattern to look for in files")
   (file-pattern
    :type string
    :optional t
    :description "Optional file pattern to filter which files to search (e.g., '*.js', 'src/*.py')"))
  "Run ripgrep (rg) to search for a pattern in the current project.
optionally filtering by file type."
  (if-let ((proj (project-current)))
      (let* ((default-directory (project-root proj))
             (cmd (if file-pattern
                      (format "rg --no-heading --line-number --with-filename %s %s"
                              (shell-quote-argument pattern)
                              (shell-quote-argument file-pattern))
                    (format "rg --no-heading --line-number --with-filename %s"
                            (shell-quote-argument pattern))))
             (result (shell-command-to-string cmd)))
        (if (string-empty-p result)
            "No matches found."
          result))
    "No project found. Please open a file within a project first."))

(llm-tool-collection-deftool get-flymake-diagnostics
  (:category "diagnostics" :tags (diagnostics flymake) :include t)
  ((buffer-name
    :type string
    :description "The buffer name to get the diagnostic for."))
  "Get the content of the flymake diagnostics a specific buffer."
  (if-let ((buff (get-buffer buffer-name)))
      (with-current-buffer buff
        (mapconcat (## format "%s   %s"
                      (line-number-at-pos (flymake-diagnostic-beg %))
                      (substring-no-properties
                       (flymake-diagnostic-message %)))
                   (flymake-diagnostics (point-min)
                                        (point-max))
                   "\n"))
    (format "buffer %s does not exists" buffer-name)))

(defun gptel-eglot--get-definition-context (point)
  "Get definition context for symbol around POINT."
  (when-let* ((symbol-definition (eglot--request
                                  (eglot--current-server-or-lose)
                                  :textDocument/definition
                                  (list :textDocument (eglot--TextDocumentIdentifier)
                                        :position (eglot--pos-to-lsp-position point))))
              (def-file (plist-get (seq-first symbol-definition) :targetUri))
              (def-range (plist-get (seq-first symbol-definition) :targetRange)))
    (with-current-buffer (find-file-noselect (eglot-uri-to-path def-file))
      (gptel-eglot--extract-context def-range))))

(defun gptel-eglot--get-references-context (point)
  "Get references context for symbol around POINT."
  (when-let* ((refs (eglot--request
                     (eglot--current-server-or-lose)
                     :textDocument/references
                     (list :textDocument (eglot--TextDocumentIdentifier)
                           :position (eglot--pos-to-lsp-position point)
                           :context '(:includeDeclaration t)))))
    (seq-mapcat (## let ((ref-uri (plist-get % :uri))
                           (ref-range (plist-get % :range)))
                      (with-current-buffer (find-file-noselect (eglot-uri-to-path ref-uri))
                        (format "In %s\nContext:\n%s"
                                (file-name-nondirectory (buffer-file-name))
                                (gptel-eglot--extract-context ref-range))))
                  refs
                  'string)))

(defun gptel-eglot--get-hover-context (point)
  "Get hover documentation context for symbol at POINT."
  (when-let ((hover (eglot--request
                     (eglot--current-server-or-lose)
                     :textDocument/hover
                     (list :textDocument (eglot--TextDocumentIdentifier)
                           :position (eglot--pos-to-lsp-position point)))))
    (plist-get (plist-get hover :contents) :value)))

(defun gptel-eglot--extract-context (code-range)
  "Extract CODE-RANGE context."
  (let ((start-point (eglot--lsp-position-to-point (plist-get code-range :start)))
        (end-point (eglot--lsp-position-to-point (plist-get code-range :end))))
    (save-excursion
      (let ((context-start (progn
                             (goto-char start-point)
                             (line-beginning-position)))
            (context-end (progn
                           (goto-char end-point)
                           (line-end-position))))
        (buffer-substring-no-properties context-start context-end)))))

(defun gptel-tool--find-point-in-buffer (symbol &optional line-number)
  "Find point in the current buffer using SYMBOL and maybe LINE-NUMBER."
  (condition-case nil
      (save-excursion
        (goto-char (point-min))
        (when line-number (forward-line (1- line-number)))
        (search-forward symbol (when line-number (line-end-position)) nil))
    (error (error "Could not find symbol %s in buffer %s"
                  symbol
                  (buffer-name (current-buffer))))))

(llm-tool-collection-deftool eglot-context
  (:category "code-intelligence" :tags (eglot lsp code-analysis) :include t)
  ((buffer-name
    :type string
    :description "The buffer name in which we want to inspect context around point")
   (symbol
    :type string
    :description "The symbol to examine")
   (line-number
    :type string
    :optional t
    :description "The line number on which the symbol is on"))
  "Retrieves code context using Eglot (LSP) for the given symbol
Including definitions, references, and documentation."
  (with-current-buffer buffer-name
    (if (not (eglot-current-server))
        "No Eglot server active for this buffer."
      (let ((point (gptel-tool--find-point-in-buffer symbol line-number)))
        (format "symbol definition:\n%s\nsymbol references:\n%s\nsymbol info:\n%s"
                (gptel-eglot--get-definition-context point)
                (gptel-eglot--get-references-context point)
                (gptel-eglot--get-hover-context point)
                )))))

;; Magit tools
(llm-tool-collection-deftool magit-diff-with-main
  (:category "git" :tags (git magit diff) :include t)
  ((target-branch
    :type string
    :optional t
    :description "The target branch to diff against (defaults to 'main')"))
  "Get the diff between current branch and main branch (or specified target branch)"
  (condition-case err
      (if (not (magit-git-repo-p default-directory))
          "Not in a git repository"
        (let* ((main-branch (or target-branch "main"))
               (current-branch (magit-get-current-branch)))
          (if (not current-branch)
              "Could not determine current branch"
            (if (string= current-branch main-branch)
                (format "Already on %s branch" main-branch)
              (let ((diff-output (with-temp-buffer
                                   (magit-git-insert "diff" (concat main-branch "...HEAD"))
                                   (buffer-string))))
                (if (string-empty-p diff-output)
                    (format "No differences found between %s and %s" main-branch current-branch)
                  (format "Diff between %s and %s:\n\n%s" main-branch current-branch diff-output)))))))
    (error (format "Error getting diff: %s" err))))

(llm-tool-collection-deftool magit-status
  (:category "git" :tags (git magit status) :include t)
  ()
  "Get the current git status showing staged, unstaged, and untracked files"
  (condition-case err
      (if (not (magit-git-repo-p default-directory))
          "Not in a git repository"
        (let ((status-info (with-temp-buffer
                             (magit-git-insert "status" "--porcelain")
                             (buffer-string))))
          (if (string-empty-p status-info)
              "Working directory clean"
            (format "Git status:\n%s" status-info))))
    (error (format "Error getting git status: %s" err))))

(llm-tool-collection-deftool magit-current-branch
  (:category "git" :tags (git magit branch) :include t)
  ()
  "Get the name of the current git branch"
  (condition-case err
      (if (not (magit-git-repo-p default-directory))
          "Not in a git repository"
        (let ((branch (magit-get-current-branch)))
          (if branch
              (format "Current branch: %s" branch)
            "Could not determine current branch (possibly in detached HEAD state)")))
    (error (format "Error getting current branch: %s" err))))

(llm-tool-collection-deftool magit-log
  (:category "git" :tags (git magit log) :include t)
  ((limit
    :type number
    :optional t
    :description "Number of commits to show (defaults to 10)"))
  "Get recent git commits log"
  (condition-case err
      (if (not (magit-git-repo-p default-directory))
          "Not in a git repository"
        (let* ((count (or limit 10))
               (log-output (with-temp-buffer
                             (magit-git-insert "log" "--oneline" (format "-%d" count))
                             (buffer-string))))
          (if (string-empty-p log-output)
              "No commits found"
            (format "Recent %d commits:\n%s" count log-output))))
    (error (format "Error getting git log: %s" err))))

(llm-tool-collection-deftool magit-unstaged-changes
  (:category "git" :tags (git magit diff) :include t)
  ()
  "Get the unstaged changes in the working directory"
  (condition-case err
      (if (not (magit-git-repo-p default-directory))
          "Not in a git repository"
        (let ((diff-output (with-temp-buffer
                             (magit-git-insert "diff")
                             (buffer-string))))
          (if (string-empty-p diff-output)
              "No unstaged changes"
            (format "Unstaged changes:\n\n%s" diff-output))))
    (error (format "Error getting unstaged changes: %s" err))))

(llm-tool-collection-deftool magit-staged-changes
  (:category "git" :tags (git magit diff) :include t)
  ()
  "Get the staged changes ready for commit"
  (condition-case err
      (if (not (magit-git-repo-p default-directory))
          "Not in a git repository"
        (let ((diff-output (with-temp-buffer
                             (magit-git-insert "diff" "--cached")
                             (buffer-string))))
          (if (string-empty-p diff-output)
              "No staged changes"
            (format "Staged changes:\n\n%s" diff-output))))
    (error (format "Error getting staged changes: %s" err))))

(provide 'llm-tools)
;;; llm-tools.el ends here
