;;; llm-tools.el --- An Emacs ai assistant -*- lexical-binding: t -*-
;;; Commentary:
;; Extending llm-tool-collection

;;; Code:
(require 's)
(require 'dash)
(require 'project)
(require 'llama)
(require 'cl-lib)
(require 'eglot)
(require 'magit)

(llm-tool-collection-deftool delete-file
  (:category "filesystem" :tags (filesystem editing) :confirm t :include t)
  ((filename "The full filename including the path to create" :type string))
  "Create a new directory at the specified path if it does not already
exist."
  (if (file-exists-p filename)
      (progn (delete-file filename t)
             (format "Deleted file %s successfully." filename))
    (format "No such file: %s" filename)))

(llm-tool-collection-deftool delete-dir
  (:category "filesystem" :tags (filesystem editing) :confirm t :include t)
  ((dirname "The full dir path to delete"
            :type string))
  "Deletes directory recursively"
  (if (file-exists-p dirname)
      (progn (delete-directory dirname t)
             (format "Deleted directory %s successfully." dirname))
    (format "No such directory %s." dirname)))

(llm-tool-collection-deftool get-buffer
  (:category "filesystem" :tags (filesystem editing) :confirm t :include t)
  ((buffer-name "The buffer name" :type string))
  "Get a buffer content"
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (buffer-substring-no-properties (point-min)
                                        (point-max)))
    (format "buffer %s does not exists" buffer-name)))

(llm-tool-collection-deftool get-buffer-file-name
  (:category "filesystem" :tags (filesystem) :include t)
  ((buffer-name "The buffer name" :type string))
  "Get the file name of the file visiting buffer."
  (if-let (buff (get-buffer buffer-name))
      (buffer-file-name buff)
    (format "buffer %s does not exists" buffer-name)))

(llm-tool-collection-deftool summarize-chat-buffer
  (:category "buffer" :tags (editing) :confirm t :include t)
  ((summary "The context summary" :type string)
   (chat-buffer "The buffer name in which the chat takes place" :type string))
  "Replace content of the chat buffer with a summary."
  (let ((buffer (get-buffer chat-buffer)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert summary)))))

(llm-tool-collection-deftool get-buffers-name-in-project
  (:category "filesystem" :tags (filesystem project) :include t)
  ((file-visiting-p "When true, return only the names of file visiting buffers."
                    :type boolean
                    :optional t)
   (project-root-path "The project root path. Accept path with \"~\""
                      :type string
                      :optional t))
  "Get the list of buffers in the current project.
 optionally get only file visiting buffers"
  (mapcar #'buffer-name
          (-filter (## if file-visiting-p (buffer-file-name %) t)
                   (project-buffers (project-current nil project-root-path)))))

(llm-tool-collection-deftool find-files-by-regex-in-project
  (:category "filesystem" :tags (filesystem project search) :include t)
  ((regex "Emacs lisp regular expression pattern to match against file names"
          :type string))
  "Search for files in the current project that match a regex pattern."
  (if-let ((proj (project-current nil project-root-path)))
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
  (:category "filesystem" :tags (filesystem project) :include t)
  ((project-root-path "The project root path. Accept path with \"~\""
                      :type string
                      :optional t))
  "Get the absolute path of the current project's root directory."
  (if-let ((proj (project-current nil project-root-path)))
      (project-root proj)
    "No project found. Please open a file within a project first."))

(llm-tool-collection-deftool run-rg
  (:category "filesystem" :tags (filesystem search project) :include t)
  ((pattern "The search pattern to look for in files"
            :type string)
   (file-pattern "Optional file pattern to filter which files to search (e.g., '*.js', 'src/*.py')"
                 :type string
                 :optional t)
   (project-root-path "The project root path. Accept path with \"~\""
                      :type string
                      :optional t))
  "Run ripgrep (rg) to search for a pattern in the current project.
optionally filtering by file type."
  (if-let* ((proj (project-current nil project-root-path))
            (default-directory (project-root proj))
            (cmd (if file-pattern
                     (format "rg --no-heading --line-number --with-filename %s %s"
                             (shell-quote-argument pattern)
                             (shell-quote-argument file-pattern))
                   (format "rg --no-heading --line-number --with-filename %s"
                           (shell-quote-argument pattern))))
            (result (shell-command-to-string cmd)))
      (if (string-empty-p result)
          "No matches found."
        result)
    "No project found. Please open a file within a project first."))

(llm-tool-collection-deftool get-flymake-diagnostics
  (:category "diagnostics" :tags (diagnostics flymake) :include t)
  ((buffer-name "The buffer name to get the diagnostic for."
                :type string))
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
  ((buffer-name "The buffer name in which we want to inspect context around point" :type string)
   (symbol "The symbol to examine" :type string)
   (line-number "The line number on which the symbol is on"
                :type string
                :optional t))
  "Retrieves code context using Eglot (LSP) for the given symbol
Including definitions, references, and documentation."
  (with-current-buffer buffer-name
    (if (not (eglot-current-server))
        "No Eglot server active for this buffer."
      (let ((point (gptel-tool--find-point-in-buffer symbol line-number)))
        (format "symbol definition:\n%s\nsymbol references:\n%s\nsymbol info:\n%s"
                (gptel-eglot--get-definition-context point)
                (gptel-eglot--get-references-context point)
                (gptel-eglot--get-hover-context point))))))

(llm-tool-collection-deftool summarize-chat-buffer
  (:category "buffer" :tags (editing) :confirm t :include t)
  ((summary "The context summary" :type string)
   (chat-buffer "The buffer name in which the chat takes place" :type string))
  "Summarize and replace content of the specified chat buffer."
  (if-let ((buffer (get-buffer chat-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert summary))
    (format "No such buffer: %s" chat-buffer)))

;; Magit tools
(llm-tool-collection-deftool magit-diff-with-main
  (:category "git" :tags (git magit diff) :include t)
  ((target-branch "The target branch to diff against (defaults to 'main')"
                  :type string
                  :optional t))
  "Get the diff between current branch and main branch (or specified target branch)"
  (condition-case err
      (if (not (magit-git-repo-p default-directory))
          (format "Not in a git repository. default-directory: %s" default-directory)
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
  ((limit "Number of commits to show (defaults to 10)"
          :type number
          :optional t))
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

(llm-tool-collection-deftool summarize-chat-buffer
  (:category "context" :tags (editing) :confirm nil :include nil)
  ((summary "The context summary." :type string))
  "Replace the chat buffer content with a context summary to compact the context."
  (when-let ((buffer (current-buffer)))
    (message "here")
    (with-current-buffer buffer
      (erase-buffer)
      (insert summary)
      (save-buffer)
      (goto-char (point-min))
      (while (re-search-forward "**?:PROPERTIES:.*\n\\(?:.*\n\\)*?:END:\n" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (save-buffer)
      (goto-char (point-max)))))

(provide 'llm-tools)
;;; llm-tools.el ends here

(setq \? 1)
(+ \? 2)
