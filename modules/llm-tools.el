(defun gptel-tool-read-file (file-name)
  (when (file-exists-p file-name)
    (with-current-buffer (find-file-noselect file-name t t)
      (buffer-string))))

(gptel-make-tool
 :name "read_file"
 :include t
 :function 'gptel-tool-read-file
 :description "Returns the content of the given file if it exists, otherwise returns nil."
 :args '(( :name "file_name"
           :type string
           :description "The full file name including the path"))
 :category "file-system")

(defun gptel-tool-create-file (file-name &optional parents)
  (if (stringp file-name)
      (condition-case err
          (progn (make-empty-file file-name (cl-coerce parents 'list))
                 (format "Created the file %s successfully" file-name))
        (error (format "got an unexpected error in create_file: %s" err)))
    (format "Error: file-name must be a string. actual value: %s" file-name)))

(gptel-make-tool
 :name "create_file"
 :include t
 :function 'gptel-tool-create-file
 :args '(( :name "file_name"
           :type string
           :description "The full filename including the path to create")
         ( :name "parents"
           :type array
           :items (:type string)
           :optional t
           :description "Optional array of parents directories.
When you want to create a file and also it's parent directories you should use this function.
Example:
creating the file `router.ts' in directory `src/routes' you should pass parents as [\"src\", \"routes\"]"))
 :category "file-system"
 :description "Create an empty file")

(defun gptel-tool-create-dir (dir-name &optional parents)
  (if (stringp dir-name)
      (condition-case err
          (progn (make-directory dir-name (cl-coerce parents 'list))
                 (format "Created the directory %s successfully" dir-name))
        (error (format "got an unexpected error in create_dir: %s" err)))
    (format "Error: dir-name must be a string. actual value: %s" dir-name)))

(gptel-make-tool
 :name "create_dir"
 :include t
 :function 'gptel-tool-create-dir
 :args '(( :name "dir_name"
           :type string
           :description "The full directory path to create")
         ( :name "parents"
           :type array
           :items (:type string)
           :optional t
           :description "Optional array of parents directories.
When you want to create a directory and also it's parent directories you should use this function.
Example:
creating the directory `utils' in directory `src/routes' you should pass parents as [\"src\", \"routes\"]"))
 :category "file-system"
 :description "Create a directory")

(defun gptel-tool-delete-file (file-name)
  (when (file-exists-p file-name)
    (delete-file file-name t))
  (format "Deleted file %s successfully." file-name))

(gptel-make-tool
 :name "delete_file"
 :include t
 :function 'gptel-tool-delete-file
 :args '(( :name "file_name"
           :type string
           :description "The full filename including the path to create"))
 :category "file-system"
 :description "Delete file")

(defun gptel-tool-delete-dir (dir-name)
  (if (file-exists-p dir-name)
      (progn (delete-directory dir-name t)
             (format "Deleted directory %s successfully." dir-name))
    (format "No such directory %s successfully." dir-name)))

(gptel-make-tool
 :name "delete_directory"
 :include t
 :function 'gptel-tool-delete-dir
 :args '(( :name "dir_name"
           :type string
           :description "The full directory name for deletion"))
 :category "file-system"
 :confirm t
 :description "Deletes directory recursively")

(defun gptel-eglot--get-definition-context (symbol radius)
  "Get definition context for SYMBOL with RADIUS lines of surrounding context."
  (when (and (featurep 'eglot) (eglot-current-server))
    (condition-case nil
        (let* ((def-pos (eglot--request
                         (eglot--current-server-or-lose)
                         :textDocument/definition
                         (list :textDocument (eglot--TextDocumentIdentifier)
                               :position (eglot--pos-to-lsp-position (point)))))
               (def-file (plist-get (elt def-pos 0) :uri))
               (def-range (plist-get (elt def-pos 0) :range)))
          (when def-file
            (with-current-buffer (find-file-noselect (eglot--uri-to-path def-file))
              (let* ((start-pos (eglot--lsp-position-to-point (plist-get def-range :start)))
                     (context-text (gptel-eglot--extract-context start-pos radius)))
                (concat "Definition of " symbol ":\n" context-text)))))
      (error nil))))

(defun gptel-eglot--get-references-context (symbol radius)
  "Get references context for SYMBOL with RADIUS lines of surrounding context."
  (when (and (featurep 'eglot) (eglot-current-server))
    (condition-case nil
        (let* ((refs (eglot--request
                      (eglot--current-server-or-lose)
                      :textDocument/references
                      (list :textDocument (eglot--TextDocumentIdentifier)
                            :position (eglot--pos-to-lsp-position (point))
                            :context '(:includeDeclaration t))))
               (context ""))
          (when refs
            (setq context (concat "References to " symbol ":\n"))
            (dolist (ref (seq-take refs 3))
              (let ((ref-uri (plist-get ref :uri))
                    (ref-range (plist-get ref :range)))
                (with-current-buffer (find-file-noselect (eglot--uri-to-path ref-uri))
                  (let* ((start-pos (eglot--lsp-position-to-point (plist-get ref-range :start)))
                         (context-text (gptel-eglot--extract-context start-pos (/ radius 2))))
                    (setq context (concat context
                                         "\nIn " (file-name-nondirectory (buffer-file-name)) ":\n"
                                         context-text))))))
            context))
      (error nil))))

(defun gptel-eglot--get-hover-context (symbol)
  "Get hover documentation context for SYMBOL."
  (when (and (featurep 'eglot) (eglot-current-server))
    (condition-case nil
        (let ((hover (eglot--request
                      (eglot--current-server-or-lose)
                      :textDocument/hover
                      (list :textDocument (eglot--TextDocumentIdentifier)
                            :position (eglot--pos-to-lsp-position (point))))))
          (when hover
            (concat "Information about " symbol ":\n"
                   (plist-get (plist-get hover :contents) :value))))
      (error nil))))

(defun gptel-eglot--extract-context (position radius)
  "Extract RADIUS lines of context around POSITION."
  (save-excursion
    (let ((context-start (progn
                           (goto-char position)
                           (forward-line (- radius))
                           (line-beginning-position)))
          (context-end (progn
                         (goto-char position)
                         (forward-line radius)
                         (line-end-position))))
      (buffer-substring-no-properties context-start context-end))))

(defun gptel-tool-eglot-context (symbol &optional radius)
  "Get code context for SYMBOL using Eglot with RADIUS lines of surrounding context."
  (let ((radius (or radius 5)))
    (if (not (eglot-current-server))
        "No Eglot server active for this buffer."
      (or
       (gptel-eglot--get-definition-context symbol radius)
       (gptel-eglot--get-references-context symbol radius)
       (gptel-eglot--get-hover-context symbol)
       (format "No context found for symbol '%s'" symbol)))))

(gptel-make-tool
 :name "eglot_context"
 :include t
 :function 'gptel-tool-eglot-context
 :description "Retrieves code context using Eglot (LSP) for the given symbol, including definitions, references, and documentation."
 :args '((:name "symbol"
          :type string
          :description "The symbol/identifier to look up context for")
         (:name "radius"
          :type integer
          :optional t
          :description "Number of surrounding lines to include in context (default: 5)"))
 :category "code-intelligence")

(provide 'llm-tools)
