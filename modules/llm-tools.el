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
