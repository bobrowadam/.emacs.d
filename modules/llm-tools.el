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
