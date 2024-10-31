;;;###autoload
(defun bob/jest-run-tests ()
  "Run Jest tests."
  (interactive)
  (if-let ((default-directory (locate-dominating-file "./" "jest.config.ts")))
   (compile (jest-test-command
             default-directory
             `(:file-name ,buffer-file-name :test-name ,(jest--get-current-test-name)))
            'jest-test-compilation-mode)
   (error "No jest-config found. default directory: %s" default-directory)))

(define-compilation-mode jest-test-compilation-mode "Jest Compilation"
  "Compilation mode for Jest output."
  (add-hook 'compilation-filter-hook 'jest-test-colorize-compilation-buffer nil t))

(defun jest-test-colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun jest-test-command (jest-config-dir &optional test-file-name-and-pattern)
  "Create the command to run Jest tests.
TEST-FILE-NAME-AND-PATTERN is a plist with optional
 `:file-name` and `:test-name`."
  (let ((file-name (or (plist-get test-file-name-and-pattern :file-name) ""))
        (test-name (plist-get test-file-name-and-pattern :test-name)))
    (s-trim-right (format
                   "IN_MEMORY_DB=true node --inspect ~/source/grain/node_modules/.bin/jest --runInBand --detectOpenHandles --config %sjest.config.ts %s %s"
                   jest-config-dir
                   file-name
                   (if test-name (format "-t \"%s\"" test-name) "")))))

(defun jest--is-jest-test-call (node)
  "Checks if the given NODE is a Jest test function (describe | it | test)."
  (and (string= (treesit-node-type node) "call_expression")
       (let* ((function-node (treesit-node-child-by-field-name node "function"))
              (function-name (treesit-node-text function-node t)))
         (member function-name '("describe" "it" "test")))))

(defun jest--get-current-test-name ()
  "Extract the current test name using Treesit."
  (when (treesit-ready-p 'typescript)
    (-if-let* ((node (treesit-node-at (point)))
               ;; Walk upwards, stopping on a node that matches the criterion from jest--is-jest-test-call
               (test-node (treesit-parent-until node #'jest--is-jest-test-call))
               ;; Fetch the arguments passed to the function, typically the test description
               (test-name-node (treesit-node-child-by-field-name test-node "arguments"))
               ;; Get the first argument, usually the test description
               (first-arg-node (treesit-node-child test-name-node 1))
               ;; Ensure it's a string literal node
               (node-type (treesit-node-type first-arg-node)))
        (when (string= node-type "string")
          (->>
           (treesit-node-text first-arg-node t)
           (s-chop-prefix "'")
           (s-chop-suffix "'"))))))
