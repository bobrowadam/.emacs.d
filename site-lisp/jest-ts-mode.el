(defvar *latest-test* nil)

;;;###autoload
(defun jest-ts-mode/run-test-on-point ()
  "Run the enclosing test"
  (interactive)
  (if-let ((default-directory (locate-dominating-file "./" "jest.config.ts"))
           (test-name (jest--get-current-test-name))
           (test-file-name (buffer-file-name)))
      (progn (setq *latest-test* (list test-file-name test-name default-directory))
             (compile (jest--test-command
                       default-directory
                       `(:file-name ,test-file-name :test-name ,test-name))
                      'jest-test-compilation-mode))
    (error "No jest-config found. default directory: %s" default-directory)))

;;;###autoload
(defun jest-ts-mode/rerun-latest-test ()
  "Run the latest test when exists."
  (interactive)
  (when *latest-test*
    (if-let ((default-directory (nth 2 *latest-test*)))
        (compile (jest--test-command
                  default-directory
                  `(:file-name ,(car *latest-test*) :test-name ,(nth 1 *latest-test*)))
                 'jest-test-compilation-mode)
      (error "No jest-config found. default directory: %s" default-directory))))

;;;###autoload
(defun jest-ts-mode/run-tests (describe-only)
  "Run a specific test from the current file"
  (interactive "P")
  (if-let ((default-directory (locate-dominating-file "./" "jest.config.ts"))
           (test-name (jest--choose-test-with-completion describe-only))
           (test-file-name (buffer-file-name)))
      (progn (setq *latest-test* (list test-file-name test-name default-directory))
             (compile (jest--test-command
                       default-directory
                       `(:file-name ,test-file-name :test-name ,test-name))
                      'jest-test-compilation-mode))
    (error "No jest-config found. default directory: %s" default-directory)))

(define-compilation-mode jest-test-compilation-mode "Jest Compilation"
  "Compilation mode for Jest output."
  (add-hook 'compilation-filter-hook 'jest-test-colorize-compilation-buffer nil t))

(defun jest-test-colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun jest--test-command (jest-config-dir &optional test-file-name-and-pattern)
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
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (when (treesit-ready-p 'typescript)
      (-if-let* ((node (treesit-node-at (point)))
                 (test-node (treesit-parent-until node #'jest--is-jest-test-call))
                 (test-name-node (treesit-node-child-by-field-name test-node "arguments"))
                 (first-arg-node (treesit-node-child test-name-node 1))
                 (node-type (treesit-node-type first-arg-node)))
          (when (string= node-type "string")
            (jest--prepare-test-matching-string (treesit-node-text first-arg-node t)))))))

(defun jest--prepare-test-matching-string (test-name)
  (->>
   test-name
   (s-chop-prefix "'")
   (s-chop-suffix "'")
   (s-chop-suffix "]")
   (s-chop-prefix "[")))

(defun jest--choose-test-with-completion (&optional describe-only)
  (if-let* ((candidates (jest--extract-tests-strings describe-only))
            (display-map (mapcar (lambda (candidate)
                                   (let ((type (car candidate))
                                         (text (cdr candidate)))
                                     (cons (jest--prepare-for-display type text)
                                           text)))
                                 candidates)))
    (let ((chosen-display (completing-read "Choose: "
                                           (mapcar (lambda (d) (car d))
                                                   display-map)
                                           nil t)))
      (jest--prepare-test-matching-string (cdr (assoc chosen-display display-map))))
    (error "No tests definition found in file")))

(defun jest--prepare-for-display (type text)
  (format "%s %s"
          type
          (->> text
               (s-chop-prefix "'")
               (s-chop-suffix "'"))))

(defun jest--extract-tests-strings (&optional describe-only)
  "Extract all strings within `describe` and `test` calls in the current buffer."
  (let ((queries '((call_expression
                    function: (identifier) @func-name
                    arguments: (arguments
                                (string _))))))
    (let* ((parser (treesit-parser-create 'typescript))
           (captures (treesit-query-capture parser queries)))
      (cl-loop for capture in captures
               for text = (treesit-node-text (cdr capture))
               for prefix = (cond ((string-match "describe" text) "")
                                  ((and (not describe-only)
                                        (string-match "test" text)) "✅"))
               when prefix
               collect (cons prefix 
                             (substring-no-properties
                              (treesit-node-text 
                               (treesit-node-child 
                                (cadr 
                                 (treesit-node-children 
                                  (treesit-node-parent 
                                   (cdr capture)))) 1))))))))

(provide 'jest-ts-mode)

