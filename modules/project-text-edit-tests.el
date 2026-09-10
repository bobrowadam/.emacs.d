;;; project-text-edit-tests.el --- Edit diagnostics tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'project-text-edit)

(defmacro mentat-text-edit-test-with-file (contents &rest body)
  "Create an Elisp file containing CONTENTS and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let* ((directory (make-temp-file "mentat text edit test " t))
          (file (expand-file-name "sample.el" directory)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,contents))
           ,@body)
       (delete-directory directory t))))

(ert-deftest mentat-text-edit-checks-structure-without-compiling ()
  (mentat-text-edit-test-with-file
      ";;; -*- lexical-binding: t; -*-\n(defun sample () value)\n"
    (cl-letf (((symbol-function 'byte-compile-file)
               (lambda (&rest _) (ert-fail "Editing must not compile"))))
      (dolist (result
               (list
                (mentat-text-edit-replace-once file "value" "missing-value")
                (mentat-text-edit-replace-many
                 file '(("missing-value" "another-missing-value")))
                (mentat-text-edit-insert-after-once
                 file "another-missing-value" " missing-function")))
        (should (string-match-p "Elisp parentheses check passed" result))))))

(ert-deftest mentat-text-edit-reports-elisp-syntax-errors-after-write ()
  (mentat-text-edit-test-with-file
      ";;; -*- lexical-binding: t; -*-\n(defun sample () 1)\n"
    (let ((result
           (mentat-text-edit-replace-once
            file "(defun sample () 1)" "(defun sample () 1")))
      (should (string-match-p "Elisp parentheses check failed at line" result))
      (should (string-match-p "(defun sample () 1$"
                              (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string)))))))

(ert-deftest mentat-text-edit-checks-with-elisp-syntax ()
  (mentat-text-edit-test-with-file
      "; An unmatched ( in a comment is valid.\n(list ?\\( \"(\" 1)\n"
    (should (string-match-p
             "Elisp parentheses check passed"
             (mentat-text-edit-replace-once file "1" "2")))))

(ert-deftest mentat-text-edit-skips-elisp-checks-for-other-files ()
  (mentat-text-edit-test-with-file "unbalanced ("
    (let ((text-file (concat file ".txt")))
      (rename-file file text-file)
      (cl-letf (((symbol-function 'check-parens)
                 (lambda () (ert-fail "Non-Elisp edits must not check parentheses"))))
        (should (string-match-p
                 "updated" (mentat-text-edit-replace-once
                            text-file "unbalanced" "still unbalanced")))))))

(provide 'project-text-edit-tests)
;;; project-text-edit-tests.el ends here
