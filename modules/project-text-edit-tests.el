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

(ert-deftest mentat-text-edit-reports-passing-elisp-diagnostics ()
  (mentat-text-edit-test-with-file
      ";;; -*- lexical-binding: t; -*-\n(defun sample () 1)\n"
    (let ((compiled (byte-compile-dest-file file)))
      (with-temp-file compiled (insert "preserve"))
      (let ((result (mentat-text-edit-replace-once file "1" "2")))
        (should (string-match-p "Elisp diagnostics passed" result))
        (should (equal (with-temp-buffer
                         (insert-file-contents compiled)
                         (buffer-string))
                       "preserve"))))))

(ert-deftest mentat-text-edit-reports-elisp-syntax-errors-after-write ()
  (mentat-text-edit-test-with-file
      ";;; -*- lexical-binding: t; -*-\n(defun sample () 1)\n"
    (let ((result
           (mentat-text-edit-replace-once
            file "(defun sample () 1)" "(defun sample () 1")))
      (should (string-match-p "Elisp diagnostics failed at line" result))
      (should (string-match-p "(defun sample () 1$"
                              (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string)))))))

(ert-deftest mentat-text-edit-reports-strict-compiler-warnings ()
  (mentat-text-edit-test-with-file
      ";;; -*- lexical-binding: t; -*-\n(defun sample () value)\n"
    (let ((result
           (mentat-text-edit-replace-once file "value" "missing-value")))
      (should (string-match-p "Elisp diagnostics failed" result))
      (should (string-match-p "free variable.*missing-value" result)))))

(provide 'project-text-edit-tests)
;;; project-text-edit-tests.el ends here
