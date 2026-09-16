;;; incremental-code-review.el --- Mentat incremental review adapter -*- lexical-binding: t; -*-

;;; Commentary:
;; Present validated code-review operations through bob-code-review.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bob-code-review)
(require 'mentat-elisp-library)

(mentat--elisp-register-library
 'incremental-code-review 'user
 "Present validated review operations through the Emacs code review interface.")

(defun mentat-code-review--field (operation field index)
  "Return FIELD from OPERATION, or reject review atom INDEX."
  (let ((entry
         (or (assq field operation)
             (cl-find (symbol-name field) operation
                      :key #'car
                      :test (lambda (name key)
                              (and (stringp key) (string= name key)))))))
    (unless entry
      (user-error "Review operation %d is missing %s" index field))
    (cdr entry)))

(defun mentat-code-review--nonempty-string (operation field index)
  "Return nonempty string FIELD from OPERATION at INDEX."
  (let ((value (mentat-code-review--field operation field index)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (user-error "Review operation %d has an invalid %s" index field))
    value))

(defun mentat-code-review--positive-integer (operation field index)
  "Return positive integer FIELD from OPERATION at INDEX."
  (let ((value (mentat-code-review--field operation field index)))
    (unless (and (integerp value) (> value 0))
      (user-error "Review operation %d has an invalid %s" index field))
    value))

(defun mentat-code-review--normalize-operation (operation index)
  "Validate and normalize review OPERATION at INDEX."
  (unless (and (listp operation)
               (cl-every #'consp operation))
    (user-error "Review operation %d must be an object" index))
  (let* ((file (mentat-code-review--nonempty-string operation 'file index))
         (line (mentat-code-review--positive-integer operation 'line index))
         (name (mentat-code-review--nonempty-string operation 'name index))
         (start-line
          (mentat-code-review--positive-integer operation 'start_line index))
         (end-line
          (mentat-code-review--positive-integer operation 'end_line index))
         (description
          (mentat-code-review--nonempty-string operation 'description index))
         (narration
          (mentat-code-review--nonempty-string operation 'narration index))
         (symbols (mentat-code-review--field operation 'symbols index))
         (absolute-file (expand-file-name file)))
    (unless (and (listp symbols)
                 (cl-every (lambda (symbol)
                             (and (stringp symbol)
                                  (not (string-empty-p symbol))))
                           symbols))
      (user-error "Review operation %d has invalid symbols" index))
    (unless (<= start-line line end-line)
      (user-error "Review operation %d has a line outside its span" index))
    (unless (file-regular-p absolute-file)
      (user-error "Review operation %d does not name a file" index))
    (unless (file-readable-p absolute-file)
      (user-error "Review operation %d names an unreadable file" index))
    (list :file absolute-file
          :line line
          :name name
          :start_line start-line
          :end_line end-line
          :description description
          :symbols symbols
          :narration narration)))

(defun mentat-code-review--normalize-operations (operations)
  "Validate JSON OPERATIONS for bob-code-review."
  (unless (and (listp operations) operations)
    (user-error "Expected a nonempty array of review operations"))
  (cl-loop for operation in operations
           for index from 1
           collect (mentat-code-review--normalize-operation operation index)))

(defun mentat-code-review--speak-p (speak)
  "Return whether SPEAK requests audio narration."
  (cond
   ((eq speak t) t)
   ((memq speak '(nil :false :json-false)) nil)
   (t (user-error "SPEAK must be true or false"))))

(mentat-defun mentat-code-review-present (operations &key speak)
  "Present review OPERATIONS in Emacs.
OPERATIONS is a nonempty array of objects with file, line, name, start_line,
end_line, description, symbols, and narration.  File paths may be relative to
the caller directory.  SPEAK defaults to false.  When false, no credential
lookup or audio process is started."
  (:execution async :display "Present Code Review")
  (lambda (resolve reject on-cancel)
    (funcall on-cancel #'ignore)
    (condition-case err
        (let* ((normalized (mentat-code-review--normalize-operations operations))
               (speak-p (mentat-code-review--speak-p speak)))
          (bob-code-review-present normalized nil speak-p)
          (funcall resolve
                   `((operations . ,(length normalized))
                     (speech . ,(if speak-p "started" "disabled")))))
      (error
       (funcall reject (error-message-string err))))))

(provide 'incremental-code-review)
;;; incremental-code-review.el ends here
