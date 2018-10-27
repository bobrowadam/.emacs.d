(defun random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random))
               (length alnum))))
    (substring alnum i (1+ i))))

(defun random-n-letter-string (n)
  (mapconcat 'identity (cl-loop for x to n collect (random-alnum)) ""))

(defun my/unique (list)
  "Get a new list with LISTS's unique values."
  (let ((hash-t (make-hash-table :test 'eq)))
    (dolist (n list) (puthash n 1 hash-t))
    (hash-table-keys hash-t)))

(defun my/deep-contains (list element)
  (if (not list)
      nil
    (and (listp list)
         (or (-contains\? list element)
             (my/deep-contains (car list) element)
             (my/deep-contains (cdr list) element)))))

(defmacro λ (body)
  "Short lambda version with implicit argument list.
Arguments are _ _1 _2 ... _1000 and are ordered in ascending order.
You can use any of those arguments in BODY"
  (let* ((lambda-args (cons '_ (mapcar (lambda (_)
                                         (intern (format "_%s" _)))
                                       (number-sequence 1 1000))))
         (args (my/unique (-filter
                           (lambda (arg) (progn arg))
                           (mapcar (lambda (arg)
                                     (if (my/deep-contains body arg)
                                         arg
                                       nil))
                                   lambda-args)))))
    `(lambda ,args ,body)))

(defmacro sformat (string)
  "Smart formating a STRING.
To refer a variable in STRING use `$ as prefix.
You can escape '$' with '\\' as prefix.
(setq name \"adam\")
(sformat \"my name is $name\" => \"my name is Adam>\")
(sformat \"my name is \\\\$name\" => \"my name is $name>\")."
  (let* ((words (s-split " " string))
         ($-words (-filter (λ (equal (substring _ 0 1) "$")) words))
         ($-args (mapcar (λ (intern (substring _ 1))) $-words))
         (words-with-% (mapcar (λ (if (-contains? $-words _) "%s" _))
                               words))
         (string-with-% (replace-regexp-in-string "\\\\" "" (mapconcat (λ (progn _)) words-with-% " "))))
    `(format ,string-with-% ,@$-args)))

(provide 'misc-funcs)
