(defun random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random))
               (length alnum))))
    (substring alnum i (1+ i))))

(defun random-n-letter-string (n)
  (mapconcat 'identity (cl-loop for x to n collect (random-alnum)) ""))

(provide 'misc-funcs)
