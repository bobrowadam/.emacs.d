;;; package --- Summary
;;; Code:
;;; Commentary:

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

(defun write-file-with-timestamp (filename)
  "Write FILENAME with timestamp sufix"
  (interactive "sEnter file name:")
  (write-file (format "%s-%s.org" filename (format-time-string "%b-%d-%Y"))))
(format-time-string "")

;; (defun sqrt (ai num n-seq)
;;   "Newton-Raphson Square Roots"
;;   (if n-seq
;;       (let ((ai+1 (/ (+ ai (/ num ai)) 2)))
;;         (sqrt ai+1 num (cdr n-seq)))
;;     ai+1))

(defun to-string (element)
  "Format ELEMENT to string."
  (format "%s" element))

;;;###autoload
(defun my/refresh-google-calendar ()
  "Refresh google calendar org file."
  (interactive)
  (require 'request)
  (let ((file-path (concat org-directory "/google-calendar.org"))
        (tmp-path "/tmp/icalawk")
        (ical-private-url (f-read-text (concat org-directory "/ical-url-path"))))
    (progn
      (f-write-text
       (request-response-data (request
                                ical-private-url
                                :parser 'buffer-string
                                :sync t))
       'utf-8 tmp-path)
      (call-process "ical2org.awk" tmp-path `((:file ,file-path) nil) nil))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun gen-tags (n)
  (interactive)
  (cl-loop for x from 0 to n concat
           (if (eq x n)
               (format "\"%s\": \"%s\"" x (to-string (number-sequence 0 10)))
             (format "\"%s\": \"%s\"\n," x (to-string (number-sequence 0 50))))))

(defun save-scratch ()
  "Save the *scratch* buffer to file."
  (interactive)
  (with-current-buffer "*scratch*"
    (write-file (concat user-emacs-directory "scratch-backup.el"))))
(provide 'misc-funcs)
;;; misc-funcs.el ends here
