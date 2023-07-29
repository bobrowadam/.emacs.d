(defun assocdr (key alist)
  "Return the cdr of the cons whose car is KEY in ALIST."
  (cdr (assoc key alist)))

(defun read-file (file-name)
  "Return the contents of FILE-NAME as a lisp data type."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

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

(defmacro sformat (string)
  "Smart formating a STRING.
To refer a variable in STRING use `$ as prefix.
You can escape '$' with '\\' as prefix.
(setq name \"adam\")
(sformat \"my name is $name\" => \"my name is Adam>\")
(sformat \"my name is \\\\$name\" => \"my name is $name>\")."
  (let* ((words (s-split " " string))
         ($-words (-filter (λ (equal (substring %1 0 1) "$")) words))
         ($-args (mapcar (λ (intern (substring %1 1))) $-words))
         (words-with-% (mapcar (λ (if (-contains? $-words %1) "%s" %1))
                               words))
         (string-with-% (replace-regexp-in-string "\\\\" "" (mapconcat (λ (progn %1)) words-with-% " "))))
    `(format ,string-with-% ,@$-args)))

(defun to-string (element)
  "Format ELEMENT to string."
  (format "%s" element))

(defmacro time (&rest body)
  `(let ((time (current-time))
         (res ,@body))
     (progn
       (message (format "Eval time for %s is %.06f" ',@body
                        (float-time (time-since time))))
       res)))

(defun gen-tags (n)
  (interactive)
  (cl-loop for x from 0 to n concat
           (if (eq x n)
               (format "\"%s\": \"%s\"" x (to-string (number-sequence 0 10)))
             (format "\"%s\": \"%s\"\n," x (to-string (number-sequence 0 50))))))

(defun calc-sourdough-hidration (levan flour water)
  (-let* ((total-flour (+ (* levan 0.5) flour))
          (total-water (+ (* levan 0.5) water))
          (total-dough (+ levan flour water))
          (final-bread-weight (* total-dough 0.8))
          (hidration (* 100 (/ total-water total-flour)))
          (salt-weight (* 0.02 total-flour)))
    (format "Hidration: %d%%
Total flour weight: %d grams
Total Water weight: %d grams
Total dough weight: %d grams
Final bread weight: %d grams
Recomended Salt weight: %.1f grams" hidration total-flour total-water total-dough final-bread-weight salt-weight)))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun get-processes-by-string (captured-string)
  (interactive "sEnter a string to caputre: ")
  (print (format "%s" (get--processes-by-string captured-string))))

(defun get--processes-by-string (captured-string)
  (-let [lines (-filter
                (lambda (str)
                  (not (string-match-p (regexp-quote "rg") str)))
                (s-split "\n" (shell-command-to-string (format "ps aux | rg %s" captured-string)) t))]
    (mapcar (lambda (line)
              (nth 1 (s-split " " line t)))
            lines)))

(defun kill-inspect-process ()
  (interactive)
  (-let ((process (get--processes-by-string "inspect")))
    (if process
      (progn (message "Found inspect processes: %s, killing them now" process)
             (shell-command (format "kill %s" (s-join " " process))))
      (message "No inspect processes found"))))

(defun run-cl-asana (arg)
  "Run cl-asana to update the local org-asana file."
  (interactive "P")
  (let ((proc (start-process "cl-asana" "*cl-asana-output*" "~/source/common-lisp/cl-asana/cl-asana")))
    (set-process-sentinel proc (run-cl-asana-sentinel arg))))

(defun run-cl-asana-sentinel (&optional arg)
  "Display the buffer containing PROCESS output when it finishes."
  (lambda (process event)
    (when (and (boundp 'arg)
               (memq (process-status process) '(exit signal)))
      (pop-to-buffer (process-buffer process) t t))
    (message "Process %s %s" process event)))

(defun project-switch-to-open-project (dir)
  "\"Switch\" to another ACTIVE project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (completing-read "Choose a project: " (get--active-projects))))
  (let ((command (if (symbolp project-switch-commands)
                     project-switch-commands
                   (project--switch-project-command))))
    (let ((project-current-directory-override dir))
      (call-interactively command))))

(defun get--active-projects ()
  "Return a list of projects that are associated with the open buffers."
  (cl-remove-if-not 'identity
           (mapcar
            #'project--buffer
            (buffer-list))))

(defun project--buffer (buffer)
  (let ((buffer-file (buffer-file-name buffer)))
    (when buffer-file
      (let ((default-directory (file-name-directory buffer-file)))
        (when (project-current)
          (project-root (project-current)))))))

(defun bob/reset-emacs-state ()
  "Reset emacs state to initial state, keeping only Messages and Scratch buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (mapc 'kill-buffer (delq (get-buffer "*Messages*") (buffer-list)))
  (delete-other-windows)
  (setq initial-buffer-choice t)
  (setq initial-scratch-message ""))

(defun bob/rotate-list (list)
  "Take a list an rotate it such that:
(rotate (1 2 3 4)) => (4 1 2 3)"
  (cons (car (last list))
        (butlast list)))

(provide 'bobs-utils)
