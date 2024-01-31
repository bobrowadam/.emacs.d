(defun assocdr (key alist)
  "Return the cdr of the cons whose car is KEY in ALIST."
  (cdr (assoc key alist)))

(defun read-file (file-name)
  "Return the contents of FILE-NAME as a lisp data type."
  (when (file-exists-p file-name)
   (with-temp-buffer
     (insert-file-contents file-name)
     (buffer-string))))

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
             (when (equal (shell-command (format "kill %s" (s-join " " process)))
                          0)
               (message "Killed inspect processes: %s" process)))
      (message "No inspect processes found"))))

(defun kill-process-by-regex ()
  (interactive)
  (-let ((process (get--processes-by-string (read-string "Enter regex: "))))
    (if process
      (progn (message "Found processes: %s, killing them now" process)
             (when (equal (shell-command (format "kill %s" (s-join " " process)))
                          0)
               (message "Killed processes: %s" process)))
      (message "No processes found"))))

(defun run-cl-asana (arg)
  "Run cl-asana to update the local org-asana file."
  (interactive "P")
  (message "Running cl asana")
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

(defun kill--all-shell-buffers ()
  ;; Gracefully terminate comint processes before killing buffers
  (cl-loop for buffer in (buffer-list)
           do
           (with-current-buffer buffer
             (when (derived-mode-p 'comint-mode)
               (let ((process (get-buffer-process buffer)))
                 (when (process-live-p process)
                   ;; Gracefully end process before killing the buffer
                   (delete-process process)
                   ;; Now kill the buffer
                   (kill-buffer buffer)))))))

(defun bob/reset-emacs-state ()
  "Reset emacs state to initial state, keeping only Messages and Scratch buffers."
  (interactive)
  (eglot-shutdown-all)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections)
  (kill--all-shell-buffers)

  ;; Kill all other buffers except *Messages* and *scratch*
  (mapc 'kill-buffer (delq (get-buffer "*Messages*") (buffer-list)))
  (delete-other-windows)
  (scratch-buffer))

(defun bob/rotate-list (list)
  "Take a list an rotate it such that:
(rotate (1 2 3 4)) => (4 1 2 3)"
  (cons (car (last list))
        (butlast list)))

(defun seq-partition-by (element->bool sequence)
  "Split the SEQUENCE by a applying ELEMENT->BOOL on each element in SEQUENCE. "
  (let ((groups (seq-group-by
                 (lambda (elt) (if (funcall element->bool elt) t nil))
                 sequence)))
    (mapcar (lambda (group) (cdr group))
            groups)))

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; GC optimizations taken from: https://akrl.sdf.org/#orgc15a10d
;; Set garbage collection threshold to 1GB.
(defun bob/set-gc-configuration ()
  (setq gc-cons-threshold #x40000000)
  ;; When idle for 15sec run the GC no matter what.
  (defvar k-gc-timer
    (run-with-idle-timer 15 t
                         (lambda ()
                           (message "Garbage Collector has run for %.06fsec"
                                    (k-time (garbage-collect)))))))

(provide 'bobs-utils)
