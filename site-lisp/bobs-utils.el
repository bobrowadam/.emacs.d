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

(defun bob/unique (list)
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

(defun bob/gen-tags (n)
  (interactive)
  (cl-loop for x from 0 to n concat
           (if (eq x n)
               (format "\"%s\": \"%s\"" x (to-string (number-sequence 0 10)))
             (format "\"%s\": \"%s\"\n," x (to-string (number-sequence 0 50))))))

(defun bob/run-ctags ()
  "Run ctags -Re on the current directory."
  (interactive)
  (if (executable-find "ctags")
    (shell-command "ctags -Re ." "*ctags-stdout*" "*ctags-stderr*")
    (error "You need to install ctags. (on mac) run 'brew install ctags'"))
  )

(defun bob/calc-sourdough-hydration (levan bakers-hydration-input total-weight)
  (-let* ((bakers-hydration (if (> bakers-hydration-input 1)
                                (/ bakers-hydration-input 100.0)
                              bakers-hydration-input))
          (total-% (+ bakers-hydration 1))
          (water-% (/ bakers-hydration total-%))
          (water-grm (* total-weight water-%))
          (flour-grm (- total-weight water-grm))
          (salt-grm (* 0.02 (+ flour-grm (* 0.5 levan)))))
    `((flour-weight ,(round (- flour-grm (* 0.5 levan)))
      (water-weight ,(round (- water-grm (* 0.5 levan))))
      (salt-grm ,(round salt-grm))))))

(ert-deftest calc-sourdough-hidration ()
  (should (equal (bob/calc-sourdough-hydration 130 0.76 1270)
                 '((flour-weight 650)
                   (water-weight 476)
                   (salt-weight 14)))))

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

;;;###autoload
(defun kill-inspect-process ()
  (interactive)
  (-let ((process (get--processes-by-string "inspect")))
    (if process
      (progn (message "Found inspect processes: %s, killing them now" process)
             (when (equal (shell-command (format "kill %s" (s-join " " process)))
                          0)
               (message "Killed inspect processes: %s" process)))
      (message "No inspect processes found"))))

;;;###autoload
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
    (set-process-sentinel proc (process-generic-sentinel arg))))

(defun run-calendar-sync (arg)
  "Run the \"Sync calendar to org file\" IOS shortcut
to update the local agenda calendar files."
  (interactive "P")
  (message "Running Sync calendar to org file")
  (let ((proc (start-process "Sync Calendar"
                             "*sync-calendar-output*"
                             "/usr/bin/shortcuts"
                             "run"
                             "Sync calendar to org file")))
    (set-process-sentinel proc (process-generic-sentinel arg))))

(defun process-generic-sentinel (&optional arg)
  "Display the buffer containing PROCESS output when it finishes."
  (lambda (process event)
    (when (and (boundp 'arg)
               (memq (process-status process) '(exit signal)))
      (pop-to-buffer (process-buffer process) t t))
    (when (equal (process-name process) "Sync Calendar")
      (bob/reset-org-element-cache-in-agenda-files))
    (message "Process %s %s" process event)))

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

(defun bob/default--display-to-item (display-name hash-table)
  "Map a display name to item name. HASH-TABLE "
  (if-let* ((conflicted-value (gethash display-name
                                       hash-table)))
      (format "%s-%s" conflicted-value (random* 100000))
      display-name))

(cl-defun bob/completing-read (prompt items
                                      &optional
                                      (display-to-item 'bob/default--display-to-item)
                                      &keys file-history)
  (let ((display-map (make-hash-table :test 'equal)))
    (dolist (item items)
      (puthash (funcall display-to-item item display-map)
               item
               display-map))
    (-if-let* ((completing-result (completing-read prompt
                                                   display-map
                                                   nil
                                                   nil
                                                   nil
                                                   file-history))
               (found-value (gethash completing-result
                                     display-map)))
        found-value
      completing-result)))

(defun fahrenheit-to-celsius (fahrenheit)
  "Convert Fahrenheit to Celsius."
  (/ (* (- (float fahrenheit) 32) 5) 9))

(defun celsius-to-fahrenheit (celsius)
  "Convert Celsius to Fahrenheit."
  (+ (/ (* (float celsius) 9) 5) 32))

(defun bob/calc-stabilizers (total-weight)
  "Calculate stabilizers weight"
  (let* ((total (+ 0.8 0.4 0.2))
        (lbg (* total-weight (/ 0.8 total)))
        (guar (* total-weight (/ 0.4 total)))
        (l-carrageenan (* total-weight (/ 0.2 total))))
    (format "LBG: %.2f, Guar: %.2f, Carrageenan: %.2f"
            lbg guar l-carrageenan)))

(defun bob/transpose-surroundings-sexps ()
  "Transpose the surrounding sexps around the current point expression:
a-word center-word b-word =>
b-word center-word a-word"
  (interactive)
  (save-excursion
    (forward-sexp)
    (transpose-sexps 1)
    (backward-sexp)
    (backward-sexp)
    (transpose-sexps 1)
    (transpose-sexps 1)))

(defun bob/eval-to-kill-ring ()
  (interactive)
  (kill-new (with-output-to-string (princ (call-interactively 'eval-expression)))))

(provide 'bobs-utils)
