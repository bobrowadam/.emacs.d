(require 'f)

(defconst emacs-uptime-file (format "%s.emacs-uptime" user-emacs-directory))

(defun date-to-uptime ()
  (cons (current-time)
   (string-to-number (emacs-uptime "%s"))))

(defun sort--cdrs (cons-list sort-by)
  (-sort (lambda (x y) (funcall sort-by (cdr x) (cdr y)))
        cons-list))

(defun save-emacs-uptime ()
  (interactive)
  (let ((uptime (emacs-uptime "%s"))
        (file emacs-uptime-file))
    (f-append (format "%s\n" (date-to-uptime)) 'utf-8 emacs-uptime-file)))

(add-hook 'kill-emacs-hook #'save-emacs-uptime)

(defun format-session-uptime (suffix session-uptime)
  (format "%s session ended at %s and lasted %s"
          suffix
          (format-time-string "%Y-%m-%d %H:%M:%S" (car session-uptime))
          (seconds-to-string (cdr session-uptime))))

(defun read-sessions-file ()
  (read (format "(%s)" (f-read emacs-uptime-file))))

(defun is-equal-uptime-session (uptime-1 uptime-2)
  (and (equal (cdr uptime-1)
              (cdr uptime-2) )
       (equal (car uptime-1)
              (car uptime-2))))

(defun print-sessions-duration (emacs-sessions-uptime)
  (let* ((latest-session (car (last emacs-sessions-uptime)))
         (longest-session (car (sort--cdrs emacs-sessions-uptime #'>))))
    (if (is-equal-uptime-session latest-session longest-session)
        (format "Hi 👋\n⏲ Your latest session was also the longest!\n🏆 %s"
                (format-session-uptime "Latest" latest-session))
        (format "Hi 👋\n⏲ %s\n🏆 %s"
             (format-session-uptime "Latest" latest-session)
             (format-session-uptime "Longest" longest-session)))))

(run-with-timer (* 60 15) t 'save-emacs-uptime)

(provide 'emacs-uptime)
