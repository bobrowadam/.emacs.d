;;; package --- Summary
;;; Commentary:
;; Functions for shell actions
;;; Code:
(require 'dash)
(require 'subr-x)

(defun get-dir-name (&optional path)
  "Get the current directly name on PATH."
  (file-name-nondirectory
     (directory-file-name
      (file-name-directory path))))

(defun bob/vterm-other-window (&optional buffer-name)
  "Create a new vterm buffer with BUFFER-NAME."
  (let ((buffer buffer-name))
    (unless (get-buffer buffer)
        (generate-new-buffer buffer)
     (with-current-buffer buffer
       (vterm-mode)))
    (pop-to-buffer buffer)))

(defun bob/vterm (&optional user-shell-name)
  "Open shell on current project with.
use USER-SHELL-NAME for buffer name"
  (interactive)
  (let ((shell-name (format "*SHELL*::%s" (upcase (abbreviate-file-name default-directory)))))
    (bob/vterm-other-window shell-name)))

(defun bob/projectile-run-vterm (&optional arg)
  "Invoke `shell' in the project's root.
Switch to the project specific shell buffer if it already exists.
Passing a prefix arg 0 will allow adding a custom suffix to the shell buffer name.
Any other prefis will be used as the suffix itself."
  (interactive "P")
  (-let [suffix (cond ((eq arg 0) (read-string "enter custom suffix: "))
                      (arg (to-string arg))
                      (t ""))]
   (if (projectile-project-p)
       (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
         (bob/vterm-other-window (concat "*SHELL " (projectile-project-name) "* " suffix)))
     (vterm))))

(defvar bob/last-shell-buffer nil)
(defun bob/jump-to-shell ()
  "Jump to a shell buffer."
  (interactive)
  (if-let* ((shell-buffers (bob/drop-current-shell-buffer
                            (set-last-shell-buffer-as-first
                             (seq-filter
                              (lambda (b) (or (s-contains\? "*shell*" b  t)
                                              (s-contains\? "*shell" b  t)
                                              (s-contains\? "*eshell" b  t)
                                              (s-contains\? "*sbt*" b  t)
                                              (s-contains\? "vterm<" b  t)
                                              ))
                              (mapcar (function buffer-name) (buffer-list))))))
            (shell-buffer (ivy-completing-read "Shell: " shell-buffers)))
      (progn
        (setq bob/last-shell-buffer shell-buffer)
       (switch-to-buffer shell-buffer))
    (message "No Shell bufers exists")))

(defun bob/drop-current-shell-buffer (buffers)
  (seq-remove (lambda (buf) (cond
                             ((equal buf (buffer-name)) t)))
              buffers))

(defun set-last-shell-buffer-as-first (buffers)
  (seq-sort (lambda (a b)
              (equal a bob/last-shell-buffer))
            buffers))

(provide 'shell-defuns)
;;; shell-defuns.el ends here
