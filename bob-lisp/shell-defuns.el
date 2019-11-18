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
(generate-new-buffer "vterm")

(defun bob/vterm-other-window (buffer-name)
  "Create a new vterm buffer with BUFFER-NAME."
  (let ((buffer buffer-name))
    (generate-new-buffer buffer)
    (with-current-buffer buffer
      (vterm-mode))
    (pop-to-buffer buffer)))

(defun bob/shell-in-project (&optional user-shell-name)
  "Open shell on current project with.
use USER-SHELL-NAME for buffer name"
  (interactive)
  (let ((shell-name (format "*SHELL*::%s" (upcase (get-dir-name
                                                   (or (projectile-project-root)
                                                       default-directory))))))
    (bob/vterm-other-window shell-name)))

(defun bob/jump-to-shell ()
  "Jump to a shell buffer."
  (interactive)
  (if-let* ((shell-buffers (seq-filter
                            (lambda (b) (or (s-contains\? "*SHELL*" b  t)
                                            (s-contains\? "*eshell" b  t)
                                            (s-contains\? "*sbt*" b  t)
                                            (s-contains\? "vterm<" b  t)))
                            (mapcar (function buffer-name) (buffer-list))))
            (shell-buffer (ivy-completing-read "Shell: " shell-buffers)))
      (switch-to-buffer shell-buffer)
    (message "No Shell bufers exists")))


(provide 'shell-defuns)
;;; shell-defuns.el ends here
