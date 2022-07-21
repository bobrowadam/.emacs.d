;;; package --- Summary
;;; Commentary:
;; Functions for shell actions
;;; Code:

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
                      (arg (number-to-string arg))
                      (t ""))]
    (delete-other-windows)
    (split-window-sensibly)
    (other-window 1)
    (if (projectile-project-p)
        (projectile-run-vterm)
      (vterm))))

(defvar bob/last-shell-buffer nil)

(defun bob/jump-to-shell ()
  "Jump to a shell buffer."
  (interactive)
  (if-let* ((shell-buffers
             (bob/drop-buffer
              (set-last-shell-buffer-as-first
               (seq-filter
                (lambda (b) (or (equal (with-current-buffer b major-mode) 'vterm-mode)
                                (equal (with-current-buffer b major-mode) 'eshell-mode)
                                (equal (with-current-buffer b major-mode) 'shell-mode)
                                (equal (with-current-buffer b major-mode) 'js-comint-mode)
                                (equal (with-current-buffer b major-mode) 'sly-mrepl-mode)
                                ))
                (mapcar (function buffer-name) (buffer-list))
                ;; (flatten-tree (mapcar (λ (buffer-name %1)) (persp-get-buffers)))
                ))))
            (shell-buffer (completing-read "Shell: " shell-buffers)))
      (progn
        (setq bob/last-shell-buffer shell-buffer)
        (switch-to-buffer shell-buffer))
    (message "No Shell bufers exists")))

(defun bob/magit-buffers ()
  "Jump to a magit buffer."
  (interactive)
  (if-let* ((magit-buffers
             (bob/drop-buffer
              (set-last-magit-buffer-as-first
               (seq-filter
                (lambda (b) (or (equal (with-current-buffer b major-mode) 'magit-status-mode)))
                (mapcar (function buffer-name) (buffer-list))))))
            (magit-buffer (completing-read "Magit: " magit-buffers)))
      (progn
        (setq bob/last-magit-buffer magit-buffer)
        (switch-to-buffer magit-buffer))
    (message "No Magit buffers exists")))


(defun bob/drop-buffer (buffers)
  (seq-remove (lambda (buf)
                (cond ((equal buf (buffer-name)) t)))
              buffers))

(defun set-last-shell-buffer-as-first (buffers)
  (seq-sort (lambda (a b)
              (equal a bob/last-shell-buffer))
            buffers))

(defun set-last-magit-buffer-as-first (buffers)
  (seq-sort (lambda (a b)
              (equal a bob/last-shell-buffer))
            buffers))

(defun import-customer (customer-id)
  (interactive "N")
  (-let [default-directory (format "%s/source/services/catapult" (getenv "HOME"))]
    (async-shell-command (format "npm run import-customer %s" customer-id))))

(provide 'shell-defuns)
;;; shell-defuns.el ends here
