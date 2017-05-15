(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-default-todo-file (concat org-directory "/todo.org"))

(defun org-go-to-notes()
  (interactive)
  (find-file org-default-notes-file))

(defun org-go-to-todos()
  (interactive)
  (find-file org-default-todo-file))

(define-key global-map (kbd "C-c o c") 'org-capture)
(define-key global-map (kbd "C-c o n") 'org-go-to-notes)
(define-key global-map (kbd "C-c o a") 'org-agenda)
(define-key global-map (kbd "C-c o t") 'org-go-to-todos)

;;Code highlight:
;; (require 'color)
;; (set-face-attribute 'org-block nil :background
;;                     (color-darken-name
;;                      (face-attribute 'default :background) 3))


(provide 'setup-org)
