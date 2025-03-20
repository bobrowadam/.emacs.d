(defun bob/active--projects-buffers ()
  "Get an alist of project-name -> list of file visiting buffers."
  (->> (-group-by
        (lambda (buff)
          (with-current-buffer buff
            (project-current)))
        (-filter #'buffer-file-name (buffer-list)))
       (-filter #'car)
       (mapcar (lambda (projcet-buffers)
                 (cons (project-root (car projcet-buffers))
                       (list (cdr projcet-buffers)))))))

(defun bob/switch-to-open-project-buffer ()
  (interactive)
  (let* ((project-to-buffers (bob/active--projects-buffers))
        (some-project-buffer (caar (assocdr
                                    (completing-read "Switch to open project:" project-to-buffers)
                                    project-to-buffers))))
    (with-current-buffer some-project-buffer
      (consult-project-buffer))))

(provide 'project-extras)
