;; Load Perspective

(use-package perspective
  :ensure t
  :init
  (defmacro custom-persp (name &rest body)
    `(let ((initialize (not (gethash ,name perspectives-hash)))
           (current-perspective persp-curr))
       (persp-switch ,name)
       (when initialize ,@body)
       (setq persp-last current-perspective)))
  (defun custom-persp-last ()
    (interactive)
    (persp-switch (persp-name persp-last)))
  :config
  (define-key persp-mode-map (kbd "C-x x l") 'custom-persp-last)
  (persp-mode t))

(provide 'setup-perspective)
