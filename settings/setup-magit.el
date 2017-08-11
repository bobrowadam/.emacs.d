;;;; Magit Configuratinon
;;
(use-package magit
  :ensure t
  :init
  
  ;; Full screen magit-status
  (defun magit-status-fullscreen (prefix)
    (interactive "P")
    (magit-status)
    (unless prefix
      (delete-other-windows)))
  
  ;; don't prompt me
  (set-default 'magit-push-always-verify t)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes unstage-all-changes))

  ;; move cursor into position when entering commit message
  (defun my/magit-cursor-fix ()
    (beginning-of-buffer)
    (when (looking-at "#")
      (forward-line 2)))

  ;; full screen vc-annotate
  (defun vc-annotate-quit ()
    "Restores the previous window configuration and kills the vc-annotate buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :vc-annotate-fullscreen))
  
  :bind
  ("C-x g" . magit-status)
  
  :config
  ;; Add --follow-tags to magit push pop-up
  (magit-define-popup-switch 'magit-push-popup
    ?t "Follow tags" "--follow-tags")
  
  ;; Magithub
  (use-package magithub
    :disabled
    :ensure t
    :config
    (magithub-feature-autoinject t)
    (setq magithub-clone-default-directory (expand-file-name "source" user-home-directory )))
  
  (add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)
  
  (eval-after-load "vc-annotate"
    '(progn
       (defadvice vc-annotate (around fullscreen activate)
         (window-configuration-to-register :vc-annotate-fullscreen)
         ad-do-it
         (delete-other-windows))

       (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit))))

(provide 'setup-magit)
