(use-package eshell
  :init
  (defalias 'emacso 'find-file-other-window)
  (defalias 'emacs 'find-file)
  (defalias 'docstart "docker start ${docker ps -a -q")
  (defalias 'status 'magit-status)
  
  :config
  (use-package eshell-prompt-extras
    :ensure t
    
    :config
    (with-eval-after-load "esh-opt"
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-lambda)))

  (add-hook 'eshell-mode-hook (lambda () (smartparens-mode t)))
  ;; Tramp integration:
  (add-to-list 'eshell-modules-list 'eshell-tramp)

  ;; Key bindings: (Maybe use :bind later)
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (global-unset-key (kbd "C-\\"))
  (global-set-key (kbd "C-\\") 'shell-pop)
  (global-set-key (kbd "C-c e") 'eshell))

(provide 'setup-eshell)
