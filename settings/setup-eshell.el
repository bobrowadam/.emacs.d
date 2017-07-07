(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))
(defalias 'openo 'find-file-other-window)

;; Tramp integration:
;; (add-to-list 'eshell-modules-list 'eshell-tramp)

;; Key bindings:
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-\\") 'shell-pop)
(global-set-key (kbd "C-c e") 'eshell)

(provide 'setup-eshell)
