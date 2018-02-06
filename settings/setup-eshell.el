;;; package --- Summary
;;;; Eshell config
;;; Commentary:
;;; Code:

(use-package eshell
  :init
  (defalias 'emacso 'find-file-other-window)
  (defalias 'emo 'find-file-other-window)
  (defalias 'emacs 'find-file)
  (defalias 'em 'find-file)
  (defalias 'docstart "docker start ${docker ps -a -q")
  (defalias 'status 'magit-status)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "M-r")
                             (lambda ()
                               (interactive)
                               (insert
                                (ido-completing-read "Eshell history: "
                                                     (delete-dups
                                                      (ring-elements eshell-history-ring))))))
              (local-set-key (kbd "C-c C-h") 'eshell-list-history)))

  :config
  (use-package eshell-prompt-extras
    :ensure t
    :config
    (with-eval-after-load "esh-opt"
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-lambda)))

  (use-package shell-pop
    :ensure t
    :init
    (setq shell-pop-window-size 45)
    (setq shell-pop-shell-type '("eshell" "*ehell*"
                                (lambda nil
                                  (eshell))))
    :config
    (global-unset-key (kbd "C-\\"))
    (global-set-key (kbd "C-\\") 'shell-pop))

  (use-package xterm-color
    ;; This might make npm install weird symbols disappear in eshell
    ;; Check it out when you'll have time.
    :disabled
    :ensure t
    :init
    (add-hook 'eshell-mode-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    :config
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  (add-hook 'eshell-mode-hook (lambda ()
                                (smartparens-mode t)))
  ;; Tramp integration:
  (add-to-list 'eshell-modules-list 'eshell-tramp)

  ;; Key bindings: (Maybe use :bind later)
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (global-set-key (kbd "C-c e") 'eshell))

(provide 'setup-eshell)
;;; setup-eshell ends here
