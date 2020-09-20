(use-package smex)

(use-package amx
  :after ivy
  :disabled t
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/.emacs.d/amx-items")
  (amx-history-length 50)
  (amx-show-key-bindings t)
  :config
  (amx-mode 1))

;; (use-package ivy
;;   :demand t
;;   :config
;;   (setq ivy-re-builders-alist
;;         '((counsel-rg . ivy--regex-plus)
;;           (t . ivy--regex-fuzzy)
;;           ;; (t . ivy-prescient-re-builder)
;;           ))
;;   (setq ivy-use-selectable-prompt t)
;;   (setq ivy-initial-inputs-alist nil)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d%d) ")
;;   (ivy-mode 1))

(use-package ivy
  :commands ivy-mode
  :hook (after-init . ivy-mode)
  :delight ivy-mode
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done)
              ("RET" . ivy-alt-done))
  :config
  (ivy-prescient-mode t)
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function nil
        ivy-use-selectable-prompt t
        enable-recursive-minibuffers t
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t . ivy-prescient-re-builder))))

(use-package ivy-prescient
  :commands ivy-prescient-mode
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (prescient-persist-mode t))

(use-package ivy-posframe
  :disabled t
  :demand t
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
          (counsel-find-file . ivy-posframe-display-at-window-bottom-left)
          (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
          (projectile-switch-to-buffer . ivy-posframe-display-at-frame-center)
          (t               . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package counsel
  :if (window-system)
  ;; :after ivy
  ;; :init
  ;; (setq counsel-rg-base-command
  ;;       "rg --heading --context-separator \" \" -i -M 120 --line-number --color never %s .")
  ;; :config
  ;; (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  )

(provide 'setup-ivy)
