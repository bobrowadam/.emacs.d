(use-package ibuffer
  :demand t
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil))

;; (use-package ibuffer-tramp :ensure t :demand t)

(use-package ibuffer-projectile
  :if (window-system)
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))

(provide 'ibuffer-setup)
