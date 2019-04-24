(use-package ibuffer-tramp :ensure t :demand t)
(use-package ibuffer
  :demand t
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer . ibuffer-tramp-set-filter-groups-by-tramp-connection)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil))

(provide 'ibuffer-setup)
