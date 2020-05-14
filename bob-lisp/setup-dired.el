(use-package dired
  :config
  (setq dired-use-ls-dired nil)
  (setq dired-listing-switches "-alh")
  :ensure nil
  )

(use-package dired-x :ensure nil :demand t)

(use-package dired-aux
  :demand t
  :ensure nil
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  :bind (:map dired-mode-map
              ("C-+" . dired-create-empty-file)
              ("M-s f" . nil)))

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<backtab>" . dired-subtree-remove)))

(use-package diredfl
  :ensure t
  :init (diredfl-global-mode))

(use-package dired-rsync
  :init
  (setq dired-rsync-passphrase-stall-regex "Verification code")
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(use-package dired-du)

(provide 'setup-dired)
