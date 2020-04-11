(require 'dired)
(require 'dired-x)
(setq dired-use-ls-dired nil)
(setq dired-listing-switches "-alh")

(use-package dired-aux
  :ensure nil
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  :bind (:map dired-mode-map
              ("C-+" . dired-create-empty-file)
              ("M-s f" . nil)))

(provide 'setup-dired)
