;;; modus-kanagawa-lotus-theme.el --- Kanagawa Lotus Modus theme -*- lexical-binding: t; -*-

(let ((load-path (cons (file-name-directory
                         (or load-file-name buffer-file-name))
                       load-path)))
  (require 'modus-kanagawa))

(modus-themes-theme
 'modus-kanagawa-lotus
 'modus-kanagawa
 "Canonical Kanagawa Lotus palette on Modus Themes."
 'light
 'modus-operandi-palette
 'modus-kanagawa-lotus-palette
 'modus-kanagawa-lotus-palette-overrides
 'modus-kanagawa-custom-faces)

(provide-theme 'modus-kanagawa-lotus)
;;; modus-kanagawa-lotus-theme.el ends here
