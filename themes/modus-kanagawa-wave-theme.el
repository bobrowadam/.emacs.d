;;; modus-kanagawa-wave-theme.el --- Kanagawa Wave Modus theme -*- lexical-binding: t; -*-

(let ((load-path (cons (file-name-directory
                         (or load-file-name buffer-file-name))
                       load-path)))
  (require 'modus-kanagawa))

(modus-themes-theme
 'modus-kanagawa-wave
 'modus-kanagawa
 "Canonical Kanagawa Wave palette on Modus Themes."
 'dark
 'modus-vivendi-palette
 'modus-kanagawa-wave-palette
 'modus-kanagawa-wave-palette-overrides
 'modus-kanagawa-custom-faces)

(provide-theme 'modus-kanagawa-wave)
;;; modus-kanagawa-wave-theme.el ends here
