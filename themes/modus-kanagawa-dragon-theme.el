;;; modus-kanagawa-dragon-theme.el --- Kanagawa Dragon Modus theme -*- lexical-binding: t; -*-

(let ((load-path (cons (file-name-directory
                         (or load-file-name buffer-file-name))
                       load-path)))
  (require 'modus-kanagawa))

(modus-themes-theme
 'modus-kanagawa-dragon
 'modus-kanagawa
 "Canonical Kanagawa Dragon palette on Modus Themes."
 'dark
 'modus-vivendi-palette
 'modus-kanagawa-dragon-palette
 'modus-kanagawa-dragon-palette-overrides
 'modus-kanagawa-custom-faces)

(provide-theme 'modus-kanagawa-dragon)
;;; modus-kanagawa-dragon-theme.el ends here
