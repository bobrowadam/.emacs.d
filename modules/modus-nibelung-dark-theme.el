;;; modus-nibelung-dark-theme.el --- modus-nibelung dark Modus theme -*- lexical-binding: t; -*-

(let ((load-path (cons (file-name-directory (or load-file-name buffer-file-name))
                       load-path)))
  (require 'modus-nibelung))

(modus-themes-theme
 'modus-nibelung-dark
 'modus-nibelung-themes
 "Minimal dark theme built on Modus Themes."
 'dark
 'modus-nibelung-dark-palette
 nil
 'modus-nibelung-dark-palette-overrides
 'modus-nibelung-custom-faces)

;;; modus-nibelung-dark-theme.el ends here
