;;; modus-nibelung-light-theme.el --- modus-nibelung light Modus theme -*- lexical-binding: t; -*-

(let ((load-path (cons (file-name-directory (or load-file-name buffer-file-name))
                       load-path)))
  (require 'modus-nibelung))

(modus-themes-theme
 'modus-nibelung-light
 'modus-nibelung-themes
 "Minimal light theme built on Modus Themes."
 'light
 'modus-nibelung-light-palette
 nil
 'modus-nibelung-light-palette-overrides
 'modus-nibelung-custom-faces)

;;; modus-nibelung-light-theme.el ends here
