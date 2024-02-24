(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))
;; (set-frame-font "DaddyTimeMono Nerd Font 19")
;; (add-to-list 'default-frame-alist
;;              '(font . "DaddyTimeMono Nerd Font 19"))
(set-frame-font "FiraCode Nerd Font 21")
(add-to-list 'default-frame-alist
             '(font . "FiraCode Nerd Font 21"))

(defun remote-config-p ()
  (and (boundp 'remote-mode) remote-mode))

(use-package modus-themes
  :demand t
  :config
  (setq modus-vivendi-tinted-palette-overrides
        nil)
  ;; (modus-themes-select 'modus-vivendi-tinted)
  (setq modus-vivendi-tritanopia-palette-overrides 
        '((bg-main "grey6")))
  (modus-themes-select 'modus-vivendi-tritanopia)
  :hook
  (modus-themes-after-load-theme . bobs-modeline/enable))

(use-package ef-themes
  :hook
  (ef-themes-post-load . bobs-modeline/enable))

(use-package spacious-padding
  :demand t
  :config 
  (spacious-padding-mode 1)
  (spacious-padding-set-invisible-dividers 'modus-vivendi-tinted))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package all-the-icons :demand t
  :if (display-graphic-p))

(use-package bobs-modeline
  :after modus-themes
  :ensure nil
  :demand t
  :load-path "~/source/bobs-modeline/"
  :config (bobs-modeline/enable))

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :demand t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(provide 'appearance)
