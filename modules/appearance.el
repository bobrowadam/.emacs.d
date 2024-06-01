(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))
;; (set-frame-font "DaddyTimeMono Nerd Font 19")
;; (add-to-list 'default-frame-alist
;;              '(font . "DaddyTimeMono Nerd Font 19"))
(set-frame-font "FiraMono Nerd Font 21")
(add-to-list 'default-frame-alist
             '(font . "FiraMono Nerd Font 21"))

(defun remote-config-p ()
  (and (boundp 'remote-mode) remote-mode))
;; (load-theme 'bobs-badger)

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-to-toggle '( modus-operandi-tinted modus-vivendi-tinted))
  :config
  (setq modus-vivendi-tinted-palette-overrides
        nil)
  (modus-themes-select 'modus-vivendi-tinted)
  (setq modus-vivendi-tritanopia-palette-overrides 
        '((bg-main "grey6")))
  ;; (modus-themes-select 'modus-vivendi-deuteranopia)
  :hook
  (modus-themes-after-load-theme . bobs-modeline/enable))

(use-package ef-themes
  :demand t
  :custom
  (ef-themes-to-toggle '(ef-cyprus ef-deuteranopia-dark))
  :hook
  (ef-themes-post-load . bobs-modeline/enable)
  ;; :config
  ;; (ef-themes-select 'ef-deuteranopia-dark)
)

(use-package spacious-padding
  :demand t
  :custom
  (spacious-padding-widths
   '(:internal-border-width 15
                            :header-line-width 4
                            :mode-line-width 6
                            :tab-width 4
                            :right-divider-width 1
                            :scroll-bar-width 8
                            :fringe-width 8))
  :config 
  (spacious-padding-mode))

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

;; (defun bobs/hide-mode-line ()
;;   (setq-local mode-line-format nil))

;; (add-hook 'comint-mode-hook 'bobs/hide-mode-line)

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

(use-package doom-modeline
  :disabled t
  :init (doom-modeline-mode 1))

;; (window-divider-mode 1)
;; (setq window-divider-default-places t)

(provide 'appearance)
