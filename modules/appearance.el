(setq custom-safe-themes t)
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(defun set-font-based-on-resolution (frame)
  (message "setting font properties based on frame resolution")
  (let* ((attrs (frame-monitor-attributes))
         (geo (cdr (assq 'geometry attrs))))
    (if (and geo (>= (length geo) 4))
        (let* ((width (nth 2 geo))
                (height (nth 3 geo))
                (font-size
                 (cond
                  ((and (> width 1920) (> height 1080)) 29)  ; Large screens
                  ((and (> width 1366) (> height 768)) 23)   ; Medium screens
                  (t 17))))    ; Small screens
          (let ((font-string (format "Iosevka-%d:weight=medium:width=expanded" font-size)))
            (set-frame-font font-string 'keep-size t)
            (set-frame-font font-string)
            (add-to-list 'default-frame-alist `(font . ,font-string))))
      (message "Error: Unable to determine screen dimensions."))))

(setq move-frame-functions '(set-font-based-on-resolution))

(defun remote-config-p ()
  (and (boundp 'remote-mode) remote-mode))

(use-package modus-themes
  :disabled t
  :custom
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tritanopia))
  :config
  (setq modus-vivendi-tinted-palette-overrides
        nil)
  (setq modus-vivendi-tritanopia-palette-overrides 
        '((bg-main "grey12")
          (shadow "blue5")))
  (setq modus-vivendi-deuteranopia-palette-overrides
        '((bg-main "grey12")
          (forge-topic-slug-open "blue4")))
  (modus-themes-select 'modus-vivendi-tritanopia)
  ;; :hook
  ;; (modus-themes-after-load-theme . bobs-modeline/enable)
)

(use-package doom-themes
  :demand t
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "#59c2ff"))))
  :custom
  ;; maybe use doom-challenger-deep. it's nice
  (doom-ayu-dark-padded-modeline t)
  (doom-ayu-dark-brighter-comments t)
  (doom-ayu-dark-brighter-modeline nil)
  (doom-ayu-dark-comment-bg nil)

  :config
  (load-theme 'doom-ayu-dark)
  ;; (load-theme 'doom-pine)
  ;; (doom-themes-set-faces 'user '(default :background "grey8"))
)

(use-package ef-themes
  :disabled t
  :custom
  (ef-themes-to-toggle '(ef-cyprus ef-deuteranopia-dark))
  :hook
  (ef-themes-post-load . bobs-modeline/enable))

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
  :custom
  (highlight-indent-guides-auto-even-face-perc 45)
  (highlight-indent-guides-auto-odd-face-perc 10)
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled t)
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package bobs-modeline
  :disabled t
  :after modus-themes
  :ensure nil
  :demand t
  :load-path "~/source/bobs-modeline/"
  :config (bobs-modeline/enable))

;; (defun bobs/hide-mode-line ()
;;   (setq-local mode-line-format nil))

;; (add-hook 'comint-mode-hook 'bobs/hide-mode-line)

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
  :demand t
  :custom
  (doom-modeline-percent-position nil)
  (doom-modeline-time-icon nil)
  (doom-modeline-time nil)
  (doom-modeline-buffer-encoding nil)

  :config
  (doom-modeline-mode 1))

;; (window-divider-mode 1)
;; (setq window-divider-default-places t)

(provide 'appearance)
