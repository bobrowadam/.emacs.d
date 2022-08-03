(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face 'default)
 '(doom-modeline-github t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-height 15 nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-icon t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-lsp t)
 '(doom-modeline-major-mode-color-icon t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-minor-modes nil nil nil "Customized with use-package doom-modeline")
 '(flyspell-auto-correct-binding [(control 59)])
 '(ignored-local-variable-values
   '((mmm-classes . elisp-js)
     (eval mmm-add-group 'elisp-js
           '((elisp-rawjs :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "xwwp--js \"" :back "\" js--")
             (elisp-defjs :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "xwwp-js-def .*\12.*\"\"\12" :back "\")\12")))
     (Base . 10)
     (Package . CL-USER)
     (Syntax . COMMON-LISP)))
 '(lsp-restart 'ignore)
 '(lsp-vetur-dev-log-level "DEBUG" t)
 '(magit-diff-use-overlays nil)
 '(magit-todos-group-by '(magit-todos-item-filename magit-todos-item-keyword))
 '(org-agenda-files nil)
 '(pos-tip-background-color "#FFF9DC")
 '(pos-tip-foreground-color "#011627")
 '(proced-format 'verbose)
 '(safe-local-variable-values
   '((eval mmm-add-group 'elisp-js
           '((elisp-rawjs :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "xwwp--js \"" :back "\" js--")
             (elisp-defjs :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "xwwp-defjs .*\12.*\"\"\12" :back "\")\12")))
     (whitespace-style quote
                       (face trailing empty tabs))
     (whitespace-action)
     (Syntax . ANSI-Common-Lisp)
     (eval cl-flet
           ((enhance-imenu-lisp
             (&rest keywords)
             (dolist
                 (keyword keywords)
               (add-to-list 'lisp-imenu-generic-expression
                            (list
                             (purecopy
                              (concat
                               (capitalize keyword)
                               (if
                                   (string=
                                    (substring-no-properties keyword -1)
                                    "s")
                                   "es" "s")))
                             (purecopy
                              (concat "^\\s-*("
                                      (regexp-opt
                                       (list
                                        (concat "define-" keyword))
                                       t)
                                      "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                             2)))))
           (enhance-imenu-lisp "bookmarklet-command" "class" "command" "ffi-method" "function" "internal-page-command" "internal-page-command-global" "mode" "parenscript" "user-class"))
     (org-edit-src-content-indentation 0)
     (cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (cider-preferred-build-tool . shadow-cljs)
     (cider-clojure-cli-global-options . "-A:fig")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))
 '(warning-suppress-types
   '((org-element-cache)
     (org-element-cache)
     (use-package)
     (modus-themes)
     (org-roam)
     (lsp-mode)
     (comp)
     (comp)))
 '(weechat-color-list
   '(unspecified "#011627" "#010F1D" "#DC2E29" "#EF5350" "#D76443" "#F78C6C" "#D8C15E" "#FFEB95" "#5B8FFF" "#82AAFF" "#AB69D7" "#C792EA" "#AFEFE2" "#7FDBCA" "#D6DEEB" "#FFFFFF"))
 '(zoom-window-mode-line-color "DarkBlue"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-project-dir ((t (:inherit bold))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "controlAccentColor"))))
 '(selectrum-current-candidate ((t (:background "#3a3f5a")))))
