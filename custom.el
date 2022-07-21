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
 '(global-company-mode t)
 '(highlight-changes-colors '("#EF5350" "#7E57C2"))
 '(ignored-local-variable-values '((Base . 10) (Package . CL-USER) (Syntax . COMMON-LISP)))
 '(lsp-restart 'ignore)
 '(lsp-vetur-dev-log-level "DEBUG" t)
 '(magit-diff-use-overlays nil)
 '(magit-todos-group-by '(magit-todos-item-filename magit-todos-item-keyword))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(org-superstar darkroom org-roam-bibtex writeroom-mode org-roam-ui sbt-mode lsp-metals flyspell node-repl racket-mode slime-company slime projectile org-notify restclient consult-lsp hotfuzz affe savehist-mode selectrum vertico embark-consult embark marginalia consult org-roam forge elfeed jest-test jest-test-mode haskell-snippets typed-clojure-mode cider lsp-haskell haskell-mode dired-sidebar cargo-mode rust-mode magit-todos-mode magit-todos terraform-mode immaterial-theme docker-file dockerfile-mode string-inflection elm-mode js-doc orderless emacs esup shell-command+ lsp-eslint csv-mode dap-mode doom-modeline all-the-icons lsp-ui lsp-mode origami origami-mode night-owl-theme ibuffer-vc popup-kill-ring company-box flycheck-posframe flycheck-postframe acme-theme abyss-theme scratch-pop undo-fu vlf literate-calc-mode docker yaml-mode emojify web-mode add-node-modules-path vterm try control-mode zoom-window flycheck-rust cargo multiple-cursors anzu expand-region yasnippet-snippets wgrep deadgrep ripgrep ob-restclient org-bullets ox-gfm ox-pandoc jest ts-comint typescript-mode jq-format json-mode nodejs-repl js2-mode company-lsp lsp-origami highlight-indent-guides sicp company whole-line-or-region diff-hl git-timemachine github-review magit smex ibuffer-projectile inf-mongo smartparens paredit golden-ratio tron-legacy-theme doom-themes flycheck exec-path-from-shell which-key dired-du dired-rsync diredfl dired-subtree dash use-package))
 '(pos-tip-background-color "#FFF9DC")
 '(pos-tip-foreground-color "#011627")
 '(proced-format 'verbose)
 '(safe-local-variable-values
   '((whitespace-style quote
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
