(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face 'default)
 '(datetime-timezone 'Israel)
 '(eglot-autoshutdown t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
        (starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(ignored-local-variable-values
   '((mmm-classes . elisp-js)
     (eval mmm-add-group 'elisp-js
           '((elisp-rawjs :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "xwwp--js \"" :back "\" js--")
             (elisp-defjs :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "xwwp-js-def .*\12.*\"\"\12" :back "\")\12")))
     (Base . 10)
     (Package . CL-USER)
     (Syntax . COMMON-LISP)))
 '(magit-diff-use-overlays nil)
 '(magit-todos-group-by '(magit-todos-item-filename magit-todos-item-keyword))
 '(package-vc-selected-packages
   '((miasma-theme :vc-backend Git :url "https://github.com/daut/miasma-theme.el")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")))
 '(pos-tip-background-color "#FFF9DC")
 '(pos-tip-foreground-color "#011627")
 '(proced-format 'verbose)
 '(realgud-safe-mode nil)
 '(safe-local-variable-values
   '((vc-default-patch-addressee . "bug-gnu-emacs@gnu.org")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (eval mmm-add-group 'elisp-js
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
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))
 '(weechat-color-list
   '(unspecified "#011627" "#010F1D" "#DC2E29" "#EF5350" "#D76443" "#F78C6C" "#D8C15E" "#FFEB95" "#5B8FFF" "#82AAFF" "#AB69D7" "#C792EA" "#AFEFE2" "#7FDBCA" "#D6DEEB" "#FFFFFF"))
 '(wucuo-font-faces-to-check
   '(font-lock-string-face font-lock-doc-face font-lock-comment-face font-lock-function-name-face font-lock-variable-name-face tree-sitter-hl-face:type tree-sitter-hl-face:string tree-sitter-hl-face:string.special tree-sitter-hl-face:doc tree-sitter-hl-face:comment tree-sitter-hl-face:variable tree-sitter-hl-face:varialbe.parameter tree-sitter-hl-face:function tree-sitter-hl-face:function.call tree-sitter-hl-face:method js2-function-param js2-object-property js2-object-property-access css-selector css-property rjsx-text rjsx-tag rjsx-attr))
 '(zoom-window-mode-line-color "DarkBlue"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-sidebar-face ((t (:height 1.5 :width extra-expanded :family "Menlo"))) t)
 '(eglot-highlight-symbol-face ((t (:inherit bold :underline t))))
 '(forge-topic-slug-open ((t (:inherit modus-themes-heading-3))))
 '(fringe ((t :background "#0d0e1c")))
 '(header-line ((t :box (:line-width 4 :color "#1d2235" :style nil))))
 '(header-line-highlight (nil))
 '(keycast-key (nil))
 '(line-number (nil))
 '(mode-line ((t :box (:line-width 6 :color "#484d67" :style nil))))
 '(mode-line-active ((t (:inherit mode-line))))
 '(mode-line-highlight (nil))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#292d48" :style nil))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "controlAccentColor"))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#0d0e1c" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#4a4f6a" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-current ((t)))
 '(tab-line-tab-inactive (nil))
 '(vertical-border (nil))
 '(window-divider ((t :background "#0d0e1c" :foreground "#0d0e1c")))
 '(window-divider-first-pixel ((t :background "#0d0e1c" :foreground "#0d0e1c")))
 '(window-divider-last-pixel ((t :background "#0d0e1c" :foreground "#0d0e1c"))))
