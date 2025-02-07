(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-format-latex-options
   '(:foreground default :background default :scale 3.0 :html-foreground "Black" :html-background "Transparent" :html-scale 3.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-vc-selected-packages
   '((pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs")
     (pg :vc-backend Git :url "https://github.com/emarsden/pg-el")
     (org-mode :url "https://code.tecosaur.net/tec/org-mode")
     (aider :url "https://github.com/tninja/aider.el")))
 '(safe-local-variable-values
   '((vc-default-patch-addressee . "bug-gnu-emacs@gnu.org")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
