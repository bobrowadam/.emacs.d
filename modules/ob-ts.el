;;; ob-ts.el --- Org-Babel support for ts via ob-bun -*- lexical-binding: t; -*-

;; This shim allows `(ts . t)` in `org-babel-load-languages` to load
;; your custom TypeScript executor from `ob-bun.el`.

(require 'ob-bun)

(provide 'ob-ts)
;;; ob-ts.el ends here
