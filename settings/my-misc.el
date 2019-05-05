;; Seed the random-number generator
(random t)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Add Urban Dictionary to webjump (C-x g)
(use-package webjump
  :ensure t
  :bind
  ("C-c w j" . webjump)
  :config
  (add-to-list
   'webjump-sites
               '("Urban Dictionary" .
                 [simple-query
                  "www.urbandictionary.com"
                  "http://www.urbandictionary.com/define.php?term="
                  ""])))



;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 = >") (λ (insert "⇒")))
(global-set-key (kbd "C-x 8 8") (λ (insert "∞")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 v") (λ (insert "✓")))

;; create a quip scratch buffer:
(global-set-key (kbd "C-c C-b") 'create-scratch-buffer)
(provide 'my-misc)