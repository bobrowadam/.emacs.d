(use-package dap-mode
  :init
  (setq services-dir "/Users/bob/source/services")
  :config
  ;; The modes above are optional
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode nil)
  (setq dap-ui-buffer-configurations
  `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.40)))
    (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
    (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
    (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
    (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))))

  (dap-register-debug-template
   "TS:Run"
   (list :type "node"
         :cwd nil
         :request "launch"
         ;; :program "node"
         :smartStep t
         :runtimeArgs ["-r" "source-map-support/register"]
         ;; :env '(("SETUP_DEV_ENV_ON_STARTUP" . "1"))
         ;; :protocol "inspector"
         :sourceMap t
         :name "TS::Run"))
  (dap-register-debug-template
   "Scraper"
   (list :type "node"
         :request "attach"
         :args ["run" "debug"]
         :cwd (format "%s/scraper" services-dir)
         :runtimeExecutable "npm"
         :protocol "inspector"
         :smartStep t
         :port 9229
         :skipFiles ["<node_internals>/**/*.js"]
         ))
  (require 'dap-node))

(provide 'dap-setup)
