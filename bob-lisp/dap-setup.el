(use-package dap-mode
  :custom
  (dap-auto-configure-features '())
  :config
  ;; (setq dap-ui-buffer-configurations
  ;; `((,dap-ui--locals-BUFFER . ((side . right) (slot . 1) (window-width . 0.40)))
  ;;   (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
  ;;   (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
  ;;   (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
  ;;   (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
  ;;   (,dap-ui--repl-buffer . ((side . bottom) (slot . 3) (window-width . 0.70)))))

  (dap-register-debug-template
   "TS:Run"
   (list :type "node"
         :request "launch"
         :smartStep t
         :runtimeArgs ["-r" "source-map-support/register"]
         ;; :env '(("SETUP_DEV_ENV_ON_STARTUP" . "1"))
         ;; :protocol "inspector"
         ;; :program "${workspaceFolder}/lemmings.js"
         :skipFiles ["<node_internals>/**"]
         :outFiles ["${workspaceFolder}/dist/**/*.js"]
         ;; :sourceMap t
         :name "TS::Run"))

  (require 'dap-node))

(provide 'dap-setup)
