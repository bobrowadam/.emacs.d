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
  (require 'dap-node)
  (dap-register-debug-template
   "Vera"
   (list :type "node"
         :request "launch"
         :args ["dist/vera.js" "--local"]
         :cwd (format "%s/vera" services-dir)
         :runtimeArgs ["-r" "source-map-support/register" ]
         :protocol "inspector"
         :sourceMap t
         :name "Vera"
         :smartStep t
         ))

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
  (dap-register-debug-template
   "Merger"
   (list :type "node"
         :request "attach"
         :args ["run" "debug"]
         :cwd (format "%s/merger" services-dir)
         :runtimeExecutable "npm"
         :protocol "inspector"
         :smartStep t
         :port 9229
         :skipFiles: [ "<node_internals>/**/*.js"]
         ))
  (dap-register-debug-template
   "Lemmings"
   (list :type "node"
         :request "launch"
         ;; :request "attach"
         :args ["run" "debug"]
         :cwd (format "%s/lemmings" services-dir)
         :runtimeExecutable "npm"
         :protocol "inspector"
         :smartStep t
         :port 9229
         :skipFiles: [ "<node_internals>/**/*.js"]
         ))
  
  (dap-register-debug-template
   "Orca"
   (list :type "node"
         :request "launch"
         ;; :request "attach"
         :args ["run" "debug"]
         :cwd (format "%s/orca" services-dir)
         :runtimeExecutable "npm"
         ;; :protocol "inspector"
         :smartStep t
         :port 9229
         :skipFiles: ["<node_internals>/**/*.js"]
         ))

  (dap-register-debug-template
   "Whatsapi"
   (list :type "node"
         :request "launch"
         ;; :request "attach"
         :args ["run" "debug"]
         :cwd (format "%s/whatsapi" services-dir)
         :runtimeExecutable "npm"
         ;; :protocol "inspector"
         :smartStep t
         :port 9229
         :skipFiles: ["<node_internals>/**/*.js"]))
  
  (dap-register-debug-template
   "Whatsapp-Aggregator"
   (list :type "node"
         ;; :request "launch"
         :request "attach"
         :args ["run" "debug"]
         :cwd (format "%s/whatsapp-aggregator" services-dir)
         :runtimeExecutable "npm"
         ;; :protocol "inspector"
         :smartStep t
         :port 9229
         :skipFiles: ["<node_internals>/**/*.js"]))

  (dap-register-debug-template
   "catapult"
   `( :type "node"
            :request "launch"
            ;; :request "attach"
            :args ["run" "debug"]
            :cwd ,(format "%s/catapult" services-dir)
            :runtimeExecutable "npm"
            ;; :protocol "inspector"
            :smartStep t
            :port 9229
            :skipFiles [
                        ,(format "%s/catapult/%s" services-dir "node_modules/lodash/lodash.js")
                        "<node_internals>/**/*.js"
                        ]
            ))
  (dap-register-debug-template
   "boti"
   `( :type "node"
            :request "launch"
            ;; :request "attach"
            :args ["run" "debug"]
            :cwd ,(format "%s/boti" services-dir)
            :runtimeExecutable "npm"
            ;; :protocol "inspector"
            :smartStep t
            :port 9229
            :skipFiles [
                        ,(format "%s/catapult/%s" services-dir "node_modules/lodash/lodash.js")
                        "<node_internals>/**/*.js"
                        ]
            ))
  (dap-register-debug-template
   "igor"
   `( :type "node"
            :request "launch"
            ;; :request "attach"
            :args ["run" "debug"]
            :cwd ,(format "%s/igor" services-dir)
            :runtimeExecutable "npm"
            ;; :protocol "inspector"
            :smartStep t
            :port 9229
            :skipFiles [
                        ,(format "%s/catapult/%s" services-dir "node_modules/lodash/lodash.js")
                        "<node_internals>/**/*.js"
                        ]
            ))
  (dap-register-debug-template
   "johnnycash"
   `( :type "node"
            ;; :request "launch"
            :request "attach"
            ;; :args ["run" "start"]
            :args ["run" "start"]
            :cwd ,(format "%s/johnnycash" services-dir)
            :runtimeExecutable "npm"
            ;; :runtimeArgs ["-r" "source-map-support/register" "--inspect" ]
            ;; :protocol "inspector"
            :smartStep t
            :port 9229
            :skipFiles [
                        ,(format "%s/catapult/%s" services-dir "node_modules/lodash/lodash.js")
                        "<node_internals>/**/*.js"
                        ]
            )))

(provide 'dap-setup)
