;;; Package --- summary:
;;; My quick pipeline setup
;;; Commentary:
;;; Code:

(defun run-pipemacs()
    (interactive)
  "Runs the 'bigpanda' minimal pipeline"
  (save-excursion
    (async-shell-command "cd ~/source/local_environment/web-api; nodemon web-api.js" (eshell 1))
    (async-shell-command "cd ~/source/local_environment/frontier; grunt" (eshell 2))
    (async-shell-command "cd ~/source/local_environment/data-api; nodemon data-api.js" (eshell 3))
    (async-shell-command "cd ~/source/local_environment/consumer; nodemon consumer.js" (eshell 4))
    (async-shell-command "cd ~/source/local_environment/envy; sbt run" (eshell 5))
    (async-shell-command "cd ~/source/local_environment/correlation; sbt run" (eshell 6)))
  (delete-other-windows))

(defun go-to-pipemacs()
    (interactive)
  "Open all pipeline buffers."
  (make-frame )
  (switch-to-buffer "web-api-stdout")
  (split-window-horizontally)
  (switch-to-buffer "frontier-stdout")
  (split-window-horizontally)
  (switch-to-buffer "data-api-stdout")
  (balance-windows)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "consumer-stdout")
  (other-window 1)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "envy-stdout")
  (other-window 1)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "correlation-stdout"))

(defun kill-pipe-line()
  (interactive)
  "Kill active pipeline buffers."
  (if (buffer-live-p (get-buffer "frontier-stdout"))
      (kill-buffer "frontier-stdout"))
  ;; (kill-buffer "web-api-stderr")
  (kill-buffer "frontier-stdout")
  ;; (kill-buffer "frontier-stderr")
  (kill-buffer "correlation-stdout")
  ;; (kill-buffer "correlation-stderr")
  (kill-buffer "consumer-stdout")
  ;; (kill-buffer "consumer-stderr")
  (kill-buffer "envy-stdout")
  ;; (kill-buffer "envy-stderr")
  (kill-buffer "data-api-stdout"))
  ;; (kill-buffer "data-api-stderr"))

;; TODO: write something that will kill the active shell buffers gracefully (no query for confiramtion)
;; (signal-process (get-buffer-process "envy-stdout") 9)

;; (signal-process (get-buffer-process "") 9)
;; (if (signal-process (get-buffer-process "eshell-1")
;;                     (kill-buffer "*eshell*")))

(provide 'pipemacs)
;;; pipemacs.el ends here
