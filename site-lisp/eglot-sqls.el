(add-to-list 'display-buffer-alist
               '("\\*sqls\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))

(defclass eglot-sqls (eglot-lsp-server) ()
    :documentation "SQL's Language Server")

(cl-defmethod eglot-execute ((server eglot-sqls) (_cmd (eql switchDatabase)) arguments)
  "For switchDatabase."
  (let* ((res (jsonrpc-request server :workspace/executeCommand
                               `(:command "showDatabases" :arguments ,arguments :timeout 0.5)))
         (menu-items (split-string res "\n"))
         (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
         (db (if (listp last-nonmenu-event)
                 (x-popup-menu last-nonmenu-event menu)
               (completing-read "[eglot] Pick an database: "
                                menu-items nil t
                                nil nil (car menu-items)))))
    (jsonrpc-request server :workspace/executeCommand
                     `(:command "switchDatabase" :arguments [,db] :timeout 0.5))))

(cl-defmethod eglot-execute ((server eglot-sqls) command)
  (let* ((uri (eglot-path-to-uri (buffer-file-name)))
         (beg (eglot--pos-to-lsp-position (if (use-region-p)
                                              (region-beginning)
                                            (point-min))))
         (end (eglot--pos-to-lsp-position (if (use-region-p)
                                              (region-end)
                                            (point-max))))
         (res (condition-case err
                  (eglot--request server :workspace/executeCommand
                                  `(:command ,(format "%s" (plist-get command :command))
                                             :range (:start ,beg :end ,end)
                                             :arguments [,uri]
                                             :timeout 0.5))
                (error (message "Failed to execute query: %s"
                                (error-message-string err))
                       nil))))
    (when res
      (let ((buffer (get-buffer-create "*sqls*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert (format "Result: %s" res))
          (org-mode)
          (setq org-pretty-entities t)
          (goto-char (point-min))
          (delete-region (point) (search-forward "Result: "))
          (goto-char (point-min))
          (replace-regexp "\\+" "|" nil (point) (point-max))
          (goto-char (point-min))
          (while (re-search-forward "\\([0-9]\\)\\(|\\)\\([0-9]\\)" nil t)
            (replace-match "\\1\\\\vert\\3" t))
)
        (pop-to-buffer buffer)))))

;; (cl-defmethod eglot-execute ((server eglot-sqls) (command (eql switchDatabase)))
;;   (let* ((uri (eglot-path-to-uri (buffer-file-name)))
;;          (beg (eglot--pos-to-lsp-position (if (use-region-p)
;;                                              (region-beginning)
;;                                            (point-min))))
;;          (end (eglot--pos-to-lsp-position (if (use-region-p)
;;                                              (region-end)
;;                                            (point-max))))
;;          (res (condition-case err
;;                 (eglot--request server :workspace/executeCommand
;;                                `(:command ,(format "%s" (plist-get command :command))
;;                                           :range (:start ,beg :end ,end)
;;                                           :arguments [,uri]
;;                                           :timeout 0.5))
;;               (error (message "Failed to execute query: %s"
;;                              (error-message-string err))
;;                      nil))))
;;     (when res
;;       (let ((buffer (get-buffer-create "*sqls*")))
;;         (with-current-buffer buffer
;;           (insert (format "Result: %s" res))
;;           (org-mode))
;;         (pop-to-buffer buffer)))))

(provide 'eglot-sqls)
