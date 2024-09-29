(defconst list-resources '("connections" "channels" "consumers" "exchanges" "queues" "bindings" "users" "vhosts" "permissions" "nodes" "parameters" "policies" "operator_policies" "vhost_limits" "overview"))

(defun rabbitadmin/list (resource &optional user password)
  (interactive
   (list (completing-read "Resource: " list-resources)))
  (cl-assert (member resource list-resources)
             t (format "Invalid argument: %s" resource))
  (let ((list-output (->>
                      (with-output-to-string
                        (call-process "rabbitmqadmin" nil standard-output
                                      "*list-rabbit-queues*"
                                      (format "--password=%s" (or user "grain"))
                                      (format "--user=%s" (or password "grain"))
                                      "list"
                                      "queues"
                                      "--format=bash"))
                      (s-split " ")
                      )))
    (with-current-buffer (get-buffer-create "*rabbitmqadmin-output*")
      (erase-buffer)
      (setq tabulated-list-format [("Result" 20)])
      (setq tabulated-list-entries
            (cl-loop for item in list-output
                     collect (list item (vector item))))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))))

(provide 'rabbitmq-admin)
