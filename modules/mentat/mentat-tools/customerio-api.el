;;; customerio-api.el --- Customer.io inspection tools -*- lexical-binding: t; -*-

(require 'mentat-elisp-library)
(require 'bob-customerio)

(mentat-defun mentat-customerio-read (environment operation &optional template-id)
  "Read Customer.io workspaces, templates, template metadata or contents.
ENVIRONMENT must be dev or prod.  OPERATION must be workspaces, templates,
template or contents.  TEMPLATE-ID is required for template and contents.
This tool performs no writes and sends no messages.  AWS SSM supplies keys."
  (:execution async)
  (lambda (resolve reject on-cancel)
    (funcall on-cancel
             (bob/customerio-read environment operation template-id resolve reject))))

(provide 'customerio-api)
;;; customerio-api.el ends here
