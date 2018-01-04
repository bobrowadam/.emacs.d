;;;; package --- Summary
;;; Commentary:
;; setup the pipeline with prodigy
;;
;;; code:

(defvar node-8-verssion "8.9.1")

(prodigy-define-service
  :name "web-api"
  :command "nodemon"
  :cwd "~/source/local_environment/web-api"
  :args '("web-api.js")
  :port 3000
  :init-async (lambda (done)
                (nvm-use node-8-verssion done))
  :tags '(pipeline javascript mandatory))

(prodigy-define-service
  :name "data-api"
  :command "nodemon"
  :cwd "~/source/local_environment/data-api"
  :args '("data-api.js")
  :port 1337
  :init-async (lambda (done)
                (nvm-use node-8-verssion done))
  :tags '(pipeline javascript mandatory))

(prodigy-define-service
  :name "enrichment"
  :command "nodemon"
  :cwd "~/source/local_environment/enrichment"
  :args '("enrichment.js")
  :port 3070
  :init-async (lambda (done)
                (nvm-use node-8-verssion done))
  :tags '(pipeline javascript mandatory))

(prodigy-define-service
  :name "public-api"
  :command "nodemon"
  :cwd "~/source/local_environment/public-api"
  :args '("public-api.js")
  :port 3099
  :tags '(pipeline node))

(prodigy-define-service
  :name "frontier"
  :command "npm"
  :cwd "~/source/local_environment/frontier"
  :args '("run" "dev")
  :port 8000
  :tags '(pipeline js mandatory))

(prodigy-define-service
  :name "correlation"
  :command "sbt"
  :cwd "~/source/local_environment/correlation"
  :args '("run")
  :stop-signal 'sigkill
  :port 2700
  :tags '(pipeline scala mandatory))

(prodigy-define-service
  :name "envy"
  :command "sbt"
  :cwd "~/source/local_environment/envy"
  :args '("run")
  :stop-signal 'sigkill
  :port 5555
  :tags '(pipeline scala mandatory))

(prodigy-define-service
  :name "snooze-api"
  :command "nodemon"
  :cwd "~/source/local_environment/snooze-api"
  :args '("snooze-api.js")
  :port 3053
  :tags '(pipeline node))

(prodigy-define-service
  :name "consumer"
  :command "nodemon"
  :cwd "~/source/local_environment/consumer"
  :args '("consumer.js")
  :port 1339
  :init-async (lambda (done)
                (nvm-use node-8-verssion done))
  :tags '(pipeline node))

(prodigy-define-service
  :name "outbound-api"
  :command "nodemon"
  :cwd "~/source/local_environment/outbound-api"
  :args '("outbound-api.js")
  :port 3042
  :tags '(pipeline node))

(prodigy-define-service
  :name "beagle"
  :command "sbt"
  :cwd "~/source/local_environment/beagle"
  :args '("run")
  :stop-signal 'sigint
  :port 3042
  :tags '(pipeline scala))

(prodigy-define-service
  :name "shaggy"
  :command "npm"
  :cwd "~/source/local_environment/shaggy"
  :args '("run" "dev")
  :stop-signal 'sigkill
  :port 8080
  :init-async (lambda (done)
                (nvm-use node-8-verssion done))
  :tags '(js mandatory))

(prodigy-define-service
  :name "metadata-api"
  :command "nodemon"
  :cwd "~/source/local_environment/metadata-api"
  :args '("metadata-api.js")
  :stop-signal 'sigint
  :port 1338
  :tags '(node pipeline ))

(prodigy-define-service
  :name "incident-log"
  :command "nodemon"
  :cwd "~/source/incident-log"
  :args '("incident-log.js")
  :stop-signal 'sigint
  :tags '(node pipeline ))

(prodigy-define-service
  :name "notif"
  :command "nodemon"
  :cwd "~/source/local_environment/notif"
  :args '("notif.js")
  :stop-signal 'sigint
  :tags '(node pipeline ))

(prodigy-define-service
  :name "atlas"
  :command "sbt"
  :cwd "~/source/local_environment/atlas"
  :args '("run")
  :stop-signal 'sigint
  :port
  :tags '(node pipeline ))

(provide 'setup-pipeline)
;;; setup-pipeline.el ends here
