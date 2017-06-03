;;;; package --- Summary
;;; Commentary:
;; setup the pipeline with prodigy
;;
;;; code:

(prodigy-define-service
  :name "web-api"
  :command "nodemon"
  :cwd "~/source/local_environment/web-api"
  :args '("web-api.js")
  :port 3000
  :tags '(pipeline node mandatory))

(prodigy-define-service
  :name "data-api"
  :command "nodemon"
  :cwd "~/source/local_environment/data-api"
  :args '("data-api.js")
  :port 1337
  :tags '(pipeline node mandatory))

(prodigy-define-service
  :name "enrichment"
  :command "nodemon"
  :cwd "~/source/local_environment/enrichment"
  :args '("enrichment.js")
  :port 3070
  :tags '(pipeline node mandatory))

(prodigy-define-service
  :name "public-api"
  :command "nodemon"
  :cwd "~/source/local_environment/public-api"
  :args '("public-api.js")
  :port 3099
  :tags '(pipeline node))

(prodigy-define-service
  :name "frontier"
  :command "grunt"
  :cwd "~/source/local_environment/frontier"
  ;; :args '("grunt")
  :port 8000
  :tags '(pipeline node))

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
  :cwd "~/source/local_environment/snooze"
  :args '("snooze-api.js")
  :port 3053
  :tags '(pipeline node))

(prodigy-define-service
  :name "consumer"
  :command "nodemon"
  :cwd "~/source/local_environment/consumer"
  :args '("consumer.js")
  :port 1339
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
  :cwd "~/source/local_environment/"
  :args '("run")
  :stop-signal 'sigint
  :port 3042
  :tags '(pipeline scala))

(provide 'setup-pipeline)
;;; setup-pipeline.el ends here
