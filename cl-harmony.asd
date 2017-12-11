;;;; cl-harmony.asd

(asdf:defsystem #:cl-harmony
  :description "Describe cl-harmony here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:sxql
               #:sqlite
               #:drakma
               #:flexi-streams
               #:cl-json
               #:livesupport
               #:websocket-driver
               #:bordeaux-threads
               #:split-sequence
               #:cl-interpol
               #:metabang-bind
               #:queues.simple-cqueue)
  :serial t
  :components ((:file "package")
               (:file "cl-harmony")))
