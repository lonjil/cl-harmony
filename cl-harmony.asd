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
               #:st-json
               #:livesupport
               #:websocket-driver
               #:bordeaux-threads
               #:split-sequence
               #:cl-interpol
               #:metabang-bind
               #:queues.simple-cqueue
               #:cffi)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "discord")
               (:file "channel-queue")
               (:file "cl-harmony")))
