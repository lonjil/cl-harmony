;;;; cl-harmony.asd

(asdf:defsystem #:cl-harmony
  :description "Discord API interaction library."
  :author "Birk Hirdman <lonjil@gmail.com>"
  :license "Not yet sure"
  :depends-on (#:alexandria
               #:sxql
               #:sqlite
               #:drakma
               #:flexi-streams
               #:websocket-driver
               #:bordeaux-threads
               #:split-sequence
               #:cl-interpol
               #:metabang-bind
               #:queues.simple-cqueue
               #:serapeum
               #:pathname-utils
               #:json-mop
               #:local-time)
  :serial t
  :components ((:file "package")
               (:file "utilities/utils")
               (:file "logger/logger")
               (:file "base/constants")
               (:file "base/structures")
               (:file "base/discord-base")
               (:file "base/api")))
