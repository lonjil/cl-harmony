;;;; package.lisp

(defpackage #:cl-harmony
  (:use #:cl #:alexandria
        #:drakma #:st-json
        #:sqlite
        #:sxql #:bordeaux-threads
        #:livesupport #:split-sequence
        #:metabang.bind #:queues)
  (:shadow eval connect disconnect)
  (:export *discord*
           make-discord
           send-message
           send-embed
           get-messages
           get-channel
           edit-message
           edit-role
           set-avatar
           channel-name
           server-of-channel
           server-name
           queue-message-async
           queue-message
           queue-edit-message-async
           blarp
           *client*
           *running*
           cb
           queue-handler
           connect
           ))

