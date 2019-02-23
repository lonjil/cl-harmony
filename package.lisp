;;;; package.lisp

(defpackage #:xyz.lonjil.discord/logger
  (:use #:cl)
  (:export #:logm))
(defpackage #:xyz.lonjil.discord/utilities
  (:use #:cl)
  (:export #:get-time))
(defpackage #:xyz.lonjil.discord/base
  (:use #:cl)
  (:shadow #:type)
  (:local-nicknames (#:s #:serapeum)
                    (#:pn #:pathname-utils)
                    (#:l #:xyz.lonjil.discord/logger)
                    (#:u #:xyz.lonjil.discord/utilities))
  (:export #:send-message #:id #:channel-id #:guild-id #:author
           #:member #:content #:user #:nick #:callback #:req
           #:invite #:username #:discrim #:code #:guild #:channel
           #:inviter #:uses #:message #:bot? #:add-member-role
           #:guild-member #:*discord* #:get-invites #:new-discord
           #:connect #:reconnect-p))
