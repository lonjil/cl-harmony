(in-package #:cl-harmony)


(defclass discord ()
  ((con :accessor con)
   (token :accessor token :initarg :token)
   (session :accessor session :initarg :session)
   (seq :accessor seq :initarg :seq)
   (hb-interval :accessor hb-interval :initarg :hb-interval :initform 41)
   (next-hb-time :accessor next-hb-time)
   (hb-lock :accessor hb-lock :initarg :hb-lock :initform (make-lock))
   (_trace :accessor _trace)
   (db :accessor db :initarg :db)
   (callback :accessor cb :initarg :cb)
   (hb-thread :accessor hb-thread :initarg :hb-thread)
   (ws-client :accessor ws-client :initarg :ws-client)
   (channel-queue-map :accessor channel-queue-map :initarg :channel-queue-map)
   (%running :accessor running :initform nil :type (member nil t))
   (%reconnect :accessor reconnect? :initform t :type (member nil t))))

(defun make-discord (token &optional callback)
  "Makes a discord"
  (make-instance
   'discord
   :token token
   :db (sqlite:connect ":memory:")
   :cb callback
   :seq 0
   :channel-queue-map (make-hash-table :test 'equal)))
