(in-package #:xyz.lonjil.discord/base)

#+(or)(defmethod update-instance-for-redefined-class
    ((o bar) added deleted plist &key)
  (setf (slot-value o 'abc) (getf plist 'xyz)))

(defclass discord-base ()
  ((%websocket-client :accessor websocket-client)
   (%token :accessor token :initarg :token)
   (%session :accessor session :initarg :session)
   (%seq :accessor seq :initarg seq)
   (%heartbeat-interval :accessor heartbeat-interval
                        :initarg :heartbeat-interval)
   (%next-heartbeat-time :accessor next-heartbeat-time :initform 0)
   (%heartbeat-thread :accessor heartbeat-thread)
   (%event-queue :accessor event-queue :initarg :event-queue)
   (%callback :accessor callback :initarg :callback)
   (%alive-p :accessor alive-p :initarg :alive-p)
   (%reconnect-p :accessor reconnect-p :initarg :reconnect-p))
  (:default-initargs
   :heartbeat-interval 45
   :event-queue (q:make-queue :simple-cqueue)
   :alive-p nil
   :reconnect-p t)
  (:documentation
   #.(format nil "Represents a connection to the Discord ~
WebSocket gateway and an abstration over the Discord REST API.")))


(defgeneric connect (discord))
(defmethod connect ((discord discord-base))
  (when (alive-p discord)
    (return-from connect))
  (let ((client (wsd:make-client
                 "wss://gateway.discord.gg/?v=6&encoding=json")))
    (setf (websocket-client discord) client)
    (wsd:on :open client
            (lambda ()
              (l:logm :important "Connected")
              (setf (alive-p discord) t)
              (setf (seq discord) 0)))
    (wsd:on :close client (make-exit-callback discord))
    (wsd:on :message client (make-ws-callback discord))
    (wsd:on :error client (lambda (error)
                            (warn (format nil "wsd: ~s" error))))
    (wsd:start-connection client)))

(defclass discord (discord-base)
  ((%limits :accessor limits :initarg :limits
            :initform (make-hash-table :test 'equal))
   (%cache :accessor cache :initarg :cache)))

(defvar *discord*)
(defun new-discord (token callback)
  (let ((d (make-instance 'discord :token token :callback callback)))
    (setf *discord* d)))

(defun do-hb (discord)
  (wsd:send (websocket-client discord)
            (format nil "{\"op\":1,\"d\":~s}" (seq discord)))
  (setf (next-heartbeat-time discord)
        (+ (heartbeat-interval discord) (u:get-time))))
(defun wait-to-hb (discord)
  (let ((now (u:get-time))
        (hbt (next-heartbeat-time discord)))
    (if (>= now hbt)
        (do-hb discord)
        (sleep (- hbt now)))))
(defun make-hb-handler (discord)
  (lambda ()
    (loop (wait-to-hb discord))))


(defgeneric dispatch (discord event data))
(defmethod dispatch ((discord discord-base) event data)
  (bt:make-thread
   (lambda ()
     (let ((*discord* discord))
       (funcall (callback discord) event data)))
   :name "dispatch"))

(defun default-identify (discord)
  (make-instance
   'payload
   :op +identify+
   :data (make-instance
          'identify
          :token (token discord)
          :properties (make-instance
                       'properties :os "linux" :browser "cl-harmony"
                       :device "cl-harmony")
          :presence (make-instance
                     'update-status
                     :status "online" :afk? nil
                     :game (make-instance 'activity :name "&help" :type 0)))))
(defun make-ws-callback (discord)
  (lambda (message)
    (process-ws-message
     discord
     (json-mop:json-to-clos message 'payload))))
(defgeneric process-ws-message (discord message))
(defmethod process-ws-message ((discord discord-base) message)
  (with-accessors ((op op) (data data) (seq seq) (event event))
      message
    (cond
      ((eql +hello+ op)
       (let ((data (json-mop:json-to-clos data 'hello)))
         (l:logm :important "HELLO: ~s" (encode message))
         (setf (heartbeat-interval discord)
               (/ (heartbeat-interval data) 1000))
         (wsd:send (websocket-client discord)
                   (encode (default-identify discord)))
         (setf (heartbeat-thread discord)
               (bt:make-thread (make-hb-handler discord) :name "heartbeater"))))
      ((eql +invalid-session+ op) (l:logm :important "Invalid session"))
      ((eql +heartbeat+ op) (do-hb discord))
      ((eql +beat-ack+ op))
      ((eql +dispatch+ op)
       (setf (seq discord) seq)
       (dispatch discord event data))
      (t (l:logm :important "Unhandled op: ~s" (encode message))))))

(defun make-exit-callback (discord)
  (lambda (&key reason code)
    (exit-callback discord reason code)))
(defun exit-callback (discord reason code)
  (when (eql code 4004)
    (setf (reconnect-p discord) nil)
    (format t "Invalid token"))
  (when (alive-p discord)
    (setf (alive-p discord) nil)
    (format t "Closed cuz '~a' (code=~a) (thread: ~a)~%"
            reason code (bt:current-thread))
    (bt:destroy-thread (heartbeat-thread discord))
    (bt:make-thread
     (lambda ()
       (let ((read-thread (slot-value (websocket-client discord)
                                      'websocket-driver.ws.client::read-thread)))
         (if read-thread
             (progn (bt:destroy-thread read-thread)
                    (l:logm :important "Killed read thread"))
             (l:logm :important "*Didn't* kill the read thread")))
       (map nil (lambda (x) (bt:destroy-thread x))
            (remove-if-not (lambda (x) (string= (bt:thread-name x)
                                        "websocket client read thread"))
                           (bt:all-threads)))
       (when (reconnect-p discord)
         (sleep 5)
         (connect discord)))
     :name "cleanup")))
(defun ws-handler (message discord)
    )
