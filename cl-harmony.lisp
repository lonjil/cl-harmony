;;;; cl-harmony.lisp

(in-package #:cl-harmony)

;;; "cl-harmony" goes here. Hacks and glory await!


(defun connect (&optional (discord *discord*))
  (setf (ws-client discord)
        (wsd:make-client "wss://gateway.discord.gg/?v=6&encoding=json"))
  (wsd:on :open (ws-client discord)
          (lambda ()
            (format t "Connected.~%")
            (setf (hb-thread discord) (make-thread (make-hb-handler discord)
                                                   :name "heartbeat"))
            (when (slot-boundp (db discord)
                               'sqlite::handle)
              (sqlite:disconnect (db discord)))
            (setf (db discord) (sqlite:connect ":memory:"))
            (ready-db (db discord))))
  (wsd:on :close (ws-client discord)
          (named-lambda dc (&key reason code)
            (format t "Closed cuz '~a' (code=~a).~%" reason code)
            (destroy-thread (hb-thread discord))
            (sqlite:disconnect (db discord))
            (setf *running* nil)))
  (wsd:on :message (ws-client discord) (make-ws-handler discord))
  (wsd:on :error (ws-client discord)
          (lambda (error)
            (warn (format t "wsd: ~a" error))))
  (wsd:start-connection (ws-client discord)))

(defun test (foo)
  (let ((n (channel-name foo)))
    (loop (format t "~a> " n)
          (queue-message-async foo (read-line)))))

(defun foo (foo)
  (loop (format t "~a> " foo)
        ))


(defun ready-db (db)
  (let ((tables (list (create-table :servers
                          ((id :type 'integer
                               :primary-key t)
                           (name :type 'text)
                           (owner :type 'integer)))
                      (create-table :channels
                          ((id :type 'integer
                               :primary-key t)
                           (server :type 'integer)
                           (type :type 'integer)
                           (name :type 'text)
                           (topic :type 'text)
                           (last_message_id :type 'integer))))))
    (loop for table in tables
          do (execute-non-query db (yield table)))))

(defvar *key* (with-open-file (foo "./key.txt")
                (read-line foo)))
(defvar *discord*)

(defvar *api* "https://discordapp.com/api/v6")
                                        ;(defvar *)

(defun req (uri &key (type :get) content (*discord* *discord*))
  "Make a request to Discord"
  (when (and (member type '(:post :patch :put))
             (eq content nil))
      (error "Request type ~s requires a content parameter present" type))
  (bind (((:values body-raw
                   status
                   headers
                   uri)
          (http-request
           (format nil "~a~a" *api* uri)
           :method type
           :content-type "application/json"
           :user-agent "cl-harmony (https://github.com/lonjil/cl-harmony, 0.1.0)"
           :additional-headers
           `(("Authorization" . ,(format nil "Bot ~a" (token *discord*))))
           :content content
           :content-length (length content)))
         (body (if (stringp body-raw)
                   body-raw
                   (flexi-streams:octets-to-string body-raw))))
    (values body status headers uri)))

(defvar *chanrates* nil)
;(defun get-remaining)

(defun send-message (channel-id message)
  "Sends a message, whoa!"
  (req (format nil "/channels/~a/messages" channel-id)
       :type :post
       :content (json:encode-json-alist-to-string
                 `((:content . ,message)))))
(defun send-embed (channel-id &key message embed)
  "Sends a message, whoa!"
  (unless (or message embed) (error "Need at least 1 key"))
  (req (format nil "/channels/~a/messages" channel-id)
       :type :post
       :content (json:encode-json-to-string
                 (let ((x nil))
                   (when message (push `(:content . ,message) x))
                   (when embed (push `(:embed . ,embed) x))
                   x))))
(defun get-messages (channel-id)
  "Totally useful docstring, whoa!"
  (req (format nil "/channels/~a/messages" channel-id)))
(defun get-channel (channel-id)
  (req (format nil "/channels/~a" channel-id)))
(defun edit-message (channel-id message-id message)
  (req (format nil "/channels/~a/messages/~a" channel-id message-id)
       :type :patch
       :content (json:encode-json-alist-to-string `((:content . ,message)))))
(defun edit-role (guild-id role-id name perms color hoist mentionable)
  "PATCHes a role on a guild"
  (req (format nil "/guilds/~a/roles/~a" guild-id role-id)
       :type :patch
       :content (format nil "{\"name\": \"~a\",
\"permissions\": ~a,
\"color\": ~a,
\"hoist\": ~a,
\"mentionable\": ~a}" name perms color hoist mentionable)))

(defun set-avatar (image &key (format "png" type-p))
  "Takes a byte vector and a format specifier"
  (declare (ignorable type-p))
  (req "/users/@me" :type :patch
                    :content (format nil "{\"avatar\": \"~a\"}"
                                     (concatenate
                                      'string
                                      "data:image/"
                                      format
                                      ","
                                      (base64:usb8-array-to-base64-string
                                       image)))))



(defconstant +dispatch+ 0) ;receive
(defconstant +heartbeat+ 1) ;both
(defconstant +identify+ 2) ;send
(defconstant +status+ 3) ;send
(defconstant +voice-state+ 4) ;send
(defconstant +voice-ping+ 5) ;send
(defconstant +resume+ 6) ;send
(defconstant +reconnect+ 7) ;receive
(defconstant +request-guild-members+ 8) ;send
(defconstant +invalid-session+ 9) ;receive
(defconstant +hello+ 10) ;receive
(defconstant +beat-ack+ 11) ;receive

(defun test (message)
  (handle-ws message))
(defvar *running* nil)
(defvar *seq* nil)
(defvar *sleep* 41.0)
(defvar *client*
  (wsd:make-client "wss://gateway.discord.gg/?v=6&encoding=json"))
(defun hb (seq)
  (princ "heartbeat")
  (terpri)
  (wsd:send
   (ws-client *discord*)
   (json:encode-json-alist-to-string `((:op . 1) (:d . ,seq))))
  (sleep *sleep*))
(defun tick ()
  (hb *seq*))
(defun handle-hb ()
  (unless *running*
    (unwind-protect
         (progn
           (princ "starting heartbeat")
           (terpri)
           (setf *running* t)
           (loop
             while *running* do
               (continuable (tick))))
      (princ "done beating")
      (terpri)
      (setf *running* nil))))

(defun do-hb (discord)
  (let ((time (next-hb-time discord)))
    (with-lock-held ((hb-lock discord))
      (when (eql time (next-hb-time discord))
        (wsd:send (ws-client discord)
                  (write-json-to-string (jso "op" 1 "d" (seq discord))))
        (format t "Heartbeat~%")
        (setf (next-hb-time discord) (+ (hb-interval discord) (get-time)))))))
(defun make-hb-handler (discord)
  (lambda ()
    (do-hb discord)
    (loop
      do
         (sleep (let ((time (- (next-hb-time discord) (get-time) 1)))
                  (if (positive-real-p time)
                      time
                      0)))
         (when (> 1 (- (next-hb-time discord) (get-time)))
           (do-hb discord)))))

(defun handle-ready (event)
  event)
(defun channel-name (chid)
  (let* ((res (execute-single
               (db *discord*)
               "select name from channels where id = ?"
               chid)))
    (unless res
      (let* ((chj (get-channel chid))
             (ch (json:decode-json-from-string chj))
             (sid (cdr (assoc :guild--id ch)))
             (name (cdr (assoc :name ch))))
        (execute-non-query
         (db *discord*)
         "insert into channels (id, server, name) values (?, ?, ?)"
         chid
         sid
         name)
        (setf res name)))
    res))
(defun server-of-channel (channel-id)
  (let* ((res (execute-single
               (db *discord*)
               "select server from channels where id = ?"
               channel-id))
         (ret res))
    (unless ret
      (let* ((channel-json (get-channel channel-id))
             (channel (json:decode-json-from-string channel-json))
             (sid (cdr (assoc :guild--id channel)))
             (name (cdr (assoc :name channel))))
        (execute-non-query
         (db *discord*)
         "insert or ignore into channels (id, server, name) values (?, ?, ?)"
         channel-id
         sid
         name)
        (execute-non-query
         (db *discord*)
         "update channels
set server=?, name=?
where changes()=0
    and id=?"
         sid
         name
         channel-id)
        (setf ret sid)))
    ret))
(defun server-name (sid)
  (let* ((ret (execute-single
               (db *discord*)
               "select name from servers where id = ?"
               sid)))
    (if ret
        ret
        (let*
            ((sj (req (format nil "/guilds/~a" sid)))
             (s (json:decode-json-from-string sj))
             (name (cdr (assoc :name s)))
             (owner (cdr (assoc :owner--id s))))
          (execute-non-query
           (db *discord*)
           "insert into servers (id, name, owner) values (?, ?, ?)"
           sid
           name
           owner)
          name))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  ;; Stolen from the cl cookbook
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun is (comparator &key (test #'eql))
  (lambda (item)
    (funcall test comparator item)))

(defun one-of (&rest items)
  (lambda (item)
    (some (is item) items)))


(defvar *queue* (make-queue :simple-cqueue))
(defvar *queuecond* (make-condition-variable :name "global queue wait condition"))
(defvar *queue-handler-lock* (make-lock "condlock"))
                                        ;(defvar *channel-queues* )

(defun queue-handler ()
  (unless (acquire-lock *queue-handler-lock* nil)
    (return-from queue-handler))
  (loop (let ((action (qpop *queue*)))
          (unless action
            (condition-wait *queuecond* *queue-handler-lock*)
            (setf action (qpop *queue*)))
          (make-thread (lambda ()
                         (funcall action))
                       :name "active action")
          (sleep 1.1))))


(defun queue-message-async (chid msg)
  (qpush *queue* (lambda ()
                   (send-message chid msg)))
  (condition-notify *queuecond*))


(defun queue-message (chid msg)
  (let ((lock (make-lock (format nil "queue message in ~a" chid)))
        (cond (make-condition-variable))
        (ret))
    (acquire-lock lock)
    (qpush *queue* (lambda ()
                     (setf ret (send-message chid msg))
                     (condition-notify cond)))
    (condition-notify *queuecond*)
    (condition-wait cond lock)
    ret))

(defun queue-edit-message-async (chid msgid new)
  (make-thread (lambda ()
                 (qpush *queue* (lambda ()
                                  (edit-message chid msgid new)))
                 (condition-notify *queuecond*))))


(defvar *auto-reconnect* t)
(defvar *testtt* t)

(defun event (event type)
  (cond
    ((equal "READY" type) nil)
    ((equal "MESSAGE_CREATE" type)
     (make-thread (lambda ()
                    (funcall (cb *discord*) event))
                  :name "message_create"))))

(defun make-ws-handler (discord)
  (lambda (message)
    (let* ((obj (read-json-from-string message))
           (op (getjso "op" obj))
           (data (getjso "d" obj)))
      (cond
        ((eq op +dispatch+)
         (let ((seq (getjso "s" obj))
               (type (getjso "t" obj)))
           (setf *seq* seq)
           (event data type)))
        ((eq op +heartbeat+)
         (princ "Heartbeat from server")
         (do-hb discord)
         (terpri))
        ((eq op +reconnect+) (format t "Told me to reconnect: ~a~%" data))
        ((eq op +invalid-session+) (format t "Invalid session: ~a~%" data))
        ((eq op +hello+)
         (format t "HELLO: ~a~%" message)
         (setf (hb-interval discord) (/ (getjso "heartbeat_interval" data) 1000)
               (_trace discord) (getjso "_trace" data))
         (wsd:send (ws-client discord)
                   (write-json-to-string
                    (jso "op" +identify+
                         "d" (jso "token" (token *discord*)
                                  "properties" (jso "$os" "linux"
                                                    "$browser" "cl-harmony"
                                                    "$device" "cl-harmony")
                                  "compress" (as-json-bool nil)
                                  "large_threshold" 50
                                  "shard" '(0 1)
                                  "presence" (jso "afk" (as-json-bool nil)
                                                  "game" (jso "name" "Type &help"
                                                              "type" 0)
                                                  "since" :null
                                                  "status" "online"))))))
        ((eq op +beat-ack+)
         (princ "HB ACK")
         (terpri))
        (t (princ "fucker: ")
           (princ message)
           (terpri)
           (finish-output))))))
