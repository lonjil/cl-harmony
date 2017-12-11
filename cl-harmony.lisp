;;;; cl-harmony.lisp

(in-package #:cl-harmony)

;;; "cl-harmony" goes here. Hacks and glory await!

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :cl-interpol)
    (named-readtables:defreadtable :cl-interpol
      (:merge :current)
      (:dispatch-macro-char #\# #\? #'interpol::interpol-reader))))

(named-readtables:in-readtable :cl-interpol)

(defclass discord ()
  ((con :accessor con)
   (token :accessor token :initarg :token)
   (session :accessor seesion)
   (db :accessor db)))

(defun make-discord (token)
  "Makes a discord"
  (let* ((ret (make-instance 'discord
                             :token token)))
    (setf (db ret) (connect ":memory:"))

    ret))

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

(defvar *discord*
  (make-discord
   "redacted"))

(defvar *api* "https://discordapp.com/api/v6")
                                        ;(defvar *)

(defun req (uri &key (type :get) content (discord *discord*))
  (if (eq type :post)
      (if (eq content nil)
          (error "NO CONTENT AAGGAGAGAGHGHAHAHADHDGH")))
  (bind (((:values body-raw
                   status
                   headers
                   uri)
          (http-request
           (format nil "~a~a" *api* uri)
           :method type
           :content-type "application/json"
           :user-agent "cl-harmony (Not yet published, 0.1)"
           :additional-headers
           `(("Authorization" . ,(format nil "Bot ~a" (token discord))))
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
  (bind (((:values body status headers uri)
          (req (format nil "/channels/~a/messages" channel-id)
               :type :post
               :content (json:encode-json-alist-to-string `((:content . ,message))))))
    (make-thread (lambda ()
                   ))
    (values body status headers uri)))
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



(defvar *dispatch* 0) ;receive
(defvar *heartbeat* 1) ;both
(defvar *identify* 2) ;send
(defvar *status* 3) ;send
(defvar *voice-state* 4) ;send
(defvar *voice-ping* 5) ;send
(defvar *resume* 6) ;send
(defvar *reconnect* 7) ;receive
(defvar *request-guild-members* 8) ;send
(defvar *invalid-session* 9) ;receive
(defvar *hello* 10) ;receive
(defvar *beat-ack* 11) ;receive

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
   *client*
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

(defun handle-ready (event)
  event)

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
         "insert into channels (id, server, name) values (?, ?, ?)"
         channel-id
         sid
         name)
        (setf ret sid)))
    ret))

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

(defmacro mkcmd (name &body body)
  (let ((llist '(args chid author)))
    `(cons ,(string name)
           (named-lambda ,name ,llist
             (declare (ignorable ,@llist))
             (make-thread
              (lambda ()
                ,@body)
              :name ,(string name))))))
(defparameter *prefix* "&")
(defparameter *help*
  (format nil "```~a```"
          (apply #'concatenate
                 'string
                 (mapcar (lambda (x) #?"${*prefix*}${x}\n")
                         '("version: prints the bot version"
                           "ping: measures the time the bot takes to send a message"
                           "fortune [type]: prints a fortune of [type], or any if none is specified"
                           "help: prints this"
                           "roll [<count>d]<number>: roll <count> dice with <number> faces (one die by default)")))))

(defvar *queue* (make-queue :simple-cqueue))
(defvar *queuecond* (make-condition-variable :name "global queue wait condition"))
(defvar *queue-handler-lock* (make-lock "condlock"))
                                        ;(defvar *channel-queues* )

(defun queue-handler ()
  (unless (acquire-lock *queue-handler-lock*)
    (return-from queue-handler))
  (loop (let ((action (qpop *queue*)))
          (unless action
            (condition-wait *queuecond* *queue-handler-lock*)
            (setf action (qpop *queue*)))
          (make-thread (lambda ()
                         (funcall action))
                       :name "active action")
          (sleep 1))))

(defun queue-message-async (chid msg)
  (make-thread (lambda ()
                 (qpush *queue* (lambda ()
                                  (send-message chid msg)))
                 (condition-notify *queuecond*))
               :name (format nil "queue_message_in:~a" chid)))


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

(defun parse-dice (string)
  (let* ((dice (split-sequence #\d string))
         (count (if (cdr dice) (if (equal (car dice) "") "1" (car dice)) "1"))
         (faces (if (cdr dice) (cadr dice) (car dice))))
    (cons (parse-integer count) (parse-integer faces))))
(defparameter *commands*
  (list
   (mkcmd ascend
     )
   (mkcmd hack
     (queue-message-async chid "Activating hack mode..."))
   (mkcmd test
     (queue-message-async chid "maid!zap FV5 bunny girl"))
   (mkcmd fact
     (queue-message-async chid (format nil "~a" (factorial (parse-integer args)))))
   (mkcmd queuetime
     (let* ((pre (get-internal-real-time))
            (retj (queue-message chid "Pong!"))
            (post (get-internal-real-time))
            (ret (json:decode-json-from-string retj))
            (diff (- post pre))
            (id (cdr (assoc :id ret))))
       (queue-edit-message-async chid id
                                 (format nil
                                         "Pong! Reply took ~ams."
                                         diff))))
   (mkcmd ping
     (let* ((pre (get-internal-real-time))
            (retj (send-message chid "Pong!"))
            (post (get-internal-real-time))
            (ret (cl-json:decode-json-from-string retj))
            (diff (- post pre))
            (id (cdr (assoc :id ret)))
            (chid (cdr (assoc :channel--id ret))))
       (edit-message chid id
                     (format
                      nil
                      "Pong! Reply took ~ams."
                      diff))))
   (mkcmd version
     (queue-message-async chid "cl-harmony 0.1 everlasting alpha"))
   (mkcmd fortune
     (queue-message-async
      chid
      (format nil "```~a```"
              (handler-case
                  (uiop:run-program `("fortune" ,args)
                                    :output '(:string :stripped t))
                (uiop:subprocess-error () (abort))))))
   (mkcmd debug
     (queue-message-async
      chid
      (format nil "```\"~a\"```" args)))
   (mkcmd help
     (queue-message-async chid *help*))
   (mkcmd nazis
     (queue-message-async
      chid
      "This is what you do with nazis: https://www.youtube.com/watch?v=f7mRG88KPbA"))
   (mkcmd roll
     (handler-case
         (bind (((count . faces)
                 (parse-dice args))
                ((:values casts total)
                 (loop repeat count
                       for x = (1+ (random faces))
                       collect x into casts
                       sum x into total
                       finally (return (values casts total))))
                (string (cond
                          ((> count 300)
                           (format nil "Total: ~a, mean: ~3$"
                                   total
                                   (/ total count)))
                          ((> count 1)
                           (format nil "~{~a~^, ~}... Total: ~a, mean: ~3$"
                                   casts
                                   total
                                   (/ total count)))
                          ((= count 1)
                           (format nil "~a" (car casts))))))
           (queue-message-async chid (format nil "<@~a>: you rolled ~a"
                                             (cdr (assoc :id author))
                                             string)))
       (error (error)
         (warn (format nil "~a" error))
         (queue-message-async chid "lmao are you stupid")
         (abort))))))

(defvar *auto-reconnect* t)
(defvar *testtt* t)
(defun handle-message (event)
  (let* ((text (cdr (assoc :content event)))
         (author (cdr (assoc :author event)))
         ;(id (cdr (assoc :id author)))
         (chid (cdr (assoc :channel--id event)))
         ;(sid (server-of-channel chid))
         (prefix nil))
    ;(declare (ignore id))
    (when (>= (length text) 2)
      (setf prefix (subseq text 0 1)))
    (when (and (equal text "ayy")
               (or (equal chid "302227361338753025")
                   (equal chid "369634356798423040")
                   (equal chid "265253936900866048")
                   (equal chid "226064559788654592")
                   (equal chid "299959018687037441")))
      (send-message chid "lmao"))
    (when (equal prefix "&")
      (multiple-value-bind
            (cmd index)
          (split-sequence #\Space (subseq text 1) :count 1)
        (let* ((args (subseq text (1+ index)))
               (cmd (car cmd))
               (fun (cdr (assoc cmd *commands* :test #'equalp))))
          (when fun
            (funcall fun args chid author)))))))
(defun event (event type)
  (cond
    ((equal "READY" type) nil)
    ((equal "MESSAGE_CREATE" type)
     (make-thread (lambda ()
                    (handle-message event))
                  :name "message_create"))))

(defun handle-ws (message)
  (let* ((obj (json:decode-json-from-string message))
         (op (cdr (assoc :op obj)))
         (data (cdr (assoc :d obj))))
    (cond
      ((eq op *dispatch*)
       (let ((seq (cdr (assoc :s obj)))
             (type (cdr (assoc :t obj))))
         (setf *seq* seq)
         (event data type)))
      ((eq op *heartbeat*)
       (princ "Heartbeat from server")
       (terpri))
      ((eq op *reconnect*) nil)
      ((eq op *invalid-session*) (format t "Invalid session: ~a" data))
      ((eq op *hello*)
       (format t "HELLO: ~a~%" message)
       (wsd:send *client*
                 (format
                  nil
                  "{
\"op\":~a,
\"d\": {\"token\": \"~a\",
\"properties\": {
\"$os\": \"linux\",
\"$browser\": \"cl-harmony\",
\"$device\": \"cl-harmony\"
},
\"compress\": false,
\"large_threshold\": 50,
\"shard\": [0, 1],
\"presence\": {\"afk\": false,
\"game\": {\"name\": \"Type &help\",\"type\":0},
\"since\": null,
\"status\": \"online\"}}}"
                  *identify*
                  (token *discord*))))
      ((eq op *beat-ack*)
       (princ "HB ACK")
       (terpri))
      (t (print "fucker: ")
         (print message)
         (terpri)
         (finish-output)))))

(defun blarp ()
  (setf *client* (wsd:make-client
                  "wss://gateway.discord.gg/?v=6&encoding=json"))
  (wsd:on :message *client* (lambda (message) (handle-ws message)))
  (wsd:on :open *client*
          (lambda ()
            (format t "Connected.~%")
            (make-thread #'handle-hb :name "heartbeat")
            (when (slot-boundp (db *discord*)
                               'sqlite::handle)
              (disconnect (db *discord*)))
            (setf (db *discord*) (connect ":memory:"))
            (ready-db (db *discord*))))
  (wsd:on :close *client*
          (lambda (&key reason code)
            (format t "Closed cuz '~a' (code=~a).~%" reason code)
            (setf *running* nil)
            (when (slot-boundp (db *discord*)
                               'sqlite::handle)
              (disconnect (db *discord*)))))
  (wsd:on :error *client*
          (lambda (error)
            (warn (format t "wsd: ~a" error)))))
