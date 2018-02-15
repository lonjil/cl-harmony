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

(defvar *key* (with-open-file (foo "./key.txt")
                (read-line foo)))
(defvar *discord*
  (make-discord *key*))

(defvar *api* "https://discordapp.com/api/v6")
                                        ;(defvar *)

(defun req (uri &key (type :get) content (discord *discord*))
  (when (and (member type '(:post :patch :put))
             (eq content nil))
      (error "NO CONTENT AAGGAGAGAGHGHAHAHADHDGH"))
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
           `(("Authorization" . ,(format nil "Bot ~a" (token discord))))
           :content content
           :content-length (length content)))
         (body (if (stringp body-raw)
                   body-raw
                   (flexi-streams:octets-to-string body-raw))))
    (values body status headers uri)))

(defvar *chanrates* nil)
;(defun get-remaining)

(defun send-message (channel-id message &key embed)
  "Sends a message, whoa!"
  (req (format nil "/channels/~a/messages" channel-id)
       :type :post
       :content (json:encode-json-alist-to-string
                 `((:content . ,message)
                   .
                   ,(if embed
                        `(:embed . ,embed)
                        nil)))))
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
         "insert into channels (id, server, name) values (?, ?, ?)"
         channel-id
         sid
         name)
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

(defmacro mkcmd (name &body body)
  (let ((llist '(args chid author id)))
    `(cons ,(string name)
           (named-lambda ,name ,llist
             (declare (ignorable ,@llist))
             (make-thread
              (lambda ()
                ,@body)
              :name ,(string name))))))
(defmacro mkpcmd (name &body body)
  (let ((llist '(args chid author id)))
    `(cons ,(string name)
           (named-lambda ,name ,llist
             (declare (ignorable ,@llist))
             (make-thread
              (lambda ()
                (when (equal id "167023260574154752")
                  ,@body))
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
                           "fortune -list: lists fortune types"
                           "help: prints this"
                           "roll <number>: roll a die with <number> faces"
                           "roll <count>d<number>: roll <count> dice with <number> faces")))))

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
(defvar *fortune-types* "art
ascii-art
computers
cookie
debian
definitions
drugs
education
ethnic
food
fortunes
goedel
humorists
kids
knghtbrd
law
linux
linuxcookie
literature
love
magic
medicine
men-women
miscellaneous
news
off
paradoxum
people
perl
pets
platitudes
politics
riddles
science
songs-poems
sports
startrek
theo
translate-me
void
wisdom
work
zippy")
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
   (mkpcmd exterminate
     (queue-message chid (format nil "Extermintor now targeting ~a" args)))
   (mkpcmd rules4
     (send-embed chid :embed '((:title . "Information:")
                               (:color . #x99ccff)
                               (:description .
                                "This server has themed roles with colours, as well as roles for those who want to help others with Fortress mode, Adventure mode, and Modding. Type `t!sar get <role>` in <#201869275617558529> to get a role. Replace `<role>` with one of these:
```
Elf
Dwarf
Human
Goblin
Kobold
ASCII master race
FortHelper
AdventureHelper
ModdingHelper
```
Be advised that the last 3 roles will often be pinged.
To leave a role, do `t!sar remove` in #bot-spam.

Permanent invite to this server: https://discord.gg/CvAEMWx
Our sister servers:
Cataclysm DDA: https://discord.gg/DPxUcX7
Elona: https://discord.gg/PJw67ha"))))
   (mkpcmd rules3
     (send-embed chid :embed '((:title . "Rules:")
                               (:color . #x99ccff)
                               (:description . "
1. Try to be respectful. If you feel like someone has crossed a line, please
ping a moderator."))))
   (mkpcmd rules2
     (send-embed chid :embed '((:title . "Rules:")
                               (:color . #x99ccff)
                               (:description . "
1. Try to be respectful. If you feel like someone has crossed a line, please ping a moderator.

2. The rules are guidelines and are up to the staff's interpretation, their decision is final. If you feel you were misjudged, bring it up with another member of the staff.

3. No illegal content, no spamming, no NSFW, no NSFL. External links to such stuff is permissible if it is clearly labeled and inside <> nonembed tags.

4. Don't advertise other Discords without prior moderator permission.

5. When posting articles about abuse, please choose a source that doesn't indentify the victim. Photos should be blurred.

6. <#245937631848824833> is for general DF talk and questions.

7. Some topics belong in <#269076916139458561>. Politics and other divisive topics. Jokes, memes, especially those about the aforementioned topics.

8. Bot usage should happen in <#201869275617558529>."))))
   (mkpcmd rules
     (send-embed chid :embed '((:title . "Rules:")
                               (:fields . (((:name . "1")
                                            (:value . "Try to be respectful. If you feel like someone has crossed a line, please ping a moderator."))
                                           ((:name . "2")
                                            (:value . "The rules are guidelines and are up to the staff's interpretation, their decision is final. If you feel you were misjudged, bring it up with another member of the staff."))
                                           ((:name . "3")
                                            (:value . "No illegal content, no spamming, no NSFW, no NSFL. External links to such stuff is permissible if it is clearly labeled and inside <> nonembed tags.")))))))
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
     (if (equal (car (split-sequence #\Space args)) "-list")
         (queue-message-async chid (format nil "```~a```" *fortune-types*))
         (queue-message-async
          chid
          (format nil "```~a```"
                  (handler-case
                      (uiop:run-program `("fortune" ,args)
                                        :output '(:string :stripped t))
                    (uiop:subprocess-error () (abort)))))))
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
   (mkcmd nigga
     (queue-message-async chid "kill nigga"))
   (mkcmd eval
     (when (equal id "167023260574154752")
       (queue-message-async chid (format nil "~a" (cl:eval (read-from-string args))))))
   (mkcmd echo
     (when (equal id "167023260574154752")
       (queue-message-async chid args)))
   (mkcmd normal
     (let* ((in (parse-integer args))
            (u (random 1.0))
            (v (random 1.0))
            (x (* (sqrt (* -2 (log u))) (cos (* 2 pi v))))
            (y (* (sqrt (* -2 (log u))) (sin (* 2 pi v))))
            (a (* 0.5 (1+ x)))
            (b (* 0.5 (1+ y))))
       (declare (ignorable b))
       (princ x)
       (terpri)
       (princ a)
       (terpri)
       (queue-message-async chid (format nil "Normally distributed number from given range: ~0f" (* a in)))))
   (mkcmd roll
     (handler-case
         (bind (((count . faces)
                 (parse-dice args))
                ((:values casts total)
                 (cond
                   ((> count 1000000))
                   ((> count 300)
                    (loop repeat count
                          for x = (1+ (random faces))
                          sum x into total
                          finally (return (values nil total))))
                   (t (loop repeat count
                          for x = (1+ (random faces))
                          collect x into casts
                          sum x into total
                          finally (return (values casts total))))))
                (string (cond
                          ((> count 300)
                           (format nil "Total: ~a, mean: ~3$"
                                   total
                                   (/ total count)))
                          ((> count 1)
                           (format nil "~{~a~^, ~}~% Total: ~a, mean: ~3$"
                                   casts
                                   total
                                   (/ total count)))
                          ((= count 1)
                           (format nil "~a" (car casts))))))
           (queue-message-async chid (format nil "<@~a>: you rolled ~a"
                                             (cdr (assoc :id author))
                                             string)))
       ((or parse-error type-error) (error)
         (warn (format nil "~a" error))
         (queue-message-async chid "lmao are you stupid")
         (abort))))))

(defvar *auto-reconnect* t)
(defvar *testtt* t)

(defmacro spy-on (name id)
  `(when (equal id ,id)
     (with-open-file (f (concatenate 'string "./" ,name "_log.txt")
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
       (format f "@~a#~a in ~a#~a: ~a~%"
               (cdr (assoc :username author))
               (cdr (assoc :discriminator author))
               (server-name sid)
               (channel-name chid)
               text))))
(defun handle-message (event)
  (let* ((text (cdr (assoc :content event)))
         (author (cdr (assoc :author event)))
         (id (cdr (assoc :id author)))
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
            (funcall fun args chid author id)))))))
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
      ((eq op +dispatch+)
       (let ((seq (cdr (assoc :s obj)))
             (type (cdr (assoc :t obj))))
         (setf *seq* seq)
         (event data type)))
      ((eq op +heartbeat+)
       (princ "Heartbeat from server")
       (terpri))
      ((eq op +reconnect+) nil)
      ((eq op +invalid-session+) (format t "Invalid session: ~a" data))
      ((eq op +hello+)
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
                  +identify+
                  (token *discord*))))
      ((eq op +beat-ack+)
       (princ "HB ACK")
       (terpri))
      (t (princ "fucker: ")
         (princ message)
         (terpri)
         (finish-output)))))

(defvar *reconnect* nil)
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
          (named-lambda dc (&key reason code)
            (format t "Closed cuz '~a' (code=~a).~%" reason code)
            (setf *running* nil)))
  (wsd:on :error *client*
          (lambda (error)
            (warn (format t "wsd: ~a" error)))))
