(in-package #:xyz.lonjil.discord/base)

(defvar *api* "https://discordapp.com/api/v6")
(defvar *ua* "cl-harmony (https://github.com, 0.2.0)")

(defun make-req-url (path &optional params)
  (format nil "~a~(~{/~a~}~@[?~{~a=~a~^&~}~]~)" *api* path params))

(defgeneric req (discord type path
                 &key params content parser class
                 &allow-other-keys)
  (:documentation "Send a request to the discord API"))

(defmethod req ((discord discord-base) type path
                &key params content parser class &allow-other-keys
                &aux (url (make-req-url path params)))
  (destructuring-bind (body . rest)
      (multiple-value-list
       (drakma:http-request
        url :method type
        :content-type "application/json" :user-agent *ua*
        :user-agent "cl-harmony (https://github.com/lonjil/cl-harmony, 0.2.0)"
        :additional-headers
        (list (cons "Authorization"
                    (format nil "Bot ~a" (token discord))))
        :content content
        :content-length (length content)))
    (let* ((body (if (stringp body)
                     body
                     (flexi-streams:octets-to-string body)))
           (body (cond
                   ((= (length body) 0) nil)
                   (t
                    (if class
                          (json-mop:json-to-clos body class)
                          (yason:parse body)))))
           (body (if (and parser body)
                     (funcall parser body)
                     body)))
      (values body rest))))

(defclass limiter ()
  ((%limit :accessor limit :initarg :limit)
   (%remaining :accessor remaining :initarg :remaining)
   (%reset :accessor reset :initarg :reset)))

(defmethod req ((discord discord) type path
                &key params content)
  (declare (ignore params content))
  (let* ((now (s:get-unix-time))
         (key (if (= (mod (length path) 2) 1)
                  (reverse (cdr (reverse path)))
                  path))
         (limiter (gethash key (limits discord))))
    (if (or (null limiter)
            (not (= 0 (remaining limiter)))
            (< (reset limiter) now)
            (sleep (- (reset limiter) now))
            t)
        (multiple-value-bind (ret rest)
            (call-next-method)
          (let* ((num (car rest))
                 (headers (cadr rest))
                 (limit (cdr (assoc :x-ratelimit-limit headers)))
                 (remaining (cdr (assoc :x-ratelimit-remaining headers)))
                 (reset (cdr (assoc :x-ratelimit-reset headers))))
            (unless (<= 200 num 299)
              (l:logm :important "~a~%~a" ret rest))
            (when limit
              (unless limiter
                (setf limiter (make-instance 'limiter)
                      (gethash key (limits discord)) limiter))
              (setf (limit limiter) (parse-integer limit)
                    (remaining limiter) (parse-integer remaining)
                    (reset limiter) (parse-integer reset))))
          (values ret rest)))))

(defun send-message (channel-id content &optional (discord *discord*))
  (req discord :post `(channels ,channel-id messages)
               :content (encode (make-instance 'message :content content))
               :class 'message))

(defun get-invites (guild &optional (discord *discord*))
  (req discord :get `(guilds ,guild invites)
               :class 'invite))

(defun add-member-role (guild user role &optional (discord *discord*))
  (req discord :put `(guilds ,guild members ,user roles ,role)
       :content "{}"))
