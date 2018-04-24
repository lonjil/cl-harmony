(defclass channel-queue ()
  ((handler-thread :accessor handler-thread :initarg :handler-thread)
   (queue :accessor queue :initarg :queue)
   (lock :accessor lock :initarg :lock)
   (cv :accessor cv :initarg :cv))
  (:documentation "Auto-rate limiting channel action queue."))

(defun make-queue-handler (channel-queue)
  (lambda ()
    (with-slots (queue lock cv) channel-queue
      (loop
        (acquire-lock lock)
        (unless (qtop queue)
          (condition-wait cv lock))
        (let ((x (qpop queue)))
          (when x
            (funcall x)))
        (release-lock lock)
        (sleep 1.1)))))

(defun make-channel-queue (&optional (thread-name "Anon channel thread"))
  (let ((foo (make-instance 'channel-queue
                            :queue (make-queue :simple-queue)
                            :lock (make-lock)
                            :cv (make-condition-variable))))
    (setf (handler-thread foo) (make-thread (make-queue-handler foo) :name thread-name))
    foo))

(defun queue-channel-action (channel discord func)
  (let ((x (gethash channel (channel-queue-map discord))))
    (unless x
      (setf x (make-channel-queue (format nil "Ch queue: ~a" channel))
            (gethash channel (channel-queue-map discord)) x))
    (with-lock-held ((lock x))
      (qpush (queue x) func))
    (condition-notify (cv x))))
