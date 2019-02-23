(in-package #:xyz.lonjil.discord/logger)

(defvar *important* *standard-output*)
(defvar *info* (open "info-log.txt" :direction :output :if-exists :rename-and-delete :if-does-not-exist :create))


(defun logm (level control-string &rest format-arguments)
  (let ((outs (case level
                (:debug (list *info*))
                (:info (list *standard-output* *info*))
                (:important (list *standard-output* *info*))
                (otherwise (list *standard-output* *info*)))))
    (loop :for out :in outs
          :do (format out "[~a] ~a: " level (time-string))
              (apply 'format out control-string format-arguments)
              (terpri out)
              (finish-output out))))
(defun time-string ()
  (multiple-value-bind
        (sec min hour day month year %day daylight-savings-p timezone)
      (get-decoded-time)
    (declare (ignorable %day daylight-savings-p))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~@d"
            year month day hour min sec (+ (- timezone)
                                           (if daylight-savings-p
                                               1
                                               0)))))
