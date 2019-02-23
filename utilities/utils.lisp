(in-package #:xyz.lonjil.discord/utilities)

(defun get-time ()
  (let ((x (local-time:now)))
    (+ (local-time:timestamp-to-unix x)
       (/ (local-time:nsec-of x)
          1000000000))))
