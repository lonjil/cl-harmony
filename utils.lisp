(in-package #:cl-harmony)
;;; Got this jso stuff from sjl

(defgeneric to-jso (object))

(defmethod to-jso (o)
  o)

(defmethod to-jso ((o vector))
  (map 'list #'to-jso o))

(defmethod to-jso ((o string))
  o)

(defmethod to-jso ((o list))
  (apply #'st-json:jso (mapcar #'to-jso o)))


(defgeneric from-jso (object))

(defmethod from-jso (o)
  o)

(defmethod from-jso ((o list))
  (map 'vector #'from-jso o))

(defmethod from-jso ((o st-json:jso))
  (let ((foo))
   (st-json:mapjso (lambda (k v)
                     (push (from-jso k) foo)
                     (push (from-jso v) foo))
                   o)
    (nreverse foo)))


(defun json-to-object (string)
  (if (string= "" string)
      :empty
      (from-jso (st-json:read-json-from-string string))))

(defun object-to-json (object)
  (if (eq :empty object)
      ""
      (st-json:write-json-to-string (to-jso object))))

(cffi:defcstruct timespec
  (seconds :long)
  (nanoseconds :long))
(cffi:defcfun "clock_gettime" :int
  (clock-type :int)
  (timespec (:pointer (:struct timespec))))

(defun get-ns-time ()
  (cffi:with-foreign-object (ts '(:pointer (:struct timespec)))
    (clock-gettime 0 ts)
    (cffi:with-foreign-slots ((seconds nanoseconds) ts (:struct timespec))
      (+ (* 1000000000 seconds) nanoseconds))))
(defun get-time ()
  (/ (get-ns-time) 1000000000))
