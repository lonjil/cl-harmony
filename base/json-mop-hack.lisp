(in-package #:xyz.lonjil.discord/base)

(defmethod json-mop:to-json-value ((value json-mop:json-serializable)
                                   (json-type (eql :hash-table)))
  value)
(defmethod json-mop:json-to-clos ((input vector) class &rest initargs)
  (map 'list (lambda (x)
                 (apply #'json-mop:json-to-clos x class initargs))
       input))

(defun encode (object)
  (with-output-to-string (s)
    (json-mop:encode object s)))

(defvar *signal-on-unbound* nil)
(defmethod slot-unbound (class (obj json-mop:json-serializable) slot)
  (if *signal-on-unbound*
      (error (make-condition 'unbound-slot :instance obj))
      nil))

(defmethod json-mop:encode :around
    ((object json-mop:json-serializable) &optional (stream *standard-output*))
  (declare (ignore stream))
  (let ((*signal-on-unbound* t))
    (call-next-method)))
