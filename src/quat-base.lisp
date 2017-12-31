(in-package :gamebox-math)

(deftype quat () '(simple-array single-float (4)))

(defstruct (quat (:type (vector single-float))
                 (:constructor %quat (w x y z))
                 (:conc-name q)
                 (:copier nil)
                 (:predicate nil))
  (w 0.0f0 :type single-float)
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (z 0.0f0 :type single-float))

(declaim (inline quat))
(defun* (quat -> quat) ((w real) (x real) (y real) (z real))
  (%quat (float w 1.0f0) (float x 1.0f0) (float y 1.0f0) (float z 1.0f0)))

(defmacro with-quat (binds &body body)
  (if (null binds)
      `(progn ,@body)
      (let ((prefix (caar binds)))
        `(with-accessors ((,prefix identity)
                          (,(make-accessor-symbol prefix '.w) qw)
                          (,(make-accessor-symbol prefix '.x) qx)
                          (,(make-accessor-symbol prefix '.y) qy)
                          (,(make-accessor-symbol prefix '.z) qz))
             ,(cadar binds)
           (with-quat ,(cdr binds) ,@body)))))

(set-pprint-dispatch
 'quat
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-quat ((q object))
       (format stream "~f ~f ~f ~f" q.w q.x q.y q.z))))
 1)
