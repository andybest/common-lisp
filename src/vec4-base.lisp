(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (deftype vec4 () '(simple-array single-float (4)))

  (defstruct (vec4 (:type (vector single-float))
                   (:constructor %vec4 (x y z w))
                   (:conc-name v4)
                   (:copier nil)
                   #-ecl(:predicate nil))
    (x 0.0f0 :type single-float)
    (y 0.0f0 :type single-float)
    (z 0.0f0 :type single-float)
    (w 0.0f0 :type single-float))

  (declaim (inline vec4))
  (defun* (vec4 -> vec4) ((x real) (y real) (z real) (w real))
    (%vec4 (float x 1.0f0) (float y 1.0f0) (float z 1.0f0) (float w 1.0f0)))

  (define-constant +v4zero+ (vec4 0 0 0 0) :test #'equalp))

(defmacro with-vec4 (binds &body body)
  (if (null binds)
      `(progn ,@body)
      (let ((prefix (caar binds)))
        `(with-accessors ((,prefix identity)
                          (,(make-accessor-symbol prefix '.x) v4x)
                          (,(make-accessor-symbol prefix '.y) v4y)
                          (,(make-accessor-symbol prefix '.z) v4z)
                          (,(make-accessor-symbol prefix '.w) v4w))
             ,(cadar binds)
           (with-vec4 ,(cdr binds) ,@body)))))

(set-pprint-dispatch
 'vec4
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vec4 ((v object))
       (format stream "~f ~f ~f ~f" v.x v.y v.z v.w))))
 1)
