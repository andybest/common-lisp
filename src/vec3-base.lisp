(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (deftype vec3 () '(simple-array single-float (3)))

  (defstruct (vec3 (:type (vector single-float))
                   (:constructor %v3 (x y z))
                   (:conc-name v3)
                   (:copier nil)
                   (:predicate nil))
    (x 0.0f0 :type single-float)
    (y 0.0f0 :type single-float)
    (z 0.0f0 :type single-float))

  (declaim (inline v3))
  (defun* (v3 -> vec3) ((x real) (y real) (z real))
    (%v3 (float x 1.0f0) (float y 1.0f0) (float z 1.0f0)))

  (define-constant +v3zero+ (v3 0 0 0) :test #'equalp))

(defmacro with-vec3 (binds &body body)
  (if (null binds)
      `(progn ,@body)
      (let ((prefix (caar binds)))
        `(with-accessors ((,prefix identity)
                          (,(make-accessor-symbol prefix '.x) v3x)
                          (,(make-accessor-symbol prefix '.y) v3y)
                          (,(make-accessor-symbol prefix '.z) v3z))
             ,(cadar binds)
           (with-vec3 ,(cdr binds) ,@body)))))

(set-pprint-dispatch
 'vec3
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vec3 ((v object))
       (format stream "~f ~f ~f" v.x v.y v.z))))
 1)
