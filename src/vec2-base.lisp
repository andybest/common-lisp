(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (deftype vec2 () '(simple-array single-float (2)))

  (defstruct (vec2 (:type (vector single-float))
                   (:constructor %v2 (x y))
                   (:conc-name v2)
                   (:copier nil)
                   (:predicate nil))
    (x 0.0f0 :type single-float)
    (y 0.0f0 :type single-float))

  (declaim (inline v2))
  (defun* (v2 -> vec2) ((x real) (y real))
    (%v2 (float x 1.0f0) (float y 1.0f0)))

  (define-constant +v2zero+ (v2 0 0) :test #'equalp))

(defmacro with-vec2 (binds &body body)
  (if (null binds)
      `(progn ,@body)
      (let ((prefix (caar binds)))
        `(with-accessors ((,prefix identity)
                          (,(make-accessor-symbol prefix '.x) v2x)
                          (,(make-accessor-symbol prefix '.y) v2y))
             ,(cadar binds)
           (with-vec2 ,(cdr binds) ,@body)))))

(set-pprint-dispatch
 'vec2
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vec2 ((v object))
       (format stream "~f ~f" v.x v.y))))
 1)
