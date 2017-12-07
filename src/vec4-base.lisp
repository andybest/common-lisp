(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (deftype vec4 () '(simple-array single-float (4)))

  (defstruct (vec4 (:type (vector single-float))
                   (:constructor %vec4 (x y z w))
                   (:conc-name v4)
                   (:copier nil)
                   (:predicate nil))
    (x 0.0 :type single-float)
    (y 0.0 :type single-float)
    (z 0.0 :type single-float)
    (w 0.0 :type single-float))

  (defun* vec4 ((x real) (y real) (z real) (w real)) (:result vec4)
    "Create a new vector."
    (%vec4 (float x 1.0) (float y 1.0) (float z 1.0) (float w 1.0)))

  (define-constant +zero-vec4+ (vec4 0 0 0 0) :test #'equalp))

(defmacro with-vec4 ((prefix vec) &body body)
  "A convenience macro for concisely accessing components of a vector.
Example: (with-vec4 (v vector) vw) would allow accessing the W component of the
vector as simply the symbol VW."
  `(with-accessors ((,prefix identity)
                    (,(symbolicate prefix 'x) v4x)
                    (,(symbolicate prefix 'y) v4y)
                    (,(symbolicate prefix 'z) v4z)
                    (,(symbolicate prefix 'w) v4w))
       ,vec
     ,@body))

(defmacro with-vec4s (binds &body body)
  "A convenience macro for concisely accessing components of multiple vectors.
Example: (with-vec4s ((a vector1) (b vector2) (c vector3)) (values ax by cz))
would access the X component of vector1, the Y component of vector2, and the Z
component of vector3."
  (if (null binds)
      `(progn ,@body)
      `(with-vec4 ,(car binds)
         (with-vec4s ,(cdr binds) ,@body))))

(defun* v4ref ((vec vec4) (index (integer 0 3))) (:result single-float)
  "A virtualized vec4 component reader. Use this instead of AREF to prevent
unintended behavior should ordering of a vec4 ever change."
  (aref vec index))

(defun* (setf v4ref) ((value single-float) (vec vec4) (index (integer 0 3)))
    (:result single-float)
  "A virtualized vec4 component writer. Use this instead of (SETF AREF) to
prevent unintended behavior should ordering of a vec4 ever change."
  (setf (aref vec index) value))

(set-pprint-dispatch
 'vec4
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vec4 (v object)
       (format stream "~f ~f ~f ~f" vx vy vz vw))))
 1)
