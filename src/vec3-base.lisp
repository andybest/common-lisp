(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (deftype vec3 () '(simple-array single-float (3)))

  (defstruct (vec3 (:type (vector single-float))
                   (:constructor %vec3 (x y z))
                   (:conc-name v3)
                   (:copier nil)
                   (:predicate nil))
    (x 0.0 :type single-float)
    (y 0.0 :type single-float)
    (z 0.0 :type single-float))

  (defun* vec3 ((x real) (y real) (z real)) (:result vec3)
    "Create a new vector."
    (%vec3 (float x 1.0) (float y 1.0) (float z 1.0)))

  (define-constant +zero-vec3+ (vec3 0 0 0) :test #'equalp))

(defmacro with-vec3 ((prefix vec) &body body)
  "A convenience macro for concisely accessing components of a vector.
Example: (with-vec3 (v vector) vz) would allow accessing the Z component of the
vector as simply the symbol VZ."
  `(with-accessors ((,prefix identity)
                    (,(symbolicate prefix 'x) v3x)
                    (,(symbolicate prefix 'y) v3y)
                    (,(symbolicate prefix 'z) v3z))
       ,vec
     ,@body))

(defmacro with-vec3s (binds &body body)
  "A convenience macro for concisely accessing components of multiple vectors.
Example: (with-vec3s ((a vector1) (b vector2) (c vector3)) (values ax by cz))
would access the X component of vector1, the Y component of vector2, and the Z
component of vector3."
  (if (null binds)
      `(progn ,@body)
      `(with-vec3 ,(car binds)
         (with-vec3s ,(cdr binds) ,@body))))

(defun* v3ref ((vec vec3) (index (integer 0 2))) (:result single-float)
  "A virtualized vec3 component reader. Use this instead of AREF to prevent
unintended behavior should ordering of a vec3 ever change."
  (aref vec index))

(defun* (setf v3ref) ((value single-float) (vec vec3) (index (integer 0 2)))
    (:result single-float)
  "A virtualized vec3 component writer. Use this instead of (SETF AREF) to
prevent unintended behavior should ordering of a vec3 ever change."
  (setf (aref vec index) value))

(set-pprint-dispatch
 'vec3
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vec3 (v object)
       (format stream "~f ~f ~f" vx vy vz))))
 1)
