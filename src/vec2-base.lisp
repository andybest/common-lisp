(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (deftype vec2 () '(simple-array single-float (2)))

  (defstruct (vec2 (:type (vector single-float))
                   (:constructor %vec2 (x y))
                   (:conc-name v2)
                   (:copier nil)
                   (:predicate nil))
    (x 0.0 :type single-float)
    (y 0.0 :type single-float))

  (defun* vec2 ((x real) (y real)) (:result vec2)
    "Create a new vector."
    (%vec2 (float x 1.0) (float y 1.0)))

  (define-constant +zero-vec2+ (vec2 0 0) :test #'equalp))

(defmacro with-vec2 ((prefix vec) &body body)
  "A convenience macro for concisely accessing components of a vector.
Example: (with-vec2 (v vector) vy) would allow accessing the Y component of the
vector as simply the symbol VY."
  `(with-accessors ((,prefix identity)
                    (,(symbolicate prefix 'x) v2x)
                    (,(symbolicate prefix 'y) v2y))
       ,vec
     ,@body))

(defmacro with-vec2s (binds &body body)
  "A convenience macro for concisely accessing components of multiple vectors.
Example: (with-vec2s ((a vector1) (b vector2)) (values ax by))
would access the X component of vector1, and the Y component of vector2."
  (if (null binds)
      `(progn ,@body)
      `(with-vec2 ,(car binds)
         (with-vec2s ,(cdr binds) ,@body))))

(defun* v2ref ((vec vec2) (index (integer 0 1))) (:result single-float)
  "A virtualized vec2 component reader. Use this instead of AREF to prevent
unintended behavior should ordering of a vec2 ever change."
  (aref vec index))

(defun* (setf v2ref) ((value single-float) (vec vec2) (index (integer 0 1)))
    (:result single-float)
  "A virtualized vec2 component writer. Use this instead of (SETF AREF) to
prevent unintended behavior should ordering of a vec2 ever change."
  (setf (aref vec index) value))

(set-pprint-dispatch
 'vec2
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vec2 (v object)
       (format stream "~f ~f" vx vy))))
 1)
