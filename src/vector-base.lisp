(in-package :gamebox-math)

(eval-when (:compile-toplevel :load-toplevel)
  (deftype vec () '(simple-array single-float (3)))

  (defstruct (vec (:type (vector single-float))
                  (:constructor %vec (&optional x y z))
                  (:conc-name v)
                  (:copier nil)
                  (:predicate nil))
    (x 0.0 :type single-float)
    (y 0.0 :type single-float)
    (z 0.0 :type single-float))

  (defun* vec (&optional ((x real) 0) ((y real) 0) ((z real) 0)) (:result vec :inline t)
    (%vec (float x 1.0) (float y 1.0) (float z 1.0)))

  (define-constant +zero-vector+ (vec) :test #'equalp)
  (define-constant +0vec+ (vec) :test #'equalp))

(defmacro with-vector ((prefix vec) &body body)
  `(with-accessors ((,prefix identity)
                    (,(make-accessor-symbol prefix 'x) vx)
                    (,(make-accessor-symbol prefix 'y) vy)
                    (,(make-accessor-symbol prefix 'z) vz))
       ,vec
     ,@body))

(defmacro with-vectors (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(with-vector ,(car binds)
         (with-vectors ,(cdr binds) ,@body))))

(defun* vref ((vec vec) (index (integer 0 2))) (:result single-float :inline t)
  (aref vec index))

(defun* (setf vref) ((value single-float) (vec vec) (index (integer 0 2))) (:result single-float :inline t)
  (setf (aref vec index) value))

(set-pprint-dispatch
 'vec
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-vector (v object)
       (format stream "~f ~f ~f" vx vy vz))))
 1)
