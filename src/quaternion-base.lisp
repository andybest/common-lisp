(in-package :gamebox-math)

(deftype quat () '(simple-array single-float (4)))

(defstruct (quat (:type (vector single-float))
                 (:constructor %quat (&optional w x y z))
                 (:conc-name q)
                 (:copier nil)
                 (:predicate nil))
  (w 0.0 :type single-float)
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defun* quat (&optional ((w real) 0) ((x real) 0) ((y real) 0) ((z real) 0))
    (:result quat)
  "Create a new quaternion."
  (%quat (float w 1.0) (float x 1.0) (float y 1.0) (float z 1.0)))

(defmacro with-quat ((prefix quat) &body body)
  "A convenience macro for concisely accessing components of a quaternion.
Example: (with-quat (q quat) qw) would allow accessing the W component of the
quaternion as simply the symbol QW."
  `(with-accessors ((,prefix identity)
                    (,(symbolicate prefix 'w) qw)
                    (,(symbolicate prefix 'x) qx)
                    (,(symbolicate prefix 'y) qy)
                    (,(symbolicate prefix 'z) qz))
       ,quat
     ,@body))

(defmacro with-quats (binds &body body)
  "A convenience macro for concisely accessing components of multiple
quaternions.
Example: (with-quats ((a quaternion1) (b quaternion2)) (values ax by)) would
access the X component of quaternion1, and the Y component of quaternion2."
  (if (null binds)
      `(progn ,@body)
      `(with-quat ,(car binds)
         (with-quats ,(cdr binds) ,@body))))

(defun* qref ((quat quat) (index (integer 0 3))) (:result single-float)
  "A virtualized quaternion component reader. Use this instead of AREF to
prevent unintended behavior should ordering of a quaternion ever change."
  (aref quat index))

(defun* (setf qref) ((value single-float) (quat quat) (index (integer 0 3)))
    (:result single-float)
  "A virtualized quaternion component writer. Use this instead of (SETF AREF) ~
to prevent unintended behavior should ordering of a quaternion ever change."
  (setf (aref quat index) value))

(set-pprint-dispatch
 'quat
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-quat (o object)
       (format stream "~f ~f ~f ~f" ow ox oy oz))))
 1)
