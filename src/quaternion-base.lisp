(in-package :gamebox-math)

(deftype quat () '(simple-array single-float (4)))

(defstruct (quat (:type (vector single-float))
                 (:conc-name q))
  (w 1.0 :type single-float)
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defun* quat (&optional ((w 1.0) real) ((x 0.0) real) ((y 0.0) real) ((z 0.0) real)) (:result quat)
  (make-array 4 :element-type 'single-float
                :initial-contents (list (float w 1.0) (float x 1.0) (float y 1.0) (float z 1.0))))

(defmacro with-quat ((prefix quat) &body body)
  `(with-accessors ((,prefix identity)
                    (,(symbolicate prefix 'w) qw)
                    (,(symbolicate prefix 'x) qx)
                    (,(symbolicate prefix 'y) qy)
                    (,(symbolicate prefix 'z) qz))
       ,quat
     ,@body))

(defmacro with-quats (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(with-quat ,(car binds)
         (with-quats ,(cdr binds) ,@body))))

(defun* qref ((quat quat) (index (integer 0 3))) (:result single-float)
  (aref quat index))

(defun* (setf qref) ((value single-float) (quat quat) (index (integer 0 3))) (:result single-float)
  (setf (aref quat index) value))

(set-pprint-dispatch
 'quat
 (lambda (stream object)
   (print-unreadable-object (object stream)
     (with-quat (o object)
       (format stream "~f ~f ~f ~f" ow ox oy oz))))
 1)
