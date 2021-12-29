(in-package #:cl-user)

(defpackage #:mfiano.math.origin.dmat2
  (:local-nicknames
   (#:dv2 #:mfiano.math.origin.dvec2)
   (#:com #:mfiano.math.origin.common)
   (#:ss #:specialization-store)
   (#:m2 #:mfiano.math.origin.mat2)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:pretty-print
   #:+zero+
   #:+id+
   #:zero
   #:zero!
   #:zero-p
   #:random
   #:id
   #:id!
   #:id-p
   #:=
   #:copy!
   #:copy
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotation-from-angle!
   #:rotation-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v2!
   #:*v2
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(in-package #:mfiano.math.origin.dmat2)

(deftype mat () '(u:f64a 4))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (u:once-only (matrix)
    `(symbol-macrolet
         ((,prefix ,matrix)
          (,(com:make-accessor-symbol prefix "00") (aref ,matrix 0))
          (,(com:make-accessor-symbol prefix "10") (aref ,matrix 1))
          (,(com:make-accessor-symbol prefix "01") (aref ,matrix 2))
          (,(com:make-accessor-symbol prefix "11") (aref ,matrix 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f~% ~,6f, ~,6f]" m00 m01 m10 m11)))

;;; Constructors

(u:fn-> %mat (u:f64 u:f64 u:f64 u:f64) mat)
(declaim (inline %mat))
(u:eval-always
  (defun %mat (m00 m10 m01 m11)
    (declare (optimize speed))
    (let ((mat (u:make-f64-array 4)))
      (setf (aref mat 0) m00
            (aref mat 1) m10
            (aref mat 2) m01
            (aref mat 3) m11)
      mat)))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0d0 0d0 0d0 0d0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1d0) 0d0 0d0 (float x 1d0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (%mat (aref mat 0) (aref mat 2) (aref mat 1) (aref mat 3)))

(ss:defspecialization (mat :inline t) ((mat (u:f64a 9))) mat
  (%mat (aref mat 0) (aref mat 3) (aref mat 1) (aref mat 4)))

(ss:defspecialization (mat :inline t) ((mat (u:f64a 16))) mat
  (%mat (aref mat 0) (aref mat 4) (aref mat 1) (aref mat 5)))

(ss:defspecialization (mat :inline t) ((vec1 dv2:vec) (vec2 dv2:vec)) mat
  (%mat (aref vec1 0) (aref vec1 1) (aref vec2 0) (aref vec2 1)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real) (d real)) mat
  (%mat (float a 1d0) (float b 1d0) (float c 1d0) (float d 1d0)))

(ss:defspecialization (mat :inline t) ((mat m2:mat)) mat
  (%mat (float (aref mat 0) 1d0) (float (aref mat 1) 1d0)
        (float (aref mat 2) 1d0) (float (aref mat 3) 1d0)))

;;; Constants

(u:define-constant +zero+ (%mat 0d0 0d0 0d0 0d0) :test #'equalp)

(u:define-constant +id+ (%mat 1d0 0d0 0d0 1d0) :test #'equalp)
