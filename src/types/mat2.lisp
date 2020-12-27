(in-package #:cl-user)

(defpackage #:origin.mat2
  (:local-nicknames
   (#:com #:origin.common)
   (#:ss #:specialization-store)
   (#:u #:golden-utils)
   (#:v2 #:origin.vec2))
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

(in-package #:origin.mat2)

(deftype mat () '(simple-array u:f32 (4)))

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

;;; constructors

(u:fn-> %mat (u:f32 u:f32 u:f32 u:f32) mat)
(declaim (inline %mat))
(u:eval-always
  (defun %mat (m00 m10 m01 m11)
    (declare (optimize speed))
    (let ((mat (make-array 4 :element-type 'u:f32)))
      (setf (aref mat 0) m00
            (aref mat 1) m10
            (aref mat 2) m01
            (aref mat 3) m11)
      mat)))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0.0 0.0
        0.0 0.0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1.0) 0.0
        0.0 (float x 1.0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (%mat (aref mat 0) (aref mat 1)
        (aref mat 2) (aref mat 3)))

(ss:defspecialization (mat :inline t) ((mat (simple-array u:f32 (9)))) mat
  (%mat (aref mat 0) (aref mat 3)
        (aref mat 1) (aref mat 4)))

(ss:defspecialization (mat :inline t) ((mat (simple-array u:f32 (16)))) mat
  (%mat (aref mat 0) (aref mat 4)
        (aref mat 1) (aref mat 5)))

(ss:defspecialization (mat :inline t) ((vec1 v2:vec) (vec2 v2:vec)) mat
  (%mat (aref vec1 0) (aref vec1 1)
        (aref vec2 0) (aref vec2 1)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real) (d real)) mat
  (%mat (float a 1.0) (float b 1.0)
        (float c 1.0) (float d 1.0)))

(ss:defspecialization (mat :inline t) ((mat (simple-array  u:f64 (4)))) mat
  (%mat (float (aref mat 0) 1.0) (float (aref mat 1) 1.0)
        (float (aref mat 2) 1.0) (float (aref mat 3) 1.0)))

;;; constants

(u:define-constant +zero+ (%mat 0.0 0.0 0.0 0.0) :test #'equalp)

(u:define-constant +id+ (%mat 1.0 0.0 0.0 1.0) :test #'equalp)
