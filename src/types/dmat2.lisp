(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.dmat2
  (:local-nicknames
   (#:dv2 #:net.mfiano.lisp.origin.dvec2)
   (#:com #:net.mfiano.lisp.origin.common)
   (#:ss #:specialization-store)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:u #:net.mfiano.lisp.golden-utils))
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
   #:with-elements
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

(in-package #:net.mfiano.lisp.origin.dmat2)

(deftype mat () '(simple-array u:f64 (4)))

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

(defmacro with-elements (((prefix m00 m01 m10 m11) &rest rest) &body body)
  (let ((%m00 (com:make-accessor-symbol prefix "00"))
        (%m10 (com:make-accessor-symbol prefix "10"))
        (%m01 (com:make-accessor-symbol prefix "01"))
        (%m11 (com:make-accessor-symbol prefix "11")))
    `(let ((,%m00 ,m00) (,%m10 ,m10) (,%m01 ,m01) (,%m11 ,m11))
       (declare (ignorable ,%m00 ,%m10 ,%m01 ,%m11))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f~% ~,6f, ~,6f]" m00 m01 m10 m11)))

;;; constructors

(u:fn-> %mat (&rest u:f64) mat)
(declaim (inline %mat))
(u:eval-always
  (defun %mat (&rest args)
    (declare (optimize speed))
    (make-array 4 :element-type 'u:f64 :initial-contents args)))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0d0 0d0
        0d0 0d0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1d0) 0d0
        0d0 (float x 1d0)))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (%mat (aref mat 0) (aref mat 2)
        (aref mat 1) (aref mat 3)))

(ss:defspecialization (mat :inline t) ((mat (simple-array u:f64 (9)))) mat
  (%mat (aref mat 0) (aref mat 3)
        (aref mat 1) (aref mat 4)))

(ss:defspecialization (mat :inline t) ((mat (simple-array u:f64 (16)))) mat
  (%mat (aref mat 0) (aref mat 4)
        (aref mat 1) (aref mat 5)))

(ss:defspecialization (mat :inline t) ((vec1 dv2:vec) (vec2 dv2:vec)) mat
  (%mat (aref vec1 0) (aref vec1 1)
        (aref vec2 0) (aref vec2 1)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real) (d real)) mat
  (%mat (float a 1d0) (float b 1d0)
        (float c 1d0) (float d 1d0)))

(ss:defspecialization (mat :inline t) ((mat m2:mat)) mat
  (%mat (float (aref mat 0) 1d0) (float (aref mat 1) 1d0)
        (float (aref mat 2) 1d0) (float (aref mat 3) 1d0)))

;;; constants

(u:define-constant +zero+ (%mat 0d0 0d0 0d0 0d0) :test #'equalp)

(u:define-constant +id+ (%mat 1d0 0d0 0d0 1d0) :test #'equalp)
