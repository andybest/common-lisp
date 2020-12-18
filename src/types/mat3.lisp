(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.mat3
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:ss #:specialization-store)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2)
   (#:v3 #:net.mfiano.lisp.origin.vec3))
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
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
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
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-to-mat2!
   #:rotation-to-mat2
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotation-x-from-angle!
   #:rotation-x-from-angle
   #:rotation-y-from-angle!
   #:rotation-y-from-angle
   #:rotation-z-from-angle!
   #:rotation-z-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v3!
   #:*v3
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(in-package #:net.mfiano.lisp.origin.mat3)

(deftype mat () '(simple-array u:f32 (9)))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (u:once-only (matrix)
    `(symbol-macrolet
         ((,prefix ,matrix)
          (,(com:make-accessor-symbol prefix "00") (aref ,matrix 0))
          (,(com:make-accessor-symbol prefix "10") (aref ,matrix 1))
          (,(com:make-accessor-symbol prefix "20") (aref ,matrix 2))
          (,(com:make-accessor-symbol prefix "01") (aref ,matrix 3))
          (,(com:make-accessor-symbol prefix "11") (aref ,matrix 4))
          (,(com:make-accessor-symbol prefix "21") (aref ,matrix 5))
          (,(com:make-accessor-symbol prefix "02") (aref ,matrix 6))
          (,(com:make-accessor-symbol prefix "12") (aref ,matrix 7))
          (,(com:make-accessor-symbol prefix "22") (aref ,matrix 8)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f, ~,6f~% ~,6f, ~,6f, ~,6f~% ~,6f, ~,6f, ~,6f]"
            m00 m01 m02 m10 m11 m12 m20 m21 m22)))

;;; constructors

(u:fn-> %mat (u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32) mat)
(declaim (inline %mat))
(u:eval-always
  (defun %mat (m00 m10 m20 m01 m11 m21 m02 m12 m22)
    (declare (optimize speed))
    (let ((mat (make-array 9 :element-type 'u:f32)))
      (setf (aref mat 0) m00
            (aref mat 1) m10
            (aref mat 2) m20
            (aref mat 3) m01
            (aref mat 4) m11
            (aref mat 5) m21
            (aref mat 6) m02
            (aref mat 7) m12
            (aref mat 8) m22)
      mat)))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0.0 0.0 0.0
        0.0 0.0 0.0
        0.0 0.0 0.0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1.0) 0.0 0.0
        0.0 (float x 1.0) 0.0
        0.0 0.0 (float x 1.0)))

(ss:defspecialization (mat :inline t) ((mat m2:mat)) mat
  (%mat (aref mat 0) (aref mat 1) 0.0
        (aref mat 2) (aref mat 3) 0.0
        0.0 0.0 1.0))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (%mat (aref mat 0) (aref mat 1) (aref mat 2)
        (aref mat 3) (aref mat 4) (aref mat 5)
        (aref mat 6) (aref mat 7) (aref mat 8)))

(ss:defspecialization (mat :inline t) ((mat (simple-array u:f32 (16)))) mat
  (%mat (aref mat 0) (aref mat 4) (aref mat 8)
        (aref mat 1) (aref mat 5) (aref mat 9)
        (aref mat 2) (aref mat 6) (aref mat 10)))

(ss:defspecialization (mat :inline t) ((vec1 v3:vec) (vec2 v3:vec) (vec3 v3:vec))
    mat
  (%mat (aref vec1 0) (aref vec1 1) (aref vec1 2)
        (aref vec2 0) (aref vec2 1) (aref vec2 2)
        (aref vec3 0) (aref vec3 1) (aref vec3 2)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real)
                                       (d real) (e real) (f real)
                                       (g real) (h real) (i real))
    mat
  (%mat (float a 1.0) (float b 1.0) (float c 1.0)
        (float d 1.0) (float e 1.0) (float f 1.0)
        (float g 1.0) (float h 1.0) (float i 1.0)))

(ss:defspecialization (mat :inline t) ((mat (simple-array u:f64 (9)))) mat
  (%mat (float (aref mat 0) 1.0)
        (float (aref mat 1) 1.0)
        (float (aref mat 2) 1.0)
        (float (aref mat 3) 1.0)
        (float (aref mat 4) 1.0)
        (float (aref mat 5) 1.0)
        (float (aref mat 6) 1.0)
        (float (aref mat 7) 1.0)
        (float (aref mat 8) 1.0)))

;;; constants

(u:define-constant +zero+ (%mat 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  :test #'equalp)

(u:define-constant +id+ (%mat 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0) :test #'equalp)
