(in-package #:cl-user)

(defpackage #:origin.dmat4
  (:local-nicknames
   (#:com #:origin.common)
   (#:dm2 #:origin.dmat2)
   (#:dm3 #:origin.dmat3)
   (#:dv3 #:origin.dvec3)
   (#:dv4 #:origin.dvec4)
   (#:m4 #:origin.mat4)
   (#:ss #:specialization-store)
   (#:u #:golden-utils))
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
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-to-mat3!
   #:rotation-to-mat3
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec3!
   #:rotation-axis-to-vec3
   #:rotation-axis-from-vec3!
   #:rotation-axis-from-vec3
   #:rotation-x-from-angle!
   #:rotation-x-from-angle
   #:rotation-y-from-angle!
   #:rotation-y-from-angle!
   #:rotation-z-from-angle
   #:rotation-z-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v4!
   #:*v4
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:orthonormalize!
   #:orthonormalize
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal
   #:determinant
   #:invert-orthogonal!
   #:invert-orthogonal
   #:invert!
   #:invert
   #:look-at!
   #:look-at
   #:ortho!
   #:ortho
   #:perspective!
   #:perspective))

(in-package #:origin.dmat4)

(deftype mat () '(u:f64a 16))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (u:once-only (matrix)
    `(symbol-macrolet
         ((,prefix ,matrix)
          (,(com:make-accessor-symbol prefix "00") (aref ,matrix 0))
          (,(com:make-accessor-symbol prefix "10") (aref ,matrix 1))
          (,(com:make-accessor-symbol prefix "20") (aref ,matrix 2))
          (,(com:make-accessor-symbol prefix "30") (aref ,matrix 3))
          (,(com:make-accessor-symbol prefix "01") (aref ,matrix 4))
          (,(com:make-accessor-symbol prefix "11") (aref ,matrix 5))
          (,(com:make-accessor-symbol prefix "21") (aref ,matrix 6))
          (,(com:make-accessor-symbol prefix "31") (aref ,matrix 7))
          (,(com:make-accessor-symbol prefix "02") (aref ,matrix 8))
          (,(com:make-accessor-symbol prefix "12") (aref ,matrix 9))
          (,(com:make-accessor-symbol prefix "22") (aref ,matrix 10))
          (,(com:make-accessor-symbol prefix "32") (aref ,matrix 11))
          (,(com:make-accessor-symbol prefix "03") (aref ,matrix 12))
          (,(com:make-accessor-symbol prefix "13") (aref ,matrix 13))
          (,(com:make-accessor-symbol prefix "23") (aref ,matrix 14))
          (,(com:make-accessor-symbol prefix "33") (aref ,matrix 15)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f]"
            m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)))

;;; Constructors

(u:fn-> %mat (u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64 u:f64
                    u:f64 u:f64)
        mat)
(declaim (inline %mat))
(u:eval-always
  (defun %mat (m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33)
    (declare (optimize speed))
    (let ((mat (make-array 16 :element-type 'u:f64)))
      (setf (aref mat 0) m00
            (aref mat 1) m10
            (aref mat 2) m20
            (aref mat 3) m30
            (aref mat 4) m01
            (aref mat 5) m11
            (aref mat 6) m21
            (aref mat 7) m31
            (aref mat 8) m02
            (aref mat 9) m12
            (aref mat 10) m22
            (aref mat 11) m32
            (aref mat 12) m03
            (aref mat 13) m13
            (aref mat 14) m23
            (aref mat 15) m33)
      mat)))

(ss:defstore mat (&rest args))

(ss:defspecialization (mat :inline t) () mat
  (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0))

(ss:defspecialization (mat :inline t) ((x real)) mat
  (%mat (float x 1d0) 0d0 0d0 0d0
        0d0 (float x 1d0) 0d0 0d0
        0d0 0d0 (float x 1d0) 0d0
        0d0 0d0 0d0 (float x 1d0)))

(ss:defspecialization (mat :inline t) ((mat dm2:mat)) mat
  (%mat (aref mat 0) (aref mat 1) 0d0 0d0
        (aref mat 2) (aref mat 3) 0d0 0d0
        0d0 0d0 1d0 0d0
        0d0 0d0 0d0 1d0))

(ss:defspecialization (mat :inline t) ((mat dm3:mat)) mat
  (%mat (aref mat 0) (aref mat 1) (aref mat 2) 0d0
        (aref mat 3) (aref mat 4) (aref mat 5) 0d0
        (aref mat 6) (aref mat 7) (aref mat 8) 0d0
        0d0 0d0 0d0 1d0))

(ss:defspecialization (mat :inline t) ((mat mat)) mat
  (%mat (aref mat 0) (aref mat 1) (aref mat 2) (aref mat 3)
        (aref mat 4) (aref mat 5) (aref mat 6) (aref mat 7)
        (aref mat 8) (aref mat 9) (aref mat 10) (aref mat 11)
        (aref mat 12) (aref mat 13) (aref mat 14) (aref mat 15)))

(ss:defspecialization (mat :inline t)
    ((vec1 dv4:vec) (vec2 dv4:vec) (vec3 dv4:vec) (vec4 dv4:vec))
    mat
  (%mat (aref vec1 0) (aref vec1 1) (aref vec1 2) (aref vec1 3)
        (aref vec2 0) (aref vec2 1) (aref vec2 2) (aref vec2 3)
        (aref vec3 0) (aref vec3 1) (aref vec3 2) (aref vec3 3)
        (aref vec4 0) (aref vec4 1) (aref vec4 2) (aref vec4 3)))

(ss:defspecialization (mat :inline t) ((a real) (b real) (c real) (d real)
                                       (e real) (f real) (g real) (h real)
                                       (i real) (j real) (k real) (l real)
                                       (m real) (n real) (o real) (p real))
    mat
  (%mat (float a 1d0) (float b 1d0) (float c 1d0) (float d 1d0)
        (float e 1d0) (float f 1d0) (float g 1d0) (float h 1d0)
        (float i 1d0) (float j 1d0) (float k 1d0) (float l 1d0)
        (float m 1d0) (float n 1d0) (float o 1d0) (float p 1d0)))

(ss:defspecialization (mat :inline t) ((mat m4:mat)) mat
  (%mat (float (aref mat 0) 1d0)
        (float (aref mat 1) 1d0)
        (float (aref mat 2) 1d0)
        (float (aref mat 3) 1d0)
        (float (aref mat 4) 1d0)
        (float (aref mat 5) 1d0)
        (float (aref mat 6) 1d0)
        (float (aref mat 7) 1d0)
        (float (aref mat 8) 1d0)
        (float (aref mat 9) 1d0)
        (float (aref mat 10) 1d0)
        (float (aref mat 11) 1d0)
        (float (aref mat 12) 1d0)
        (float (aref mat 13) 1d0)
        (float (aref mat 14) 1d0)
        (float (aref mat 15) 1d0)))

;;; Constants

(u:define-constant +zero+
    (%mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
  :test #'equalp)

(u:define-constant +id+
    (%mat 1d0 0d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 0d0 1d0 0d0 0d0 0d0 0d0 1d0)
  :test #'equalp)
