(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.mat4
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:m2 #:net.mfiano.lisp.origin.mat2)
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:ss #:specialization-store)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4))
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
   #:rotation-to-mat3!
   #:rotation-to-mat3
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec3!
   #:rotation-axis-to-vec3
   #:rotation-axis-from-vec3!
   #:rotation-axis-from-vec3
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
   #:set-view!
   #:set-view
   #:set-projection/orthographic!
   #:set-projection/orthographic
   #:set-projection/perspective!
   #:set-projection/perspective))

(in-package #:net.mfiano.lisp.origin.mat4)

(deftype mat () '(simple-array u:f32 (16)))

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

(defmacro with-elements (((prefix m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22
                           m23 m30 m31 m32 m33)
                          &rest rest)
                         &body body)
  (let ((%m00 (com:make-accessor-symbol prefix "00"))
        (%m10 (com:make-accessor-symbol prefix "10"))
        (%m20 (com:make-accessor-symbol prefix "20"))
        (%m30 (com:make-accessor-symbol prefix "30"))
        (%m01 (com:make-accessor-symbol prefix "01"))
        (%m11 (com:make-accessor-symbol prefix "11"))
        (%m21 (com:make-accessor-symbol prefix "21"))
        (%m31 (com:make-accessor-symbol prefix "31"))
        (%m02 (com:make-accessor-symbol prefix "02"))
        (%m12 (com:make-accessor-symbol prefix "12"))
        (%m22 (com:make-accessor-symbol prefix "22"))
        (%m32 (com:make-accessor-symbol prefix "32"))
        (%m03 (com:make-accessor-symbol prefix "03"))
        (%m13 (com:make-accessor-symbol prefix "13"))
        (%m23 (com:make-accessor-symbol prefix "23"))
        (%m33 (com:make-accessor-symbol prefix "33")))
    `(let ((,%m00 ,m00) (,%m10 ,m10) (,%m20 ,m20) (,%m30 ,m30)
           (,%m01 ,m01) (,%m11 ,m11) (,%m21 ,m21) (,%m31 ,m31)
           (,%m02 ,m02) (,%m12 ,m12) (,%m22 ,m22) (,%m32 ,m32)
           (,%m03 ,m03) (,%m13 ,m13) (,%m23 ,m23) (,%m33 ,m33))
       (declare (ignorable ,%m00 ,%m10 ,%m20 ,%m30 ,%m01 ,%m11 ,%m21 ,%m31
                           ,%m02 ,%m12 ,%m22 ,%m32 ,%m03 ,%m13 ,%m23 ,%m33))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f]"
            m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)))
