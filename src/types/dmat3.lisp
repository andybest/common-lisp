(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.origin.dmat3
  (:local-nicknames
   (#:com #:net.mfiano.lisp.origin.common)
   (#:dm2 #:net.mfiano.lisp.origin.dmat2)
   (#:dv2 #:net.mfiano.lisp.origin.dvec2)
   (#:dv3 #:net.mfiano.lisp.origin.dvec3)
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:ss #:specialization-store)
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

(in-package #:net.mfiano.lisp.origin.dmat3)

(deftype mat () '(simple-array u:f64 (9)))

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

(defmacro with-elements (((prefix m00 m01 m02 m10 m11 m12 m20 m21 m22)
                          &rest rest)
                         &body body)
  (let ((%m00 (com:make-accessor-symbol prefix "00"))
        (%m10 (com:make-accessor-symbol prefix "10"))
        (%m20 (com:make-accessor-symbol prefix "20"))
        (%m01 (com:make-accessor-symbol prefix "01"))
        (%m11 (com:make-accessor-symbol prefix "11"))
        (%m21 (com:make-accessor-symbol prefix "21"))
        (%m02 (com:make-accessor-symbol prefix "02"))
        (%m12 (com:make-accessor-symbol prefix "12"))
        (%m22 (com:make-accessor-symbol prefix "22")))
    `(let ((,%m00 ,m00) (,%m10 ,m10) (,%m20 ,m20)
           (,%m01 ,m01) (,%m11 ,m11) (,%m21 ,m21)
           (,%m02 ,m02) (,%m12 ,m12) (,%m22 ,m22))
       (declare (ignorable ,%m00 ,%m10 ,%m20 ,%m01 ,%m11 ,%m21 ,%m02 ,%m12
                           ,%m22))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f, ~,6f~% ~,6f, ~,6f, ~,6f~% ~,6f, ~,6f, ~,6f]"
            m00 m01 m02 m10 m11 m12 m20 m21 m22)))
