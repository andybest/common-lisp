(in-package #:net.mfiano.lisp.origin.dmat4)

(deftype mat () '(simple-array u:f64 (16)))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (u:once-only (matrix)
    `(symbol-macrolet
         ((,prefix ,matrix)
          (,(int:make-accessor-symbol prefix "00") (aref ,matrix 0))
          (,(int:make-accessor-symbol prefix "10") (aref ,matrix 1))
          (,(int:make-accessor-symbol prefix "20") (aref ,matrix 2))
          (,(int:make-accessor-symbol prefix "30") (aref ,matrix 3))
          (,(int:make-accessor-symbol prefix "01") (aref ,matrix 4))
          (,(int:make-accessor-symbol prefix "11") (aref ,matrix 5))
          (,(int:make-accessor-symbol prefix "21") (aref ,matrix 6))
          (,(int:make-accessor-symbol prefix "31") (aref ,matrix 7))
          (,(int:make-accessor-symbol prefix "02") (aref ,matrix 8))
          (,(int:make-accessor-symbol prefix "12") (aref ,matrix 9))
          (,(int:make-accessor-symbol prefix "22") (aref ,matrix 10))
          (,(int:make-accessor-symbol prefix "32") (aref ,matrix 11))
          (,(int:make-accessor-symbol prefix "03") (aref ,matrix 12))
          (,(int:make-accessor-symbol prefix "13") (aref ,matrix 13))
          (,(int:make-accessor-symbol prefix "23") (aref ,matrix 14))
          (,(int:make-accessor-symbol prefix "33") (aref ,matrix 15)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22
                           m23 m30 m31 m32 m33)
                          &rest rest)
                         &body body)
  (let ((%m00 (int:make-accessor-symbol prefix "00"))
        (%m10 (int:make-accessor-symbol prefix "10"))
        (%m20 (int:make-accessor-symbol prefix "20"))
        (%m30 (int:make-accessor-symbol prefix "30"))
        (%m01 (int:make-accessor-symbol prefix "01"))
        (%m11 (int:make-accessor-symbol prefix "11"))
        (%m21 (int:make-accessor-symbol prefix "21"))
        (%m31 (int:make-accessor-symbol prefix "31"))
        (%m02 (int:make-accessor-symbol prefix "02"))
        (%m12 (int:make-accessor-symbol prefix "12"))
        (%m22 (int:make-accessor-symbol prefix "22"))
        (%m32 (int:make-accessor-symbol prefix "32"))
        (%m03 (int:make-accessor-symbol prefix "03"))
        (%m13 (int:make-accessor-symbol prefix "13"))
        (%m23 (int:make-accessor-symbol prefix "23"))
        (%m33 (int:make-accessor-symbol prefix "33")))
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
