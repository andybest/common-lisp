(in-package #:origin.mat4)

(deftype mat () '(simple-array single-float (16)))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (a:once-only (matrix)
    `(symbol-macrolet ((,prefix ,matrix)
                       (,(make-accessor-symbol prefix "00") (aref ,matrix 0))
                       (,(make-accessor-symbol prefix "01") (aref ,matrix 1))
                       (,(make-accessor-symbol prefix "02") (aref ,matrix 2))
                       (,(make-accessor-symbol prefix "03") (aref ,matrix 3))
                       (,(make-accessor-symbol prefix "10") (aref ,matrix 4))
                       (,(make-accessor-symbol prefix "11") (aref ,matrix 5))
                       (,(make-accessor-symbol prefix "12") (aref ,matrix 6))
                       (,(make-accessor-symbol prefix "13") (aref ,matrix 7))
                       (,(make-accessor-symbol prefix "20") (aref ,matrix 8))
                       (,(make-accessor-symbol prefix "21") (aref ,matrix 9))
                       (,(make-accessor-symbol prefix "22") (aref ,matrix 10))
                       (,(make-accessor-symbol prefix "23") (aref ,matrix 11))
                       (,(make-accessor-symbol prefix "30") (aref ,matrix 12))
                       (,(make-accessor-symbol prefix "31") (aref ,matrix 13))
                       (,(make-accessor-symbol prefix "32") (aref ,matrix 14))
                       (,(make-accessor-symbol prefix "33") (aref ,matrix 15)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22
                           m23 m30 m31 m32 m33)
                          &rest rest)
                         &body body)
  (let ((%m00 (make-accessor-symbol prefix "00"))
        (%m01 (make-accessor-symbol prefix "01"))
        (%m02 (make-accessor-symbol prefix "02"))
        (%m03 (make-accessor-symbol prefix "03"))
        (%m10 (make-accessor-symbol prefix "10"))
        (%m11 (make-accessor-symbol prefix "11"))
        (%m12 (make-accessor-symbol prefix "12"))
        (%m13 (make-accessor-symbol prefix "13"))
        (%m20 (make-accessor-symbol prefix "20"))
        (%m21 (make-accessor-symbol prefix "21"))
        (%m22 (make-accessor-symbol prefix "22"))
        (%m23 (make-accessor-symbol prefix "23"))
        (%m30 (make-accessor-symbol prefix "30"))
        (%m31 (make-accessor-symbol prefix "31"))
        (%m32 (make-accessor-symbol prefix "32"))
        (%m33 (make-accessor-symbol prefix "33")))
    `(let ((,%m00 ,m00) (,%m01 ,m01) (,%m02 ,m02) (,%m03 ,m03)
           (,%m10 ,m10) (,%m11 ,m11) (,%m12 ,m12) (,%m13 ,m13)
           (,%m20 ,m20) (,%m21 ,m21) (,%m22 ,m22) (,%m23 ,m23)
           (,%m30 ,m30) (,%m31 ,m31) (,%m32 ,m32) (,%m33 ,m33))
       (declare (ignorable ,%m00 ,%m01 ,%m02 ,%m03 ,%m10 ,%m11 ,%m12 ,%m13
                           ,%m20 ,%m21 ,%m22 ,%m23 ,%m30 ,%m31 ,%m32 ,%m33))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f]"
            m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33)))
