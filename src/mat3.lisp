(in-package #:net.mfiano.lisp.origin.mat3)

(deftype mat () '(simple-array u:f32 (9)))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (u:once-only (matrix)
    `(symbol-macrolet
         ((,prefix ,matrix)
          (,(int:make-accessor-symbol prefix "00") (aref ,matrix 0))
          (,(int:make-accessor-symbol prefix "10") (aref ,matrix 1))
          (,(int:make-accessor-symbol prefix "20") (aref ,matrix 2))
          (,(int:make-accessor-symbol prefix "01") (aref ,matrix 3))
          (,(int:make-accessor-symbol prefix "11") (aref ,matrix 4))
          (,(int:make-accessor-symbol prefix "21") (aref ,matrix 5))
          (,(int:make-accessor-symbol prefix "02") (aref ,matrix 6))
          (,(int:make-accessor-symbol prefix "12") (aref ,matrix 7))
          (,(int:make-accessor-symbol prefix "22") (aref ,matrix 8)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix m00 m01 m02 m10 m11 m12 m20 m21 m22)
                          &rest rest)
                         &body body)
  (let ((%m00 (int:make-accessor-symbol prefix "00"))
        (%m10 (int:make-accessor-symbol prefix "10"))
        (%m20 (int:make-accessor-symbol prefix "20"))
        (%m01 (int:make-accessor-symbol prefix "01"))
        (%m11 (int:make-accessor-symbol prefix "11"))
        (%m21 (int:make-accessor-symbol prefix "21"))
        (%m02 (int:make-accessor-symbol prefix "02"))
        (%m12 (int:make-accessor-symbol prefix "12"))
        (%m22 (int:make-accessor-symbol prefix "22")))
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
