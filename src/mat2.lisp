(in-package #:origin.mat2)

(deftype mat () '(simple-array single-float (4)))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (a:once-only (matrix)
    `(symbol-macrolet ((,prefix ,matrix)
                       (,(make-accessor-symbol prefix "00") (aref ,matrix 0))
                       (,(make-accessor-symbol prefix "10") (aref ,matrix 1))
                       (,(make-accessor-symbol prefix "01") (aref ,matrix 2))
                       (,(make-accessor-symbol prefix "11") (aref ,matrix 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix m00 m01 m10 m11) &rest rest) &body body)
  (let ((%m00 (make-accessor-symbol prefix "00"))
        (%m10 (make-accessor-symbol prefix "10"))
        (%m01 (make-accessor-symbol prefix "01"))
        (%m11 (make-accessor-symbol prefix "11")))
    `(let ((,%m00 ,m00) (,%m10 ,m10) (,%m01 ,m01) (,%m11 ,m11))
       (declare (ignorable ,%m00 ,%m10 ,%m01 ,%m11))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f~% ~,6f, ~,6f]" m00 m01 m10 m11)))
