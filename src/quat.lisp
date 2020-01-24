(in-package #:origin.quat)

(deftype quat () '(simple-array single-float (4)))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  (a:once-only (quat)
    `(symbol-macrolet ((,prefix ,quat)
                       (,(make-accessor-symbol prefix "W") (aref ,quat 0))
                       (,(make-accessor-symbol prefix "X") (aref ,quat 1))
                       (,(make-accessor-symbol prefix "Y") (aref ,quat 2))
                       (,(make-accessor-symbol prefix "Z") (aref ,quat 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix w x y z) &rest rest) &body body)
  (let ((%w (make-accessor-symbol prefix "W"))
        (%x (make-accessor-symbol prefix "X"))
        (%y (make-accessor-symbol prefix "Y"))
        (%z (make-accessor-symbol prefix "Z")))
    `(let ((,%w ,w) (,%x ,x) (,%y ,y) (,%z ,z))
       (declare (ignorable ,%w ,%x ,%y ,%z))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))
