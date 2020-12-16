(in-package #:net.mfiano.lisp.origin.dquat)

(deftype quat () '(simple-array u:f64 (4)))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  (u:once-only (quat)
    `(symbol-macrolet
         ((,prefix ,quat)
          (,(int:make-accessor-symbol prefix "W") (aref ,quat 0))
          (,(int:make-accessor-symbol prefix "X") (aref ,quat 1))
          (,(int:make-accessor-symbol prefix "Y") (aref ,quat 2))
          (,(int:make-accessor-symbol prefix "Z") (aref ,quat 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix w x y z) &rest rest) &body body)
  (let ((%w (int:make-accessor-symbol prefix "W"))
        (%x (int:make-accessor-symbol prefix "X"))
        (%y (int:make-accessor-symbol prefix "Y"))
        (%z (int:make-accessor-symbol prefix "Z")))
    `(let ((,%w ,w) (,%x ,x) (,%y ,y) (,%z ,z))
       (declare (ignorable ,%w ,%x ,%y ,%z))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))
