(in-package #:origin.vec4)

(deftype vec () '(simple-array single-float (4)))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (a:once-only (vec)
    `(symbol-macrolet ((,prefix ,vec)
                       (,(make-accessor-symbol prefix "X") (aref ,vec 0))
                       (,(make-accessor-symbol prefix "Y") (aref ,vec 1))
                       (,(make-accessor-symbol prefix "Z") (aref ,vec 2))
                       (,(make-accessor-symbol prefix "W") (aref ,vec 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix x y z w) &rest rest) &body body)
  (let ((%x (make-accessor-symbol prefix "X"))
        (%y (make-accessor-symbol prefix "Y"))
        (%z (make-accessor-symbol prefix "Z"))
        (%w (make-accessor-symbol prefix "W")))
    `(let ((,%x ,x) (,%y ,y) (,%z ,z) (,%w ,w))
       (declare (ignorable ,%x ,%y ,%z ,%w))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))
