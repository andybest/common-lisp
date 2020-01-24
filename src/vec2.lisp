(in-package #:origin.vec2)

(deftype vec () '(simple-array single-float (2)))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (a:once-only (vec)
    `(symbol-macrolet ((,prefix ,vec)
                       (,(make-accessor-symbol prefix "X") (aref ,vec 0))
                       (,(make-accessor-symbol prefix "Y") (aref ,vec 1)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix x y) &rest rest) &body body)
  (let ((%x (make-accessor-symbol prefix "X"))
        (%y (make-accessor-symbol prefix "Y")))
    `(let ((,%x ,x) (,%y ,y))
       (declare (ignorable ,%x ,%y))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))
