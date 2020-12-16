(in-package #:net.mfiano.lisp.origin.dvec4)

(deftype vec () '(simple-array u:f64 (4)))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(int:make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(int:make-accessor-symbol prefix "Y") (aref ,vec 1))
          (,(int:make-accessor-symbol prefix "Z") (aref ,vec 2))
          (,(int:make-accessor-symbol prefix "W") (aref ,vec 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix x y z w) &rest rest) &body body)
  (let ((%x (int:make-accessor-symbol prefix "X"))
        (%y (int:make-accessor-symbol prefix "Y"))
        (%z (int:make-accessor-symbol prefix "Z"))
        (%w (int:make-accessor-symbol prefix "W")))
    `(let ((,%x ,x) (,%y ,y) (,%z ,z) (,%w ,w))
       (declare (ignorable ,%x ,%y ,%z ,%w))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))
