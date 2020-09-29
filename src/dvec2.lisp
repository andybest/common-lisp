(in-package #:net.mfiano.lisp.origin.dvec2)

(deftype vec () '(simple-array double-float (2)))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(int:make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(int:make-accessor-symbol prefix "Y") (aref ,vec 1)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix x y) &rest rest) &body body)
  (let ((%x (int:make-accessor-symbol prefix "X"))
        (%y (int:make-accessor-symbol prefix "Y")))
    `(let ((,%x ,x) (,%y ,y))
       (declare (ignorable ,%x ,%y))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))
