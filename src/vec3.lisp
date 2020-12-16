(in-package #:net.mfiano.lisp.origin.vec3)

(deftype vec () '(simple-array u:f32 (3)))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(int:make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(int:make-accessor-symbol prefix "Y") (aref ,vec 1))
          (,(int:make-accessor-symbol prefix "Z") (aref ,vec 2)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defmacro with-elements (((prefix x y z) &rest rest) &body body)
  (let ((%x (int:make-accessor-symbol prefix "X"))
        (%y (int:make-accessor-symbol prefix "Y"))
        (%z (int:make-accessor-symbol prefix "Z")))
    `(let ((,%x ,x) (,%y ,y) (,%z ,z))
       (declare (ignorable ,%x ,%y ,%z))
       ,(if rest
            `(with-elements ,rest ,@body)
            `(progn ,@body)))))
