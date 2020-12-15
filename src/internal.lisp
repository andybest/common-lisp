(in-package #:net.mfiano.lisp.origin.internal)

#+sbcl
(setf sb-ext:*inline-expansion-limit*
      (max sb-ext:*inline-expansion-limit* 2048))

(defun make-accessor-symbol (prefix &rest args)
  (u:format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (cons prefix args)))

(defmacro = (x y rel abs)
  `(< (abs (- ,x ,y))
      (max ,abs (* ,rel (max (abs ,x) (abs ,y))))))
