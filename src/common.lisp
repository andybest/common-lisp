(in-package :box.math.base)

(defconstant +epsilon+ 1e-7)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl(setf sb-ext:*inline-expansion-limit* 1024)

  (declaim (inline %make-accessor-symbol))
  (defun %make-accessor-symbol (prefix &rest args)
    (intern (format nil "~@:(~{~a~}~)" (cons prefix args)) (symbol-package prefix))))

(declaim (inline %~))
(defun* (%~ -> boolean) ((a real) (b real) (tolerance single-float))
  (< (abs (- a b)) tolerance))
