(in-package :box.math.base)

(defconstant +epsilon+ 1e-7
  "The smallest positive quantity that is possible for a scalar.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl(setf sb-ext:*inline-expansion-limit* 1024)

  (declaim (inline %make-accessor-symbol))
  (defun %make-accessor-symbol (prefix &rest args)
    (intern (format nil "~@:(~{~a~}~)" (cons prefix args)) (symbol-package prefix))))

(declaim (inline %~))
(declaim (ftype (function (single-float single-float single-float) boolean) %~))
(defun %~ (a b tolerance)
  (< (abs (- a b)) tolerance))
