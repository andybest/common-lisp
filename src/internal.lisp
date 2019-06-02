(in-package #:cl-user)

(defpackage #:origin.internal
  (:use #:cl))

(in-package #:origin.internal)

(au:eval-always
  #+sbcl(setf sb-ext:*inline-expansion-limit*
              (max sb-ext:*inline-expansion-limit* 1024))

  (declaim (inline make-accessor-symbol))
  (defun make-accessor-symbol (prefix &rest args)
    (au:format-symbol (symbol-package prefix) "~@:(~{~a~}~)"
                      (cons prefix args))))

(declaim (inline ~))
(declaim (ftype (function (single-float single-float single-float) boolean) ~))
(defun ~ (a b tolerance)
  (< (abs (- a b)) tolerance))
