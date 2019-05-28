(in-package #:cl-user)

(defpackage #:box.math.common
  (:use #:cl)
  (:export
   #:+epsilon+
   #:round-up
   #:round-down))

(in-package #:box.math.common)

(defconstant +epsilon+ 1e-7
  "The smallest positive quantity that is possible for a scalar.")

(au:eval-always
  #+sbcl(setf sb-ext:*inline-expansion-limit* 1024)

  (declaim (inline %make-accessor-symbol))
  (defun %make-accessor-symbol (prefix &rest args)
    (au:format-symbol (symbol-package prefix) "~@:(~{~a~}~)"
                      (cons prefix args))))

(declaim (inline %~))
(declaim (ftype (function (single-float single-float single-float) boolean) %~))
(defun %~ (a b tolerance)
  (< (abs (- a b)) tolerance))

(defun round-up (x)
  (floor (+ x 1/2)))

(defun round-down (x)
  (ceiling (- x 1/2)))
