(in-package #:cl-freebsd)

(defmacro with-error-check ((func error-value) &body body)
  (u:with-gensyms (result)
    `(let ((,result (progn ,@body)))
       (if (,func ,result ,error-value)
           (error 'freebsd-error)
           ,result))))
