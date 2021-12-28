(in-package #:mfiano.ffi.freebsd)

(defmacro with-error-check ((func error-value) &body body)
  (u:with-gensyms (result)
    `(let ((,result (progn ,@body)))
       (if (,func ,result ,error-value)
           (error 'freebsd-error)
           ,result))))
