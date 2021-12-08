(in-package #:mfiano.scripts.base)

(define-condition user-error (error)
  ((%message :reader user-error-message
             :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~a" (user-error-message condition)))))

(defmacro with-ctrl-c ((&optional cleanup-func) &body body)
  `(handler-case (wua:with-user-abort (progn ,@body))
     (wua:user-abort ()
       (unwind-protect (ui:exit 130)
         ,@(when cleanup-func
             `((funcall ,cleanup-func)))))))

(defun run-non-interactively (run-func &key (cleanup-func (constantly nil)))
  (let ((*interactive* nil))
    (with-ctrl-c (cleanup-func)
      (handler-case (funcall run-func)
        (user-error (e)
          (ui:print-error-and-exit e))))))
