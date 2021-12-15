(in-package #:mfiano.scripts.base)

(define-condition user-error (simple-error) ())

(defun user-error (message &rest args)
  (error 'user-error
         :format-control message
         :format-arguments args))

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
