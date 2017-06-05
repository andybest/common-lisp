(in-package :gamebox-math)

(defconstant +epsilon+ 1e-7)

(eval-when (:compile-toplevel :load-toplevel :execute)

  #+sbcl(setf sb-ext:*inline-expansion-limit* 1024))

(defmacro defun* (fname arglist (&key result inline abbrev) &body body)
  (flet ((make-def (fname)
           (append
            (when inline `((declaim (inline ,fname))))
            `((defstar:defun* ,fname ,arglist (:returns ,result) ,@body)))))
    (append
     '(progn)
     (make-def fname)
     (when abbrev (make-def abbrev)))))

(defmacro defdoc ((object type) &body body)
  (let ((type (if (eq type 'macro) 'function type)))
    `(setf (documentation ',object ',type)
           (format nil ,@body))))

(defun* ~ ((a real) (b real) (tolerance single-float)) (:result boolean)
  (< (abs (- a b)) tolerance))
