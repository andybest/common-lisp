(in-package :gamebox-math)

(defconstant +epsilon+ 1e-7)

(eval-when (:compile-toplevel :load-toplevel :execute)

  #+sbcl(setf sb-ext:*inline-expansion-limit* 1024)

  (defun parse-args (args/types)
    (let ((args)
          (types)
          (seen-keyword))
      (dolist (x args/types)
        (if (consp x)
            (progn
              (push (car x) args)
              (if (consp (car x))
                  (if seen-keyword
                      (push (list (make-keyword (caar x)) (cadr x)) types)
                      (push (cadr x) types))
                  (push (cadr x) types)))
            (progn
              (if (eq x '&key)
                  (setf seen-keyword t)
                  (setf seen-keyword nil))
              (push x args)
              (push x types))))
      (values (reverse args) (reverse types))))

  (defun make-accessor-symbol (prefix &rest symbols)
    (intern (format nil "~:@(~{~A~}~)" (cons prefix symbols))
            (symbol-package prefix))))

(defmacro defun* (name args/types (&key result inline abbrev) &body body)
  (multiple-value-bind (args types) (parse-args args/types)
    (flet ((make-def (name)
             (append
              `((declaim (ftype (function ,types ,result) ,name)))
              (when inline `((declaim (inline ,name))))
              `((defun ,name ,args ,@body)))))
      (append
       '(progn)
       (make-def name)
       (when abbrev (make-def abbrev))))))

(defmacro defdoc ((object type) &body body)
  (let ((type (if (eq type 'macro) 'function type)))
    `(setf (documentation ',object ',type)
           (format nil ,@body))))
