(in-package #:origin.internal)

#+sbcl
(setf sb-ext:*inline-expansion-limit*
      (max sb-ext:*inline-expansion-limit* 2048))

(declaim (inline make-accessor-symbol))
(defun make-accessor-symbol (prefix &rest args)
  (a:format-symbol (symbol-package prefix) "~@:(~{~a~}~)"
                   (cons prefix args)))

(defun split-arg-spec (arg-spec)
  (let ((rest (position '&rest arg-spec))
        (key (position '&key arg-spec)))
    (values
     (subseq arg-spec 0 (or rest key))
     (when rest
       (subseq arg-spec (1+ rest) key))
     (when key
       (subseq arg-spec (1+ key))))))

(defun generate-function-args (required rest keywords)
  `(,@(mapcar #'first required)
    ,@(when rest
        `(&rest))
    ,@(mapcar #'first rest)
    ,@(when keywords
        `(&key))
    ,@(mapcar
       (lambda (x)
         (destructuring-bind (arg type &optional default) x
           (declare (ignore type))
           (if default
               (list arg default)
               arg)))
       keywords)))

(defun generate-type-signature (required rest keywords)
  `(,@(mapcar #'second required)
    ,@(when rest
        `(&rest))
    ,@(mapcar #'second rest)
    ,@(when keywords
        `(&key))
    ,@(mapcar
       (lambda (x)
         (destructuring-bind (arg type &optional default) x
           (declare (ignore default))
           (list (a:make-keyword arg) type)))
       keywords)))

(defmacro define-op (op arg-spec (&key out (inline t) (speed t))
                      &body body)
  (u:mvlet* ((required rest keywords (split-arg-spec arg-spec))
             (args (generate-function-args required rest keywords))
             (types (generate-type-signature required rest keywords))
             (body decls doc (a:parse-body body :documentation t)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(when inline
           `((declaim (inline ,op))))
       (declaim (ftype (function ,types ,out) ,op))
       (defun ,op ,args
         ,@(when speed
             '((declare (optimize speed))))
         ,@decls
         ,@(when doc
             `(,doc))
         ,@body))))
