(in-package #:cl-user)

(defpackage #:origin.internal
  (:use #:cl)
  (:export
   #:make-accessor-symbol
   #:define-op))

(in-package #:origin.internal)

(setf sb-ext:*inline-expansion-limit*
      (max sb-ext:*inline-expansion-limit* 2048))

(declaim (inline make-accessor-symbol))
(defun make-accessor-symbol (prefix &rest args)
  (au:format-symbol (symbol-package prefix) "~@:(~{~a~}~)"
                    (cons prefix args)))

(defun split-arg-spec (arg-spec)
  (let ((key (position '&key arg-spec)))
    (values
     (subseq arg-spec 0 key)
     (when key
       (subseq arg-spec (1+ key))))))

(defun generate-function-args (required keywords)
  `(,@(mapcar #'first required)
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

(defun generate-type-signature (required keywords)
  `(,@(mapcar #'second required)
    ,@(when keywords
        `(&key))
    ,@(mapcar
       (lambda (x)
         (destructuring-bind (arg type &optional default) x
           (declare (ignore default))
           (list (au:make-keyword arg) type)))
       keywords)))

(defmacro define-op (op arg-spec (&key out (inline t)) &body body)
  (au:mvlet* ((required keywords (split-arg-spec arg-spec))
              (args (generate-function-args required keywords))
              (types (generate-type-signature required keywords))
              (body decls doc (au:parse-body body :documentation t)))
    (declare (ignore decls))
    `(progn
       ,@(when inline
           `((declaim (inline ,op))))
       (declaim (ftype (function ,types ,out) ,op))
       (defun ,op ,args
         ,@(when doc
             `(,doc))
         ,@body))))
