(in-package #:origin.common)

#+sbcl (setf sb-ext:*inline-expansion-limit* (max sb-ext:*inline-expansion-limit* 2048))

(defun make-accessor-symbol (prefix &rest args)
  (u:format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (cons prefix args)))

(defmacro = (x y rel abs)
  (u:once-only (x y)
    `(< (abs (- ,x ,y)) (max ,abs (* ,rel (max (abs ,x) (abs ,y)))))))

(defmacro cwset (count out subst &body body)
  `(psetf
    ,@(loop :for i :below count
            :append `((aref ,out ,i)
                      ,@(u:tree-leaves
                         body
                         (lambda (x) (and (symbolp x) (member x (u:ensure-list subst))))
                         (lambda (x) `(aref ,x ,i)))))))

(defmacro cwcmp (count subst &body body)
  `(and
    ,@(loop :for i :below count
            :append `(,@(u:tree-leaves
                         body
                         (lambda (x) (and (symbolp x) (member x (u:ensure-list subst))))
                         (lambda (x) `(aref ,x ,i)))))))

(defmacro cwcmp-or (count subst &body body)
  `(or
    ,@(loop :for i :below count
            :append `(,@(u:tree-leaves
                         body
                         (lambda (x) (and (symbolp x) (member x (u:ensure-list subst))))
                         (lambda (x) `(aref ,x ,i)))))))
