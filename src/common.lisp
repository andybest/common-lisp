(in-package #:box.math.common)

(deftype int32 () '(signed-byte 32))

(defconstant +epsilon+ 1e-7
  "The smallest positive quantity that is possible for a scalar.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl(setf sb-ext:*inline-expansion-limit* 1024)

  (declaim (inline %make-accessor-symbol))
  (defun %make-accessor-symbol (prefix &rest args)
    (intern (format nil "~@:(~{~a~}~)" (cons prefix args))
            (symbol-package prefix)))

  (defun %swizzle/combinations (n items)
    (if (= n 1)
        (mapcar #'list items)
        (mapcan
         (lambda (x)
           (mapcar
            (lambda (y)
              (cons x y))
            (%swizzle/combinations (1- n) items)))
         items)))

  (defun %swizzle/component-groups (size)
    (loop :for masks :in '((x y z w) (r g b a) (s t p q))
          :append
          (loop :with set = (subseq masks 0 size)
                :for i from 1 :to size
                :for items = (%swizzle/combinations i set)
                :append (mapcar (lambda (x) (format nil "~{~a~}" x)) items))))

  (defun %swizzle/char-position (components index)
    (let ((char (char components index)))
      (or (position char "XYZW")
          (position char "RGBA")
          (position char "STPQ"))))

  (defun %swizzle/function-body (components)
    (let ((size (length components)))
      (if (= size 1)
          `(aref vec ,(%swizzle/char-position components 0))
          `(let ((result (make-array ,size :element-type 'single-float)))
             ,@(loop :for i :below size
                     :for pos = (%swizzle/char-position components i)
                     :collect `(setf (aref result ,i) (aref vec ,pos)))
             result))))

  (defun %swizzle/generate-docstring (components)
    (let ((size (length components)))
      (if (= size 1)
          (format nil "Swizzle: Get the scalar component ~a from VEC."
                  components)
          (format nil "Swizzle: Create a vec~a from the ~
                       ~{~a~#[~;, and ~:;, ~]~} components of VEC."
                  size (coerce components 'list))))))

(declaim (inline %~))
(declaim (ftype (function (single-float single-float single-float) boolean) %~))
(defun %~ (a b tolerance)
  (< (abs (- a b)) tolerance))

(defmacro %generate-swizzle-functions (size)
  `(progn
     ,@(loop :for components :in (%swizzle/component-groups size)
             :for func-name = (au:format-symbol *package* "~:@(.~a~)"
                                                components)
             :for component-list = (coerce components 'list)
             :append
             `((declaim (inline ,func-name))
               (export ',func-name)
               (setf (documentation ',func-name 'function)
                     (%swizzle/generate-docstring ,components))
               (defun ,func-name (vec)
                 ,(%swizzle/function-body components))))))
