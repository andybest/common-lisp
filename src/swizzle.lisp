(in-package :box.math.base)

(eval-when (:compile-toplevel :load-toplevel)
  (defun %swizzle/permutations (n items)
    (if (= n 1)
        (mapcar #'list items)
        (mapcan
         (lambda (x)
           (mapcar
            (lambda (y)
              (cons x y))
            (%swizzle/permutations (1- n) items)))
         items)))

  (defun %swizzle/component-groups (size)
    (loop :for masks :in '((x y z w) (r g b a) (s t p q))
          :append
          (loop :with set = (subseq masks 0 size)
                :for i from 1 :to size
                :for items = (%swizzle/permutations i set)
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
             result)))))

(defmacro generate-swizzle-functions (component-count package-name)
  `(progn
     ,@(loop :for components :in (%swizzle/component-groups component-count)
             :for func-name = (intern (format nil ".~a" components)
                                      package-name)
             :append
             `((declaim (inline ,func-name))
               (export ',func-name ,package-name)
               (defun ,func-name (vec)
                 ,(%swizzle/function-body components))))))

(generate-swizzle-functions 2 :box.math.vec2)
(generate-swizzle-functions 3 :box.math.vec3)
(generate-swizzle-functions 4 :box.math.vec4)
