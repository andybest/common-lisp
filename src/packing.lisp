(in-package :shadow)

(defun pack-container (type)
  (etypecase type
    (varjo:v-vector :vec)
    (varjo:v-matrix (error "Shader blocks containing matrices are not currently supported."))
    (varjo:v-array :array)))

(defun pack-type (type)
  (etypecase type
    (varjo:v-bool '(:bool))
    (varjo:v-int '(:int 32))
    (varjo:v-uint '(:uint 32))
    (varjo:v-float '(:float 32))
    (varjo:v-struct (varjo:type->type-spec type))
    (varjo:v-container
     (let ((element-type (varjo:v-element-type type)))
       (cond
         ((or (typep element-type 'varjo:v-user-struct)
              (typep element-type 'varjo:v-array))
          (error "Shader blocks containing arrays of aggregates are not currently supported."))
         (t (list* (pack-container type)
                   (pack-type element-type)
                   (varjo:v-dimensions type))))))))

(defun pack-struct (struct)
  (loop :with name = (varjo:type->type-spec struct)
        :for (slot-name slot-type) :in (varjo.internals:v-slots struct)
        :for type = (pack-type slot-type)
        :collect (list slot-name type) :into members
        :finally (return `(,name (:struct () ,@members)))))

(defun pack-block (layout)
  (loop :with uniform = (uniform layout)
        :with name = (varjo:name uniform)
        :with layout-type = (layout-type layout)
        :for (slot-name slot-type) :in (varjo.internals:v-slots (varjo:v-type-of uniform))
        :for packed-type = (pack-type slot-type)
        :collect (list slot-name packed-type) :into members
        :finally (return `(,name (:block (:packing ,layout-type) ,@members)))))

(defun pack-layout (layout)
  (let ((structs (collect-layout-structs layout)))
    (glsl-packing:pack-structs
     `(,@(mapcar #'pack-struct structs)
       ,(pack-block layout)))))

(defun unpack-type (layout-type type)
  (flet ((add-stride (&rest args)
           (let ((count (or (getf args :count) 1)))
             (setf (getf args :element-stride)
                   (ecase layout-type
                     (:std140 4)
                     (:std430 (if (= count 3) 4 count))))
             args)))
    (destructuring-bind ((spec &optional x y) &key &allow-other-keys) type
      (ecase spec
        ((:bool :uint) (add-stride :type '(unsigned-byte 32)))
        (:int (add-stride :type '(signed-byte 32)))
        (:float (add-stride :type 'single-float))
        (:vec (apply #'add-stride :count y (unpack-type layout-type (list x))))
        (:array (unpack-type layout-type (list x)))))))
