(in-package :shadow)

(defun pack-container (type)
  (etypecase type
    (varjo:v-vector :vec)
    (varjo:v-matrix :mat)
    (varjo:v-array :array)))

(defun pack-type (type)
  (etypecase type
    (varjo:v-bool '(:bool))
    (varjo:v-int '(:int 32))
    (varjo:v-uint '(:uint 32))
    (varjo:v-float '(:float 32))
    (varjo:v-struct (varjo:type->type-spec type))
    (varjo:v-container
     (let ((element-type (varjo:v-element-type type))
           (dimensions (varjo:v-dimensions type)))
       (cond
         ((or (typep element-type 'varjo:v-user-struct)
              (typep element-type 'varjo:v-array))
          (error "Arrays of aggregates are not currently supported."))
         ((and (cadr dimensions)
               (not (= (cadr dimensions) 4)))
          (error "Only 4x4 matrices are currently supported."))
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

(defun unpack-type (type)
  (destructuring-bind ((x &optional y z) &key &allow-other-keys) type
    (cond
      ((or (and (eq x :bool) (null y))
           (and (eq x :uint) (eql y 32)))
       (list :type '(unsigned-byte 32)))
      ((and (eq x :int)
            (eql y 32))
       (list :type '(signed-byte 32)))
      ((and (eq x :float)
            (eql y 32))
       (list :type 'single-float))
      ((eq x :vec)
       (append (list :count z) (unpack-type (list y))))
      ((eq x :array)
       (unpack-type (list y))))))
