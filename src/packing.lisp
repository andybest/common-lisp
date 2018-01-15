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
    (varjo:v-double '(:float 64))
    (varjo:v-struct (varjo:type->type-spec type))
    (varjo:v-container (list* (pack-container type)
                              (pack-type (varjo:v-element-type type))
                              (varjo:v-dimensions type)))))

(defun pack-struct (struct)
  (loop :with name = (varjo:type->type-spec struct)
        :for (slot-name slot-type) :in (varjo.internals:v-slots struct)
        :for type = (pack-type slot-type)
        :collect (list slot-name type) :into members
        :finally (return `(,name (:struct () ,@members)))))

(defun pack-block-layout (block)
  (if (member :std-430 (varjo:qualifiers block))
      :std430
      :std140))

(defun pack-block (block)
  (loop :with name = (varjo:name block)
        :with struct = (varjo:v-type-of block)
        :for (slot-name slot-type) :in (varjo.internals:v-slots struct)
        :for type = (pack-type slot-type)
        :for layout = (pack-block-layout block)
        :collect (list slot-name type) :into members
        :finally (return `(,name (:block (:packing ,layout) ,@members)))))

(defun pack-all (structs blocks)
  (glsl-packing:pack-structs
   `(,@(mapcar #'pack-struct structs)
     ,@(mapcar #'pack-block blocks))))
