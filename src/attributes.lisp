(in-package :shadow)

(defun store-attributes (program stage)
  (when (eq (stage-type stage) :vertex)
    (loop :for attr :in (varjo:input-variables stage)
          :for id = (ensure-keyword (varjo:name attr))
          :for type = (varjo:v-type-of attr)
          :do (setf (au:href (attributes program) id)
                    (make-stage-variable :name (varjo:glsl-name attr)
                                         :type (varjo:type->type-spec type))))))

(defun store-attribute-locations (program)
  (let ((id (id program)))
    (gl:use-program id)
    (au:maphash-values
     (lambda (x)
       (setf (stage-variable-location x) (gl:get-attrib-location id (stage-variable-name x))))
     (attributes program))
    (gl:use-program 0)))
