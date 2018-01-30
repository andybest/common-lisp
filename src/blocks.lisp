(in-package :shadow)

(defclass shader-block ()
  ((%name :reader name
          :initarg :name)
   (%type :reader block-type
          :initarg :type)
   (%layout :reader layout
            :initarg :layout)))

(defun get-block-type (struct)
  (if (has-qualifier-p struct :ubo)
      (values :uniform :ubo)
      (when (has-qualifier-p struct :ssbo)
        (values :buffer :ssbo))))

(defun make-block (program layout)
  (let* ((uniform (uniform layout))
         (id (ensure-keyword (varjo:name uniform)))
         (type (varjo:v-type-of uniform)))
    (multiple-value-bind (block-type buffer-type) (get-block-type type)
      (setf (gethash (cons block-type id) (blocks program))
            (make-instance 'shader-block
                           :name (format nil "_~a_~a" buffer-type id)
                           :type block-type
                           :layout layout)))))

(defun store-blocks (program stage)
  (map nil (lambda (layout) (make-block program layout)) (collect-layouts stage)))

(defun bind-uniform-block (program-name block-id binding-point)
  (with-accessors ((id id) (blocks blocks)) (program-by-name program-name)
    (let* ((block (gethash (cons :uniform block-id) blocks))
           (index (%gl:get-uniform-block-index id (name block))))
      (pushnew block (gethash binding-point (uniform-block-bindings *shader-info*)))
      (%gl:uniform-block-binding id index binding-point))))

(defun bind-shader-storage-block (program-name block-id binding-point)
  (with-accessors ((id id) (blocks blocks)) (program-by-name program-name)
    (let* ((block (gethash (cons :buffer block-id) blocks))
           (index (gl:get-program-resource-index id :shader-storage-block (name block))))
      (pushnew block (gethash binding-point (buffer-block-bindings *shader-info*)))
      (%gl:shader-storage-block-binding id index binding-point))))
