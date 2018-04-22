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

(defgeneric get-bound-blocks (block-or-buffer-type binding-point)
  (:method ((block-or-buffer-type (eql :uniform)) binding-point)
    (gethash binding-point (uniform-block-bindings *shader-info*)))
  (:method ((block-or-buffer-type (eql :ubo)) binding-point)
    (gethash binding-point (uniform-block-bindings *shader-info*)))
  (:method ((block-or-buffer-type (eql :buffer)) binding-point)
    (gethash binding-point (buffer-block-bindings *shader-info*)))
  (:method ((block-or-buffer-type (eql :ssbo)) binding-point)
    (gethash binding-point (buffer-block-bindings *shader-info*))))

(defun make-block (program layout)
  (let* ((uniform (uniform layout))
         (id (ensure-keyword (varjo:name uniform)))
         (name (varjo.internals:safe-glsl-name-string id))
         (type (varjo:v-type-of uniform)))
    (multiple-value-bind (block-type buffer-type) (get-block-type type)
      (setf (gethash (cons block-type id) (blocks program))
            (make-instance 'shader-block
                           :name (format nil "_~a_~a" buffer-type name)
                           :type block-type
                           :layout layout)))))

(defun store-blocks (program stage)
  (map nil (lambda (layout) (make-block program layout)) (collect-layouts stage)))

(defun find-block (program-name block-type block-id)
  (alexandria:when-let ((program (find-program program-name)))
    (gethash (cons block-type block-id) (blocks program))))

(defun block-binding-valid-p (block binding-point)
  (every
   (lambda (x)
     (varjo:v-type-eq
      (varjo:v-type-of (uniform (layout block)))
      (varjo:v-type-of (uniform (layout x)))))
   (get-bound-blocks (block-type block) binding-point)))

(defun ensure-valid-block-binding (block binding-point)
  (or (block-binding-valid-p block binding-point)
      (error "Cannot bind a block to a binding point with existing blocks of a different layout.")))

(defun bind-uniform-block (program-name block-id binding-point)
  "Bind a uniform block to a binding point."
  (let* ((program-id (id (find-program program-name)))
         (block (find-block program-name :uniform block-id))
         (index (%gl:get-uniform-block-index program-id (name block))))
    (ensure-valid-block-binding block binding-point)
    (pushnew block (gethash binding-point (uniform-block-bindings *shader-info*)))
    (%gl:uniform-block-binding program-id index binding-point)))

(defun bind-shader-storage-block (program-name block-id binding-point)
  "Bind a shader storage block to a binding point."
  (let* ((program-id (id (find-program program-name)))
         (block (find-block program-name :buffer block-id))
         (index (gl:get-program-resource-index program-id :shader-storage-block (name block))))
    (ensure-valid-block-binding block binding-point)
    (pushnew block (gethash binding-point (buffer-block-bindings *shader-info*)))
    (%gl:shader-storage-block-binding program-id index binding-point)))
