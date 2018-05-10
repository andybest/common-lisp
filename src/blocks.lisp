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

(defgeneric get-bound-blocks (block-type binding-point)
  (:method ((block-type (eql :uniform)) binding-point)
    (au:href (uniform-block-bindings *shader-info*) binding-point))
  (:method ((block-type (eql :buffer)) binding-point)
    (au:href (buffer-block-bindings *shader-info*) binding-point)))

(defun make-block (program layout)
  (let* ((uniform (uniform layout))
         (id (ensure-keyword (varjo:name uniform)))
         (name (varjo.internals:safe-glsl-name-string id))
         (type (varjo:v-type-of uniform)))
    (au:mvlet ((block-type buffer-type (get-block-type type)))
      (setf (au:href (blocks program) (cons block-type id))
            (make-instance 'shader-block
                           :name (format nil "_~a_~a" buffer-type name)
                           :type block-type
                           :layout layout)))))

(defun store-blocks (program stage)
  (map nil (lambda (layout) (make-block program layout)) (collect-layouts stage)))

(defun find-block (program-name block-type block-id)
  (au:when-let ((program (find-program program-name)))
    (au:href (blocks program) (cons block-type block-id))))

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
    (pushnew block (au:href (uniform-block-bindings *shader-info*) binding-point))
    (%gl:uniform-block-binding program-id index binding-point)))

(defun bind-shader-storage-block (program-name block-id binding-point)
  "Bind a shader storage block to a binding point."
  (let* ((program-id (id (find-program program-name)))
         (block (find-block program-name :buffer block-id))
         (index (gl:get-program-resource-index program-id :shader-storage-block (name block))))
    (ensure-valid-block-binding block binding-point)
    (pushnew block (au:href (buffer-block-bindings *shader-info*) binding-point))
    (%gl:shader-storage-block-binding program-id index binding-point)))
