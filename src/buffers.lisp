(in-package :shadow)

(defclass buffer ()
  ((%id :accessor id
        :initform 0)
   (%name :reader name
          :initarg :name)
   (%type :reader block-type
          :initarg :type)
   (%target :reader target
            :initarg :target)
   (%size :reader size
          :initarg :size)
   (%members :reader members
             :initform (make-hash-table))))

(defclass buffer-data ()
  ((%type :reader data-type
          :initarg :type)
   (%dimensions :reader dimensions
                :initarg :dimensions
                :initform nil)
   (%count :reader element-count
           :initarg :count
           :initform 1)
   (%offset :reader offset
            :initarg :offset)
   (%size :reader size
          :initarg :size)))

(defun buffer-type->target (type)
  (ecase type
    (:ubo :uniform-buffer)
    (:ssbo :shader-storage)))

(defun collect-blocks (stages block-type)
  (remove-duplicates
   (mapcan
    (lambda (x)
      (loop :for uniform :in (varjo:uniform-variables x)
            :when (qualifier-exists-p uniform block-type)
              :collect uniform))
    stages)
   :key #'varjo:name))

(defun collect-block-structs (blocks)
  (let ((structs))
    (labels ((process (type)
               (typecase type
                 (varjo:v-array (process-array type))
                 (varjo:v-user-struct (process-struct type))))
             (process-array (type)
               (process (varjo:v-element-type type)))
             (process-struct (type)
               (unless (find type structs :test #'varjo:v-type-eq)
                 (map nil (lambda (x) (process (second x))) (varjo.internals:v-slots type))
                 (push type structs)))
             (find-structs (types)
               (map nil #'process types)
               (reverse structs)))
      (find-structs
       (mapcan
        (lambda (x)
          (loop :with struct = (varjo:v-type-of x)
                :for (nil type) :in (varjo.internals:v-slots struct)
                :when (typep type 'varjo:v-user-struct)
                  :collect type))
        blocks)))))

(defun make-buffer-data (buffer data)
  (flet ((make-buffer-data-symbol (name)
           (ensure-keyword
            (format nil "~{~a~^.~}" (alexandria:ensure-list name)))))
    (dolist (part (getf data :members))
      (destructuring-bind (&key type name offset size &allow-other-keys) part
        (let ((key (make-buffer-data-symbol name))
              (unpacked-type (unpack-type type)))
          (when unpacked-type
            (setf (gethash key (members buffer))
                  (apply #'make-instance 'buffer-data
                         :offset offset
                         :size size
                         unpacked-type))))))))

(defun store-buffer-data (stages)
  (dolist (block-type '(:ubo :ssbo))
    (loop :with blocks = (collect-blocks stages block-type)
          :with structs = (collect-block-structs blocks)
          :with packing = (pack-all structs blocks)
            :initially (when *debugp* (format t "~&~a packing:~%~S~%~%" block-type packing))
          :for ((root) . (data)) :in (pack-all structs blocks)
          :when (find root blocks :key #'varjo:name)
            :do (let ((buffer (make-instance 'buffer
                                             :name (format nil "_~a_~a" block-type root)
                                             :type block-type
                                             :target (buffer-type->target block-type)
                                             :size (getf data :size))))
                  (setf (gethash (ensure-keyword root) (buffers *shader-info*)) buffer)
                  (make-buffer-data buffer data)))))

(defun initialize-buffers ()
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (unless (zerop (id v))
       (gl:delete-buffers (list (id v))))
     (let ((id (gl:gen-buffer)))
       (gl:bind-buffer (target v) id)
       (%gl:buffer-data (target v) (size v) (cffi:null-pointer) :static-draw)
       (%gl:bind-buffer (target v) 0)
       (setf (id v) id)))
   (buffers *shader-info*)))
