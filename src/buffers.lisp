(in-package :shadow)

(defvar *current-binding-point*)

(defclass buffer ()
  ((%id :accessor id
        :initform 0)
   (%name :reader name
          :initarg :name)
   (%type :reader block-type
          :initarg :type)
   (%target :reader target
            :initarg :target)
   (%binding-point :accessor binding-point
                   :initform 0)
   (%size :reader size
          :initarg :size)
   (%members :reader members
             :initform (make-hash-table))))

(defclass buffer-data ()
  ((%dimensions :reader dimensions
                :initarg :dimensions
                :initform 1)
   (%element-count :reader element-count
                   :initarg :count
                   :initform 1)
   (%element-type :reader element-type
                  :initarg :element-type)
   (%stride :reader stride
            :initarg :stride)
   (%offset :reader offset
            :initarg :offset)
   (%size :reader size
          :initarg :size)))

(defun buffer-by-name (buffer-name)
  (gethash buffer-name (buffers *shader-info*)))

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

(defun make-uniform-buffer (block-id block-name data)
  (let ((buffer (make-instance 'buffer
                               :name block-name
                               :type :ubo
                               :target (buffer-type->target :ubo)
                               :size (getf data :size))))
    (labels ((make-data-symbol (name)
               (ensure-keyword (format nil "~{~a~^.~}" (alexandria:ensure-list name))))
             (make-data ()
               (dolist (part (getf data :members))
                 (destructuring-bind
                     (&key type name offset size stride matrix-stride &allow-other-keys) part
                   (alexandria:when-let ((key (make-data-symbol name))
                                         (unpacked-type (unpack-type type)))
                     (setf (gethash key (members buffer))
                           (apply #'make-instance 'buffer-data
                                  :offset offset
                                  :size size
                                  :stride (or stride matrix-stride size)
                                  unpacked-type)))))))
      (setf (gethash block-id (buffers *shader-info*)) buffer)
      (make-data))))

(defun bind-uniform-blocks (program)
  (let ((*current-binding-point* 1))
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (let ((index (gl:get-uniform-block-index (id program) (name v)))
             (binding *current-binding-point*))
         (%gl:uniform-block-binding (id program) index binding)
         (%gl:bind-buffer-base :uniform-buffer binding (id v))
         (setf (binding-point v) binding)
         (incf *current-binding-point*)))
     (buffers *shader-info*))))

(defun store-uniform-blocks (stages)
  (loop :with blocks = (collect-blocks stages :ubo)
        :with structs = (collect-block-structs blocks)
        :for ((root) . (data)) :in (pack-all structs blocks)
        :when (find root blocks :key #'varjo:name)
          :do (let* ((block-id (ensure-keyword root))
                     (block-name (format nil "_UBO_~a" block-id)))
                (make-uniform-buffer block-id block-name data))))

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

(defun %write-buffer-data (buffer data value)
  (check-type value sequence)
  (let ((count (length value))
        (padding 4))
    (with-slots (%target) buffer
      (with-slots (%offset %element-count %element-type %stride) data
        (when (> count %element-count)
          (error "Cannot write more data to a buffer's path than its size."))
        (static-vectors:with-static-vector (sv (* count padding) :element-type %element-type)
          (let ((pointer (static-vectors:static-vector-pointer sv))
                (i 0))
            (map nil
                 (lambda (x)
                   (if (typep x 'sequence)
                       (replace sv x :start1 i)
                       (setf (aref sv i) x))
                   (incf i padding))
                 value)
            (%gl:buffer-sub-data %target %offset (* %stride count) pointer)))))))

(defun write-buffer-data (buffer-name path value)
  (let* ((buffer (buffer-by-name buffer-name))
         (data (gethash path (members buffer))))
    (with-slots (%id %target) buffer
      (gl:bind-buffer %target %id)
      (%write-buffer-data buffer data value)
      (gl:bind-buffer %target 0))))
