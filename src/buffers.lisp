(in-package :shadow)

(defclass shader-buffer ()
  ((%id :reader id
        :initform (gl:gen-buffer))
   (%target :reader target
            :initarg :target)
   (%layout :reader layout
            :initarg :layout)))

(defun %make-buffer (target layout)
  (make-instance 'shader-buffer :target target :layout layout))

(defun buffer-by-name (buffer-name)
  (gethash buffer-name (buffers *shader-info*)))

(defun buffer-type->target (type)
  (ecase type
    (:ubo :uniform-buffer)
    (:ssbo :shader-storage-buffer)))

(defgeneric get-bound-blocks (buffer-type binding-point)
  (:method ((buffer-type (eql :ubo)) binding-point)
    (gethash binding-point (uniform-block-bindings *shader-info*)))
  (:method ((buffer-type (eql :ssbo)) binding-point)
    (gethash binding-point (buffer-block-bindings *shader-info*))))

(defun create-buffer (type name binding-point)
  (let ((target (buffer-type->target type))
        (bound-blocks (get-bound-blocks type binding-point)))
    (if bound-blocks
        (let ((buffer (%make-buffer target (layout (first bound-blocks)))))
          (gl:bind-buffer target (id buffer))
          (%gl:buffer-data target (size (layout buffer)) (cffi:null-pointer) :static-draw)
          (%gl:bind-buffer-base target binding-point (id buffer))
          (%gl:bind-buffer target 0)
          (setf (gethash name (buffers *shader-info*)) buffer))
        (error "Cannot bind a buffer unless a block is bound to the same binding point."))))

(defun delete-buffer (buffer-name)
  (let ((buffer (buffer-by-name buffer-name)))
    (gl:delete-buffers (list (id buffer)))
    (remhash buffer-name (buffers *shader-info*))))

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

(defun write-buffer-path (buffer-name path value)
  (let* ((buffer (buffer-by-name buffer-name))
         (data (gethash path (members (layout buffer)))))
    (with-slots (%id %target) buffer
      (gl:bind-buffer %target %id)
      (%write-buffer-data buffer data value)
      (gl:bind-buffer %target 0))))
