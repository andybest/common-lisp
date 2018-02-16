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

(defun create-buffer (type name binding-point)
  "Create a buffer of the given TYPE and NAME, binding it to a binding point."
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
  "Delete the buffer having a name of BUFFER-NAME."
  (let ((buffer (buffer-by-name buffer-name)))
    (gl:delete-buffers (list (id buffer)))
    (remhash buffer-name (buffers *shader-info*))))

(defun %write-buffer-member (target member value)
  (with-slots (%element-type %offset %element-stride %byte-stride) member
    (let ((count (length value)))
      (static-vectors:with-static-vector (sv (* count %element-stride) :element-type %element-type)
        (let ((ptr (static-vectors:static-vector-pointer sv))
              (i 0))
          (map nil
               (lambda (x)
                 (if (typep x 'sequence)
                     (replace sv x :start1 i)
                     (setf (aref sv i) x))
                 (incf i %element-stride))
               value)
          (%gl:buffer-sub-data target %offset (* count %byte-stride) ptr))))))

(defun %write-buffer-member-matrix (target member value)
  (with-slots (%element-type %offset %element-stride %byte-stride %dimensions) member
    (let ((count (length value)))
      (destructuring-bind (columns . rows) %dimensions
        (static-vectors:with-static-vector (sv (* count columns %element-stride)
                                               :element-type %element-type)
          (let ((ptr (static-vectors:static-vector-pointer sv))
                (i 0))
            (map nil
                 (lambda (x)
                   (loop :repeat columns
                         :for j :from i :by %element-stride
                         :for k :by rows
                         :do (replace sv x :start1 j :start2 k :end2 (+ k rows)))
                   (incf i (* columns %element-stride)))
                 value)
            (%gl:buffer-sub-data target %offset (* count %byte-stride) ptr)))))))

(defun write-buffer-path (buffer-name path value)
  "Write VALUE to the buffer with the name BUFFER-NAME, starting at the given PATH.

PATH: A \"dot-separated\" keyword symbol, where each part denotes a member in the buffers block
layout.

Value: A value to write, such as a scalar or matrix depending on the type of the member PATH refers
to. To write to an array, use a sequence of values.

Note: Writing to arrays which contain other aggregates types (other arrays or structures) is not
possible. This is a design decision to allow this library to have a simple \"path-based\" buffer
writing interface."
  (with-slots (%id %target %layout) (buffer-by-name buffer-name)
    (let ((member (gethash path (members %layout))))
      (check-type value sequence)
      (gl:bind-buffer %target %id)
      (if (cdr (dimensions member))
          (%write-buffer-member-matrix %target member value)
          (%write-buffer-member %target member value))
      (gl:bind-buffer %target 0))))
