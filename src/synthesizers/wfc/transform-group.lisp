(in-package #:%syntex.synthesizers.wfc.transform)

(defclass group ()
  ((%rotation-count :reader rotation-count
                    :initarg :rotation-count
                    :initform 1)
   (%reflect-p :reader reflect-p
               :initarg :reflect-p
               :initform nil)
   (%angle :reader angle
           :initarg :angle)
   (%transforms :reader transforms
                :initform (make-array 0 :fill-pointer 0 :adjustable t))))

(defun make-group (rotation-count reflect-p)
  (let* ((angle (truncate 360 rotation-count))
         (group (make-instance 'group
                               :rotation-count rotation-count
                               :reflect-p reflect-p
                               :angle angle)))
    (dotimes (reflection (if reflect-p 2 1))
      (loop :for rotation :below 360 :by angle
            :for transform = (make-transform rotation (plusp reflection))
            :do (vector-push-extend transform (transforms group))))
    group))

(defun check (group transform)
  (let ((rotation (rotation transform)))
    (unless (find rotation (transforms group) :key #'rotation :test #'=)
      (error "Rotation angle ~d is not permitted." rotation))
    (when (and (reflect-x transform)
               (not (reflect-p group)))
      (error "Reflections are not permitted."))))
