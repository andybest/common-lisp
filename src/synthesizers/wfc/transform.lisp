(in-package #:%syntex.synthesizers.wfc.transform)

(defclass transform ()
  ((%rotation :reader rotation
              :initarg :rotation)
   (%reflect-x :reader reflect-x
               :initarg :reflect-x
               :initform nil)))

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

(defun make-transform (&optional (rotation 0) reflect-x)
  (make-instance 'transform :rotation rotation :reflect-x reflect-x))

(defun identity-p (transform)
  (and (= (rotation transform) 0)
       (not (reflect-x transform))))

(defun invert (transform)
  (let ((rotation (rotation transform))
        (reflect-x (reflect-x transform)))
    (make-transform (if reflect-x
                        rotation
                        (mod (- 360 rotation) 360))
                    reflect-x)))

(defun * (transform1 transform2)
  (let ((rotation1 (rotation transform1))
        (rotation2 (rotation transform2))
        (reflect-x1 (reflect-x transform1))
        (reflect-x2 (reflect-x transform2)))
    (make-transform (mod (+ (if reflect-x2 (- rotation1) rotation1) rotation2 360) 360)
                    (or (and reflect-x1 (not reflect-x2))
                        (and reflect-x2 (not reflect-x1))))))

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
