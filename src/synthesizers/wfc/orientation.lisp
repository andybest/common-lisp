(in-package #:%syntex.synthesizers.wfc)

(defclass orientation ()
  ((%rotation :reader rotation
              :initarg :rotation
              :initform 0)
   (%reflect-x :reader reflect-x
               :initarg :reflect-x
               :initform nil)))

(defclass orientation-group ()
  ((%rotation-count :reader rotation-count
                    :initarg :rotation-count
                    :initform 0)
   (%reflect-p :reader reflect-p
               :initarg :reflect-p
               :initform nil)
   (%angle :reader angle
           :initarg :angle
           :initform 0)
   (%orientations :reader orientations
                  :initform (make-array 0 :fill-pointer 0 :adjustable t))))

(defun identity-orientation-p (orientation)
  (and (= (rotation orientation) 0)
       (not (reflect-x orientation))))

(defun invert-orientation (orientation)
  (let ((rotation (rotation orientation))
        (reflect-x (reflect-x orientation)))
    (make-instance 'orientation :rotation (if reflect-x
                                              rotation
                                              (mod (- 360 rotation) 360))
                                :reflect-x reflect-x)))

(defun compose-orientations (orientation1 orientation2)
  (let ((rotation1 (rotation orientation1))
        (rotation2 (rotation orientation2))
        (reflect-x1 (reflect-x orientation1))
        (reflect-x2 (reflect-x orientation2)))
    (make-instance 'orientation
                   :rotation (mod (+ (if reflect-x2 (- rotation1) rotation1) rotation2 360) 360)
                   :reflect-x (or (and reflect-x1 (not reflect-x2))
                                  (and reflect-x2 (not reflect-x1))))))

(defun make-orientation-group (rotation-count reflect-p)
  (let* ((angle (truncate 360 rotation-count))
         (group (make-instance 'orientation-group
                               :rotation-count rotation-count
                               :reflect-p reflect-p
                               :angle angle)))
    (dotimes (reflection (if reflect-p 2 1))
      (loop :for rotation :below 360 :by angle
            :for orientation = (make-instance 'orientation
                                              :rotation rotation
                                              :reflect-x (plusp reflection))
            :do (vector-push-extend orientation (orientations group))))
    group))
