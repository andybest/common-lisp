(in-package #:%syntex.synthesizers.wfc.tile-transform)

(defclass tile (tile:tile)
  ((%transform :reader transform
               :initarg :transform)))

(defclass transforms ()
  ((%transforms :reader transforms
                :initarg :transforms
                :initform (u:dict #'eq))
   (%group :reader group
           :initarg :group)
   (%treatments :reader treatments
                :initarg :treatments
                :initform (u:dict #'eq))
   (%default-treatment :reader default-treatment
                       :initarg :default-treatment
                       :initform :unchanged)))

(defun make-tile (&key tile transform)
  (make-instance 'tile :tile tile :transform transform))

(defun make-transforms (&key (rotation-count 1) reflect-p)
  (make-instance 'transforms :group (tfm:make-group rotation-count reflect-p)))

(defun transform-tile (transforms tile transform)
  (when (and (group transforms)
             (typep tile 'tile))
    (setf transform (tfm:* (transform tile) transform)))
  (u:when-let* ((table (u:href (transforms transforms) tile))
                (result (u:href table transform)))
    (return-from transform-tile (values result t)))
  (let ((treatment (or (u:href (treatments transforms) tile)
                       (default-treatment transforms))))
    (ecase treatment
      (:missing
       (values (tile:tile nil) nil))
      (:unchanged
       (values tile t))
      (:generated
       (if (tfm:identity-p transform)
           (values tile t)
           (values (make-tile :tile tile :transform transform) t))))))

(defun transform-tiles (transforms tiles transform)
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t)))
    (map nil
         (lambda (x)
           (u:mvlet ((tile success-p (transform-tile transforms x transform)))
             (when success-p
               (vector-push-extend tile result))))
         tiles)
    result))

(defgeneric transform-all (transforms tile/sequence))

(defmethod transform-all ((transforms transforms) (tile tile:tile))
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t)))
    (map nil
         (lambda (x)
           (u:mvlet ((tile success-p (transform-tile transforms tile x)))
             (when success-p
               (vector-push-extend tile result))))
         (tfm:transforms (group transforms)))
    result))

(defmethod transform-all ((transforms transforms) (tiles sequence))
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t)))
    (map nil
         (lambda (x)
           (u:when-let ((tiles (transform-all transforms x)))
             (map nil
                  (lambda (x)
                    (vector-push-extend x result))
                  tiles)))
         tiles)
    result))
