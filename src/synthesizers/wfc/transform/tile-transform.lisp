(in-package #:%syntex.synthesizers.wfc.transform)

(defclass tile-transform ()
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

(defun make-tile-transform (&key (rotation-count 1) reflect-p)
  (make-instance 'tile-transform :group (make-group rotation-count reflect-p)))

(defun transform-tile (tile-transform tile transform)
  (when (and (group tile-transform)
             (typep tile 'transformed-tile))
    (setf transform (* (transform tile) transform)))
  (u:when-let* ((table (u:href (transforms tile-transform) tile))
                (result (u:href table transform)))
    (return-from transform-tile (values result t)))
  (let ((treatment (or (u:href (treatments tile-transform) tile)
                       (default-treatment tile-transform))))
    (ecase treatment
      (:missing
       (values (base:make-tile nil) nil))
      (:unchanged
       (values tile t))
      (:generated
       (if (identity-p transform)
           (values tile t)
           (values (make-transformed-tile :tile tile :transform transform) t))))))

(defun transform-tiles (tile-transform tiles transform)
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t)))
    (map nil
         (lambda (x)
           (u:mvlet ((tile success-p (transform-tile tile-transform x transform)))
             (when success-p
               (vector-push-extend tile result))))
         tiles)
    result))

(defgeneric transform-all (tile-transform tile/sequence))

(defmethod transform-all ((tile-transform tile-transform) (tile base:tile))
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t)))
    (map nil
         (lambda (x)
           (u:mvlet ((tile success-p (transform-tile tile-transform tile x)))
             (when success-p
               (vector-push-extend tile result))))
         (transforms (group tile-transform)))
    result))

(defmethod transform-all ((tile-transform tile-transform) (tiles sequence))
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t)))
    (map nil
         (lambda (x)
           (u:when-let ((tiles (transform-all tile-transform x)))
             (map nil
                  (lambda (x)
                    (vector-push-extend x result))
                  tiles)))
         tiles)
    result))
