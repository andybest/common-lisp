(in-package #:%syntex.synthesizers.wfc.transformed-tile)

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

(defun %make-transforms (&key transforms group treatments default-treatment)
  (make-instance 'transforms
                 :transforms transforms
                 :group group
                 :treatments treatments
                 :default-treatment default-treatment))

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
  (let ((list nil))
    (map nil
         (lambda (x)
           (u:mvlet ((tile success-p (transform-tile transforms x transform)))
             (when success-p
               (push tile list))))
         tiles)
    (nreverse list)))

(defgeneric transform-all (transforms tile/sequence))

(defmethod transform-all ((transforms transforms) (tile tile:tile))
  (let ((list nil))
    (map nil
         (lambda (x)
           (u:mvlet ((tile success-p (transform-tile transforms tile x)))
             (when success-p
               (push tile list))))
         (tfm:transforms (group transforms)))
    (nreverse list)))

(defmethod transform-all ((transforms transforms) (tiles sequence))
  (let ((list nil))
    (map nil
         (lambda (x)
           (u:when-let ((tiles (transform-all transforms x)))
             (push tiles list)))
         tiles)
    (u:flatten (nreverse list))))
