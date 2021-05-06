(in-package #:%syntex.synthesizers.wfc)

(defclass transformed-tile (tile:tile)
  ((%transform :reader transform
               :initarg :transform)))

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

(defun make-transformed-tile (&key tile transform)
  (make-instance 'transformed-tile :tile tile :transform transform))

(defun %make-tile-transform (&key transforms group treatments default-treatment)
  (make-instance 'tile-transform
                 :transforms transforms
                 :group group
                 :treatments treatments
                 :default-treatment default-treatment))

(defun make-tile-transform (&key (rotation-count 1) reflect-p)
  (make-instance 'tile-transform
                 :group (tfm:make-group rotation-count reflect-p)))

(defgeneric transform-tile (tile-transform tile transform)
  (:method ((tile-transform tile-transform) (tile tile:tile) (transform tfm:transform))
    (when (and (group tile-transform)
               (typep tile 'transformed-tile))
      (setf transform (tfm:* (transform tile) transform)))
    (u:when-let* ((table (u:href (transforms tile-transform) tile))
                  (result (u:href table transform)))
      (return-from transform-tile (values result t)))
    (let ((treatment (or (u:href (treatments tile-transform) tile)
                         (default-treatment tile-transform))))
      (ecase treatment
        (:missing
         (values (tile:tile nil) nil))
        (:unchanged
         (values tile t))
        (:generated
         (if (tfm:identity-p transform)
             (values tile t)
             (values (make-transformed-tile :tile tile :transform transform) t)))))))

(defgeneric transform-tiles (tile-transform tiles transform)
  (:method ((tile-transform tile-transform) (tiles sequence) (transform tfm:transform))
    (let ((list nil))
      (map nil
           (lambda (x)
             (u:mvlet ((tile success-p (transform-tile tile-transform x transform)))
               (when success-p
                 (push tile list))))
           tiles)
      (nreverse list))))

(defgeneric transform-all (tile-transform tile/sequence))

(defmethod transform-all ((tile-transform tile-transform) (tile tile:tile))
  (let ((list nil))
    (map nil
         (lambda (x)
           (u:mvlet ((tile success-p (transform-tile tile-transform tile x)))
             (when success-p
               (push tile list))))
         (transforms (group tile-transform)))
    (nreverse list)))

(defmethod transform-all ((tile-transform tile-transform) (tiles sequence))
  (let ((list nil))
    (map nil
         (lambda (x)
           (u:when-let ((tiles (transform-all tile-transform x)))
             (push tiles list)))
         tiles)
    (u:flatten (nreverse list))))
