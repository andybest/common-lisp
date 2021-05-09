(in-package #:%syntex.synthesizers.wfc.tile-model)

(defclass model ()
  ((%tiles :reader tiles
           :initform nil)))

(defgeneric get-mapping (model topology))

(defgeneric multiply-frequency (model tile multiplier))

(defun %multiply-frequency (model tile multiplier transforms)
  (let ((transformed-tiles (u:dict #'eq)))
    (map nil
         (lambda (x)
           (u:mvlet ((x success-p (tfm.tile:transform-tile transforms tile x)))
             (when (and success-p (not (u:href transformed-tiles x)))
               (setf (u:href transformed-tiles x) x)
               (multiply-frequency model x multiplier))))
         (tfm.tile:group transforms))))
