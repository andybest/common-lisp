(in-package #:%syntex.synthesizers.wfc.model)

(defclass tile-model ()
  ((%tiles :reader tiles
           :initform nil)))

(defgeneric get-pattern (model tile))

(defgeneric get-mapping (model topology))

(defgeneric multiply-frequency (model tile multiplier))

(defgeneric set-frequency (model tile frequency &optional tile-transform))

(defgeneric add-sample (model sample &optional tile-transform))

(defun %multiply-frequency (model tile multiplier tile-transform)
  (let ((transformed-tiles (u:dict #'eq)))
    (map nil
         (lambda (x)
           (u:mvlet ((x success-p (tfm:transform-tile tile-transform tile x)))
             (when (and success-p (not (u:href transformed-tiles x)))
               (setf (u:href transformed-tiles x) x)
               (multiply-frequency model x multiplier))))
         (tfm:group tile-transform))))
