(in-package #:%syntex.synthesizers.wfc.tile-model)

(defclass model ()
  ((%tiles :reader tiles)))

(defgeneric get-mapping (model topology))

(defgeneric multiply-frequency (model tile multiplier))

#++(defun %multiply-frequency (model tile multiplier transform)
     (let ((transformed-tiles (u:dict #'eq)))
       (map nil
            (lambda (x))
            (tfm.tile:))))
