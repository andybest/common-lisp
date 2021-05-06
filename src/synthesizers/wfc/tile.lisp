(in-package #:%syntex.synthesizers.wfc.tile)

(defclass tile ()
  ((%value :reader value
           :initarg :value)))

(defun tile (value)
  (make-instance 'tile :value value))
