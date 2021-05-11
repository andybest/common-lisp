(in-package #:%syntex.synthesizers.wfc.base)

(defclass tile ()
  ((%value :reader value
           :initarg :value)))

(defun make-tile (value)
  (make-instance 'tile :value value))
