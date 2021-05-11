(in-package #:%syntex.synthesizers.wfc.transform)

(defclass transformed-tile (tile:tile)
  ((%transform :reader transform
               :initarg :transform)))

(defun make-transformed-tile (&key tile transform)
  (make-instance 'transformed-tile :tile tile :transform transform))
