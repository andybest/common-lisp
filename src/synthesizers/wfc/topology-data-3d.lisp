(in-package #:%syntex.synthesizers.wfc)

(defclass topology-data-3d (topology-data) ())

(defun make-topology-data-3d (data &key topology periodic-p)
  (let ((topology (or topology
                      (make-topology-grid-3d (array-dimension data 0)
                                             (array-dimension data 1)
                                             (array-dimension data 2)
                                             :periodic-p periodic-p))))
    (make-instance 'topology-data-3d :topology topology :data data)))

(defmethod get-value-by-coords ((topology-data topology-data-3d) x &optional (y 0) (z 0))
  (aref (data topology-data) x y z))

(defmethod get-value-by-index ((topology-data topology-data-3d) index)
  (u:mvlet ((x y z (get-coords (topology topology-data) index)))
    (get-value-by-coords topology-data x y z)))
