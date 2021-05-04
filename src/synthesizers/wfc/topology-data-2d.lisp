(in-package #:%syntex.synthesizers.wfc)

(defclass topology-data-2d (topology-data) ())

(defun make-topology-data-2d (data &key topology periodic-p)
  (let ((topology (or topology
                      (make-topology-grid-2d (array-dimension data 0)
                                             (array-dimension data 1)
                                             :periodic-p periodic-p))))
    (make-instance 'topology-data-2d :topology topology :data data)))

(defmethod get-value-by-coords ((topology-data topology-data-2d) x &optional (y 0) (z 0))
  (declare (ignore z))
  (aref (data topology-data) x y))

(defmethod get-value-by-index ((topology-data topology-data-2d) index)
  (u:mvlet ((x y z (get-coords (topology topology-data) index)))
    (get-value-by-coords topology-data x y z)))
