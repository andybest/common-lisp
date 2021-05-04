(in-package #:%syntex.synthesizers.wfc)

(defclass topology-data-1d (topology-data) ())

(defun make-topology-data-1d (topology data)
  (make-instance 'topology-data-1d
                 :topology topology
                 :data data))

(defmethod get-value-by-coords ((topology-data topology-data-1d) x &optional (y 0) (z 0))
  (aref (data topology-data) (get-index (topology topology-data) x y z)))

(defmethod get-value-by-index ((topology-data topology-data-1d) index)
  (aref (data topology-data) index))
