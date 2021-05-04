(in-package #:%syntex.synthesizers.wfc)

(defclass topology-data-1d (topology-data) ())

(defun make-topology-data-1d (topology data)
  (make-instance 'topology-data-1d
                 :topology topology
                 :data data))

(defmethod get-value ((topology-data topology-data-1d) (point point))
  (aref (data topology-data) (get-index (topology topology-data) point)))

(defmethod get-value ((topology-data topology-data-1d) (index integer))
  (aref (data topology-data) index))
