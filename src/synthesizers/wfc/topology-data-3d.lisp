(in-package #:%syntex.synthesizers.wfc)

(defclass topology-data-3d (topology-data) ())

(defun make-topology-data-3d (data &key topology periodic-p)
  (let ((topology (or topology
                      (make-grid-3d (array-dimension data 0)
                                    (array-dimension data 1)
                                    (array-dimension data 2)
                                    :periodic-p periodic-p))))
    (make-instance 'topology-data-3d :topology topology :data data)))

(defmethod get-value ((topology-data topology-data-3d) (point point:point))
  (aref (data topology-data) (point:x point) (point:y point) (point:z point)))

(defmethod get-value ((topology-data topology-data-3d) (index integer))
  (let ((point (top:get-coords (topology topology-data) index)))
    (get-value topology-data point)))
