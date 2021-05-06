(in-package #:%syntex.synthesizers.wfc)

(defclass topology-data-2d (topology-data) ())

(defun make-topology-data-2d (data &key topology periodic-p)
  (let ((topology (or topology
                      (make-grid-2d (array-dimension data 0)
                                    (array-dimension data 1)
                                    :periodic-p periodic-p))))
    (make-instance 'topology-data-2d :topology topology :data data)))

(defmethod get-value ((topology-data topology-data-2d) (point point:point))
  (aref (data topology-data) (point:x point) (point:y point)))

(defmethod get-value ((topology-data topology-data-2d) (index integer))
  (let ((point (top:get-coords (topology topology-data) index)))
    (get-value topology-data point)))
