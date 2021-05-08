(in-package #:%syntex.synthesizers.wfc.topology-data)

(defclass data-2d (top:data) ())

(defun make-data-2d (values &key topology periodic-p)
  (let ((topology (or topology
                      (grid:make-grid-2d (array-dimension values 0)
                                         (array-dimension values 1)
                                         :periodic-p periodic-p))))
    (make-instance 'data-2d :topology topology :values values)))

(defmethod top:get ((data data-2d) (point point:point))
  (aref (top:values data) (point:x point) (point:y point)))

(defmethod top:get ((data data-2d) (index integer))
  (let ((point (top:get-coords (top:topology data) index)))
    (top:get data point)))
