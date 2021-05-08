(in-package #:%syntex.synthesizers.wfc.topology-data)

(defclass data-3d (top:data) ())

(defun make-data-3d (values &key topology periodic-p)
  (let ((topology (or topology
                      (grid:make-grid-3d (array-dimension values 0)
                                         (array-dimension values 1)
                                         (array-dimension values 2)
                                         :periodic-p periodic-p))))
    (make-instance 'data-3d :topology topology :values values)))

(defmethod top:get ((data data-3d) (point point:point))
  (aref (top:values data) (point:x point) (point:y point) (point:z point)))

(defmethod top:get ((data data-3d) (index integer))
  (let ((point (top:get-coords (top:topology data) index)))
    (top:get data point)))
