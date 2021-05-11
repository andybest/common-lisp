(in-package #:%syntex.synthesizers.wfc.topology)

(defclass data-3d (data) ())

(defclass data-3d/tiles (data/tiles) ())

(defun make-data-3d (values &key topology periodic-p)
  (let ((topology (or topology
                      (make-grid-3d (array-dimension values 0)
                                    (array-dimension values 1)
                                    (array-dimension values 2)
                                    :periodic-p periodic-p))))
    (make-instance 'data-3d :topology topology :values values)))

(defmethod get ((data data-3d) (point point:point))
  (aref (values data) (point:x point) (point:y point) (point:z point)))

(defmethod get ((data data-3d) (index integer))
  (let ((point (get-coords (topology data) index)))
    (get data point)))
