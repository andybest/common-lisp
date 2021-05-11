(in-package #:%syntex.synthesizers.wfc.topology)

(defclass data-2d (data) ())

(defclass data-2d/tiles (data/tiles) ())

(defun make-data-2d (values &key topology periodic-p)
  (let ((topology (or topology
                      (make-grid-2d (array-dimension values 0)
                                    (array-dimension values 1)
                                    :periodic-p periodic-p))))
    (make-instance 'data-2d :topology topology :values values)))

(defmethod get ((data data-2d) (point base:point))
  (aref (%values data) (base:point-x point) (base:point-y point)))

(defmethod get ((data data-2d) (index integer))
  (let ((point (get-coords (topology data) index)))
    (get data point)))
