(in-package #:%syntex.synthesizers.wfc.topology)

(defclass data-1d (data) ())

(defclass data-1d/tiles (data/tiles) ())

(defun make-data-1d (topology values)
  (make-instance 'data-1d :topology topology :values values))

(defmethod get ((data data-1d) (point base:point))
  (aref (%values data) (get-index (topology data) point)))

(defmethod get ((data data-1d) (index integer))
  (aref (%values data) index))
