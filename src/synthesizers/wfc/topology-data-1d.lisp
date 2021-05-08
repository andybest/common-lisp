(in-package #:%syntex.synthesizers.wfc.topology-data)

(defclass data-1d (top:data) ())

(defun make-data-1d (topology values)
  (make-instance 'data-1d :topology topology :values values))

(defmethod top:get ((data data-1d) (point point:point))
  (aref (top:values data) (top:get-index (top:topology data) point)))

(defmethod top:get ((data data-1d) (index integer))
  (aref (top:values data) index))
