(in-package #:%syntex.synthesizers.wfc)

(defclass topology-data ()
  ((%topology :reader topology
              :initarg :topology)
   (%data :reader data
          :initarg :data)))

(defgeneric get-value-by-index (topology-data index))

(defgeneric get-value-by-coords (topology-data x &optional y z))

(defmethod mask-topology ((data topology-data) mask)
  )
