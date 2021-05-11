(in-package #:%syntex.synthesizers.wfc.topology)

(defclass topology ()
  ((%index-count :accessor index-count)
   (%directions-count :accessor directions-count)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%depth :reader depth
           :initarg :depth)
   (%mask :reader mask
          :initarg :mask
          :initform nil)))

(defclass data ()
  ((%topology :reader topology
              :initarg :topology)
   (%values :reader %values
            :initarg :values)))

(defclass data/tiles (data) ())

(defun contains-index-p (topology index)
  (let ((mask (mask topology)))
    (when (or (not mask) (aref mask index))
      t)))

(defun get-indices (topology)
  (let ((indices (make-array 0 :fill-pointer 0 :adjustable t)))
    (dotimes (i (index-count topology))
      (when (contains-index-p topology i)
        (vector-push-extend i indices)))
    indices))

(defgeneric get (data point/index))

(defgeneric get-index (topology point))

(defgeneric get-coords (topology index))

(defgeneric try-move (topology point/index direction))
