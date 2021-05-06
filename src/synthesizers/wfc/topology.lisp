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

(defun contains-index-p (topology index)
  (u:when-let (mask (mask topology))
    (aref mask index)))

(defun get-indices (topology)
  (let ((indices nil))
    (dotimes (i (index-count topology))
      (when (contains-index-p topology i)
        (push i indices)))
    (nreverse indices)))

(defgeneric get-index (topology point))

(defgeneric get-coords (topology index))

(defgeneric try-move (topology point/index direction))
