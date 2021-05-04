(in-package #:%syntex.synthesizers.wfc)

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

(defgeneric contains-index-p (topology index)
  (:method ((topology topology) (index integer))
    (u:when-let (mask (mask topology))
      (aref mask index))))

(defgeneric get-indices (topology)
  (:method ((topology topology))
    (let ((indices nil))
      (dotimes (i (index-count topology))
        (when (contains-index-p topology i)
          (push i indices)))
      (nreverse indices))))

(defgeneric get-index (topology point))

(defgeneric get-coords (topology index))

(defgeneric try-move (topology point/index direction))
