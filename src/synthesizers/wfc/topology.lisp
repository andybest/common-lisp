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
   (%mask :accessor mask
          :initarg :mask)))

(defgeneric get-index (topology x y z))

(defgeneric get-coords (topology index))

(defgeneric mask-topology (topology mask))
