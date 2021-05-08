(in-package #:%syntex.synthesizers.wfc.tile-propagator-tile-set)

(defclass tile-set ()
  ((%tiles :reader tiles
           :initarg :tiles)
   ;; dict(int -> hashset(int -> int))
   (%offset->patterns :reader offset->patterns
                      :initform (u:dict #'eql))))

(defun make-tile-set (tiles)
  (make-instance 'tile-set :tiles (map 'vector #'identity tiles)))
