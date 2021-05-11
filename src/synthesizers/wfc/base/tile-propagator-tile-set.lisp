(in-package #:%syntex.synthesizers.wfc.base)

(defclass tile-propagator-tile-set ()
  ((%tiles :reader tiles
           :initarg :tiles)
   (%offset->patterns :reader offset->patterns
                      :initform (u:dict #'eql))))

(defun make-tile-propagator-tile-set (tiles)
  (make-instance 'tile-propagator-tile-set :tiles (map 'vector #'identity tiles)))
