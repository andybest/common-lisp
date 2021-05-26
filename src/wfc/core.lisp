(in-package #:%syntex.wfc)

(defvar *rng* nil)

(deftype strategy () '(member :none :backtrack))

(defclass core ()
  ((%seed :reader seed
          :initarg :seed)
   (%sample :reader sample
            :initarg :sample)
   (%data->pattern :reader data->pattern
                   :initform (u:dict #'equalp))
   (%id->pattern :accessor id->pattern
                 :initform (make-array 0))
   (%adjacencies :accessor adjacencies
                 :initform (make-array 0))
   (%progress :accessor progress
              :initform 0)
   (%strategy :reader strategy
              :initarg :strategy
              :initform :backtrack)
   (%backtracker :reader backtracker
                 :initarg :backtracker)
   (%tile-map :accessor tile-map)))

(defun make-core (&key seed sample backtracker strategy)
  (make-instance 'core :seed seed :sample sample :backtracker backtracker :strategy strategy))
