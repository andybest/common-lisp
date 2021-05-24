(in-package #:%syntex.wfc)

(deftype direction () '(member :left :right :up :down))

(deftype direction-index () '(integer 0 3))

(deftype strategy () '(member :none :backtrack))

(defclass core ()
  ((%rng :reader rng
         :initarg :rng)
   (%seed :reader seed
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
   (%uncollapsed-count :accessor uncollapsed-count
                       :initform 0)
   (%strategy :reader strategy
              :initarg :strategy
              :initform :backtrack)
   (%history :reader history
             :initarg :history)
   (%tile-map :accessor tile-map
              :initarg :tile-map)))

(defun make-core (&key seed sample history strategy)
  (make-instance 'core
                 :rng (rng:make-generator seed)
                 :seed seed
                 :sample sample
                 :history history
                 :strategy strategy))

(u:fn-> direction->index (direction) direction-index)
(declaim (inline direction->index))
(defun direction->index (direction)
  (declare (optimize speed))
  (ecase direction
    (:left 0)
    (:right 1)
    (:up 2)
    (:down 3)))

(u:fn-> direction->offset (direction) (values (integer -1 1) (integer -1 1)))
(declaim (inline direction->offset))
(defun direction->offset (direction)
  (declare (optimize speed))
  (ecase direction
    (:left (values -1 0))
    (:right (values 1 0))
    (:up (values 0 -1))
    (:down (values 0 1))))

(u:fn-> invert-direction (direction) direction)
(declaim (inline invert-direction))
(defun invert-direction (direction)
  (declare (optimize speed))
  (ecase direction
    (:left :right)
    (:right :left)
    (:up :down)
    (:down :up)))
