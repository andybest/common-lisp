(in-package #:%syntex.wfc)

(deftype direction () '(member :left :right :up :down))

(deftype direction-index () '(integer 0 3))

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
