(in-package #:%syntex.wfc.core)

(deftype direction () '(member :left :right :up :down))

(deftype direction-index () '(integer 0 3))

(declaim (inline %make-core))
(defstruct (core
            (:constructor %make-core)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (rng nil :type rng:generator)
  (seed nil :type (or string null))
  (sample nil :type grid:grid)
  (data->pattern (u:dict #'equalp) :type hash-table)
  (id->pattern (make-array 0) :type simple-vector)
  (adjacencies (make-array 0) :type simple-vector)
  tile-map)

(u:define-printer (core stream :type nil)
  (format stream "CORE"))

(defun make-core (&key seed sample tile-map)
  (%make-core :rng (rng:make-generator seed)
              :sample sample
              :tile-map tile-map))

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
