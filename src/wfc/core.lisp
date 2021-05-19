(in-package #:%syntex.wfc.core)

(defstruct (core
            (:constructor %make-core)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (rng nil :type rng:generator)
  (seed "" :type string)
  (sample nil :type grid:grid)
  (patterns nil :type pat:pattern-collection)
  (frequencies (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (adjacencies (make-array 0) :type simple-array)
  tile-map)

(u:define-printer (core stream :type nil)
  (format stream "CORE"))

(define-condition contradiction (error) ())

(defun make-core (&key seed sample tile-map)
  (%make-core :rng (rng:make-generator seed)
              :sample sample
              :patterns (pat:make-pattern-collection)
              :tile-map tile-map))

(defun direction->index (direction)
  (ecase direction
    (:left 0)
    (:right 1)
    (:up 2)
    (:down 3)))

(defun direction->offset (direction)
  (ecase direction
    (:left (values -1 0))
    (:right (values 1 0))
    (:up (values 0 -1))
    (:down (values 0 1))))
