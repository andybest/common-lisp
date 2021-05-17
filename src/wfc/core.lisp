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
  (adjacencies (u:dict #'eql) :type hash-table)
  (origin-colors (u:make-ub32-array 0) :type u:ub32a)
  (tile-map nil :type grid:grid))

(u:define-printer (core stream :type nil)
  (format stream "CORE"))

(defun make-core (&key seed sample tile-map)
  (%make-core :rng (rng:make-generator seed)
              :sample sample
              :patterns (pat:make-pattern-collection)
              :tile-map tile-map))
