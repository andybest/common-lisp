(in-package #:%syntex.synthesizers.wfc.constraint-tile)

(defclass tile-constraint () ())

(defgeneric init (constraint propagator))

(defgeneric check (constraint propagator))
