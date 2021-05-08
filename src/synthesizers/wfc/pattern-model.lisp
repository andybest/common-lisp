(in-package #:%syntex.synthesizers.wfc.pattern-model)

(defclass pattern-model ()
  ((%propagator :accessor propagator)
   (%frequencies :accessor frequencies)))

(defun get-pattern-count (model)
  (length (frequencies model)))
