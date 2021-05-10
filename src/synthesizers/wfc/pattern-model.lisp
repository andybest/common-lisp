(in-package #:%syntex.synthesizers.wfc.pattern-model)

(defclass pattern-model ()
  ((%propagator :reader propagator
                :initarg :propagator)
   (%frequencies :reader frequencies
                 :initarg :frequencies)))

(defun get-pattern-count (model)
  (length (frequencies model)))
