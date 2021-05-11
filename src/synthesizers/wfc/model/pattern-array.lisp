(in-package #:%syntex.synthesizers.wfc.model)

(defun make-pattern-array (values)
  (u:copy-array values :element-type 'fixnum))

(defun get-width (pattern-array)
  (array-dimension pattern-array 0))

(defun get-height (pattern-array)
  (array-dimension pattern-array 1))

(defun get-depth (pattern-array)
  (array-dimension pattern-array 2))
