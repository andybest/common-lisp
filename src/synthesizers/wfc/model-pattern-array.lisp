(in-package #:%syntex.synthesizers.wfc.pattern-array)

(defclass pattern-array ()
  ((%values :reader values
            :initarg :values
            :initform (make-array (list 0 0 0)))))

(defun get-width (pattern-array)
  (array-dimension (values pattern-array) 0))

(defun get-height (pattern-array)
  (array-dimension (values pattern-array) 1))

(defun get-depth (pattern-array)
  (array-dimension (values pattern-array) 2))
