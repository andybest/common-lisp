(in-package #:%syntex.synthesizers.wfc.random-picker)

(defclass picker () ())

(defgeneric get-index (picker func &optional external-priority))

(defgeneric get-pattern (picker index func))
