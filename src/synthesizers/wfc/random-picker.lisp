(in-package #:%syntex.synthesizers.wfc.random-picker)

(defclass picker () ())

(defgeneric get-index (picker func))

(defgeneric get-pattern (picker index func))
