(in-package #:%syntex.synthesizers.wfc.base)

(defstruct (point
            (:constructor make-point (x y &optional (z 0)))
            (:predicate nil))
  (x 0 :type u:non-negative-fixnum)
  (y 0 :type u:non-negative-fixnum)
  (z 0 :type u:non-negative-fixnum))
