(in-package #:%syntex.synthesizers.wfc.point)

(defstruct (point
            (:constructor point (x y &optional (z 0)))
            (:conc-name nil)
            (:predicate nil)
            (:copier copy))
  (x 0 :type u:non-negative-fixnum)
  (y 0 :type u:non-negative-fixnum)
  (z 0 :type u:non-negative-fixnum))
