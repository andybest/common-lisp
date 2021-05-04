(in-package #:%syntex.synthesizers.wfc)

(defstruct (point
            (:constructor make-point (x y &optional (z 0)))
            (:conc-name p)
            (:predicate nil))
  (x 0 :type u:non-negative-fixnum)
  (y 0 :type u:non-negative-fixnum)
  (z 0 :type u:non-negative-fixnum))
