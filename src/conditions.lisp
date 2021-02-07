(in-package #:seedable-rng)

(define-condition invalid-range ()
  ((%lower-bound :reader lower-bound
                 :initarg :min)
   (%upper-bound :reader upper-bound
                 :initarg :max))
  (:report
   (lambda (condition stream)
     (format stream "Invalid range: [min: ~s, max: ~s].~%~
                     Lower bound can not be larger than upper bound."
             (lower-bound condition)
             (upper-bound condition)))))
