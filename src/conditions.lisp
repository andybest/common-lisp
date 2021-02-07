(in-package #:seedable-rng)

(define-condition seedable-rng-error (error) ())

(define-condition invalid-range (seedable-rng-error)
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

(define-condition empty-sequence (seedable-rng-error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unable to choose a random element because sequence is empty."))))
