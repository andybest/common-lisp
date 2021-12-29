(in-package #:mfiano.misc.rng)

(define-condition rng-error (error)
  ((%generator :reader generator
               :initarg :generator)))

(define-condition invalid-range (rng-error)
  ((%lower-bound :reader lower-bound
                 :initarg :min)
   (%upper-bound :reader upper-bound
                 :initarg :max))
  (:report
   (lambda (condition stream)
     (format stream "Invalid range: [min: ~s, max: ~s].~%~
                     Lower bound can not be larger than upper bound.~%~
                     Generator seed: ~a"
             (lower-bound condition)
             (upper-bound condition)
             (seed (generator condition))))))

(define-condition empty-sequence (rng-error) ()
  (:report
   (lambda (condition stream)
     (format stream "Unable to choose a random element because sequence is empty.~%~
                     Generator seed: ~a"
             (seed (generator condition))))))
