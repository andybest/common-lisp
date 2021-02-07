(in-package #:coherent-noise/internal)

(define-condition coherent-noise-error (error)
  ((%sampler-type :reader sampler-type
                  :initarg :sampler-type)))

(define-condition invalid-seed (coherent-noise-error)
  ((%seed :reader seed
          :initarg :seed))
  (:report
   (lambda (condition stream)
     (format stream "Invalid seed: ~s.~%~%Seed must be a string." (seed condition)))))

(define-condition invalid-cellular-distance-method (coherent-noise-error)
  ((%distance-method :reader distance-method
                     :initarg :distance-method)
   (%valid-distance-methods :reader valid-distance-methods
                            :initarg :valid-distance-methods))
  (:report
   (lambda (condition stream)
     (format stream "Invalid distance method ~s for sampler ~s.~%~%Valid types: ~{~s~^, ~}"
             (distance-method condition)
             (sampler-type condition)
             (valid-distance-methods condition)))))

(define-condition invalid-cellular-output-type (coherent-noise-error)
  ((%output-type :reader output-type
                 :initarg :output-type)
   (%valid-output-types :reader valid-output-types
                        :initarg :valid-output-types))
  (:report
   (lambda (condition stream)
     (format stream "Invalid output type ~s for sampler ~s.~%~%Valid types: ~{~s~^, ~}"
             (output-type condition)
             (sampler-type condition)
             (valid-output-types condition)))))

(define-condition invalid-cellular-jitter (coherent-noise-error)
  ((%jitter :reader jitter
            :initarg :jitter))
  (:report
   (lambda (condition stream)
     (format stream "Invalid jitter ~s for sampler ~s.~%~%Jitter must be a real number."
             (jitter condition)
             (sampler-type condition)))))
