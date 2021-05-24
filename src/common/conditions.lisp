(in-package #:%syntex.conditions)

(define-condition syntex-error (error) ())

(define-condition syntex-warning (warning) ())

(define-condition file-not-found (syntex-error)
  ((%file-path :reader file-path
               :initarg :file-path))
  (:report
   (lambda (condition stream)
     (format stream "File not found: ~s" (file-path condition)))))

(define-condition invalid-seed (syntex-error)
  ((%seed :reader seed
          :initarg :seed))
  (:report
   (lambda (condition stream)
     (format stream "Invalid seed: ~s.~%~%Seed must be a string or NIL." (seed condition)))))

(define-condition invalid-dimension (syntex-error)
  ((%dimension :reader dimension
               :initarg :dimension)
   (%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid dimension ~s for ~s.~%~%Must be an integer between 8 and 65535."
             (value condition)
             (dimension condition)))))

(define-condition invalid-output-path (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid output path: ~s.~%~%Must be a pathname or a string."
             (value condition)))))

(define-condition invalid-harrison-kernel-size (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid kernel size ~s.~%~%Must be an integer between 1 and 255."
             (value condition)))))

(define-condition invalid-harrison-rounds (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid rounds ~s for Harrison synthesizer.~%~%Must be an integer between 1 ~
                     and 255."
             (value condition)))))

(define-condition invalid-harrison-candidate-count (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid candidate count ~s for Harrison synthesizer.~%~%Must be an integer ~
                     between 1 and 255."
             (value condition)))))

(define-condition invalid-wfc-pattern-size (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid pattern size ~s.~%~%Must be an integer between 2 and 255."
             (value condition)))))

(define-condition invalid-wfc-strategy (syntex-error)
  ((%value :reader value
           :initarg :value)
   (%allowed :reader allowed
             :initarg :allowed))
  (:report
   (lambda (condition stream)
     (format stream "Invalid strategy ~s.~%~%Must be one of ~{~a~^, ~}."
             (value condition)
             (allowed condition)))))

(define-condition invalid-wfc-backtrack-distance (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid backtrack distance ~s.~%~%Must be a positive fixnum."
             (value condition)))))

(define-condition invalid-wfc-backtrack-retry-count (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid backtrack retry count ~s.~%~%Must be a positive fixnum."
             (value condition)))))

(define-condition wfc-contradiction-warning (syntex-warning)
  ((%progress :reader progress
              :initarg :progress))
  (:report
   (lambda (condition stream)
     (format stream "Contradiction occurred at ~d%."
             (progress condition)))))

(define-condition wfc-contradiction-error (syntex-warning) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Contradiction occurred and the :NONE correction strategy was supplied."))))

(define-condition wfc-max-backtrack-retries-exceeded (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Maximum number of backtrack retries exceeded: ~d.~%~%~

                     Possible solutions:~%~

                     - Supply a larger :BACKTRACK-RETRIES value.~%~
                     - Supply a larger :BACKTRACK-DISTANCE value.~%~
                     - Use another correction strategy by changing the value of :STRATEGY. "
             (value condition)))))
