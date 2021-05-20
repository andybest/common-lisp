(in-package #:%syntex.internal)

(define-condition syntex-error (error)
  ((%synthesizer-type :reader synthesizer-type
                      :initarg :synthesizer-type)))

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

(define-condition invalid-wfc-kernel-size (syntex-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid pattern size ~s.~%~%Must be an integer between 2 and 255."
             (value condition)))))

(define-condition wfc-contradiction (syntex-error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Contradiction occurred."))))
