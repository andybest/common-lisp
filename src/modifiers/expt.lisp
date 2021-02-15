(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.expt
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:expt)
  (:export
   #:expt))

(in-package #:coherent-noise.modifiers.expt)

(defstruct (expt
            (:include int::sampler)
            (:constructor %expt)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (power 1d0 :type u:f64))

(defun expt (source power)
  (%expt :rng (int::sampler-rng source)
         :source source
         :power power))

(defmethod int::sample ((sampler expt) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let ((sample (int::sample (source sampler) x y z w)))
    (1- (* (cl:expt (abs (* (1+ sample) 0.5)) (power sampler)) 2))))
