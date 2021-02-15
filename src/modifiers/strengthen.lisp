(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.strengthen
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:strengthen))

(in-package #:coherent-noise.modifiers.strengthen)

(defstruct (strengthen
            (:include int::sampler)
            (:constructor %strengthen)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (strength 1d0 :type u:f64)
  (bias 0d0 :type u:f64))

(defun strengthen (source strength &key (bias 0d0))
  (%strengthen :rng (int::sampler-rng source)
               :source source
               :strength strength
               :bias bias))

(defmethod int::sample ((sampler strengthen) x &optional (y 0d0) (z 0d0) (w 0d0))
  (+ (* (int::sample (source sampler) x y z w)
        (strength sampler))
     (bias sampler)))
