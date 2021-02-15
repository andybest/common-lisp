(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.power
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:power))

(in-package #:coherent-noise.modifiers.power)

(defstruct (power
            (:include int::sampler)
            (:constructor %power)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int::sampler)
  (source2 nil :type int::sampler))

(defun power (source1 source2)
  (%power :rng (int::sampler-rng source1)
          :source1 source1
          :source2 source2))

(defmethod int::sample ((sampler power) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample1 (abs (* (1+ (the u:f32 (int::sample (source1 sampler) x y z w))) 0.5)))
        (sample2 (abs (* (1+ (the u:f32 (int::sample (source2 sampler) x y z w))) 0.5))))
    (1- (* (cl:expt sample1 sample2) 2))))
