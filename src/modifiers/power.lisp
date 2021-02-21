(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.power
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.power)

(defstruct (power
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:power (source1 source2)
  (make-power :rng (int::sampler-rng source1) :source1 source1 :source2 source2))

(defmethod int:sample ((sampler power) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample1 (abs (* (1+ (the u:f32 (int:sample (source1 sampler) x y z w))) 0.5)))
        (sample2 (abs (* (1+ (the u:f32 (int:sample (source2 sampler) x y z w))) 0.5))))
    (1- (* (expt sample1 sample2) 2))))
