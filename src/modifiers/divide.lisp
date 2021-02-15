(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.divide
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:/)
  (:export
   #:/))

(in-package #:coherent-noise.modifiers.divide)

(defstruct (divide
            (:include int::sampler)
            (:constructor divide)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int::sampler)
  (source2 nil :type int::sampler))

(defun / (source1 source2)
  (divide :rng (int::sampler-rng source1)
          :source1 source1
          :source2 source2))

(defmethod int::sample ((sampler divide) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample1 (the u:f32 (int::sample (source1 sampler) x y z w)))
        (sample2 (the u:f32 (int::sample (source2 sampler) x y z w))))
    (if (zerop sample2) 0.0 (cl:/ sample1 sample2))))
