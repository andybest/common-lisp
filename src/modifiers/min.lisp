(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.min
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:min)
  (:export
   #:min))

(in-package #:coherent-noise.modifiers.min)

(defstruct (min
            (:include int::sampler)
            (:constructor %min)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int::sampler)
  (source2 nil :type int::sampler))

(defun min (source1 source2)
  (%min :rng (int::sampler-rng source1)
        :source1 source1
        :source2 source2))

(defmethod int::sample ((sampler min) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (cl:min (the u:f32 (int::sample (source1 sampler) x y z w))
          (the u:f32 (int::sample (source2 sampler) x y z w))))
