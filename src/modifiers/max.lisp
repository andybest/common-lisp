(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.max
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:max)
  (:export
   #:max))

(in-package #:coherent-noise.modifiers.max)

(defstruct (max
            (:include int::sampler)
            (:constructor %max)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int::sampler)
  (source2 nil :type int::sampler))

(defun max (source1 source2)
  (%max :rng (int::sampler-rng source1)
        :source1 source1
        :source2 source2))

(defmethod int::sample ((sampler max) x &optional (y 0d0) (z 0d0) (w 0d0))
  (cl:max (int::sample (source1 sampler) x y z w)
          (int::sample (source2 sampler) x y z w)))
