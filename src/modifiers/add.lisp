(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.add
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:+)
  (:export
   #:+))

(in-package #:coherent-noise.modifiers.add)

(defstruct (add
            (:include int::sampler)
            (:constructor add)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int::sampler)
  (source2 nil :type int::sampler))

(defun + (source1 source2)
  (add :rng (int::sampler-rng source1)
       :source1 source1
       :source2 source2))

(defmethod int::sample ((sampler add) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (cl:+ (the u:f32 (int::sample (source1 sampler) x y z w))
        (the u:f32 (int::sample (source2 sampler) x y z w))))
