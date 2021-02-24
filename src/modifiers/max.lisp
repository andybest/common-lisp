(in-package #:cl-user)

(defpackage #:%coherent-noise.modifiers.max
  (:local-nicknames
   (#:int #:%coherent-noise.internal)
   (#:mod #:%coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:max))

(in-package #:%coherent-noise.modifiers.max)

(defstruct (mod:max
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:max (source1 source2)
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'max :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'max :argument 'source2 :value source2))
  (make-max :rng (int::sampler-rng source1) :source1 source1 :source2 source2))

(defmethod int:sample ((sampler mod:max) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (cl:max (the u:f32 (int:sample (source1 sampler) x y z w))
          (the u:f32 (int:sample (source2 sampler) x y z w))))
