(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.multiply
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.multiply)

(defstruct (multiply
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:* (source1 source2)
  (make-multiply :rng (int::sampler-rng source1)
                 :source1 source1
                 :source2 source2))

(defmethod int:sample ((sampler multiply) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (* (the u:f32 (int:sample (source1 sampler) x y z w))
     (the u:f32 (int:sample (source2 sampler) x y z w))))
