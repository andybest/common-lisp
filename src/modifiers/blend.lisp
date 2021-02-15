(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.blend
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:blend))

(in-package #:coherent-noise.modifiers.blend)

(defstruct (blend
            (:include int::sampler)
            (:constructor %blend)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int::sampler)
  (source2 nil :type int::sampler)
  (control nil :type int::sampler))

(defun blend (source1 source2 control)
  (%blend :rng (int::sampler-rng source1)
          :source1 source1
          :source2 source2
          :control control))

(defmethod int::sample ((sampler blend) x &optional (y 0d0) (z 0d0) (w 0d0))
  (u:lerp (int::sample (control sampler) x y z w)
          (int::sample (source1 sampler) x y z w)
          (int::sample (source2 sampler) x y z w)))
