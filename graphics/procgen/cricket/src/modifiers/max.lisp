(in-package #:cl-user)

;;;; Maximum modifier
;;;; This noise modifier takes the maximum output value of its two input samplers.

(defpackage #:%mfiano.graphics.procgen.cricket.modifiers.max
  (:local-nicknames
   (#:int #:%mfiano.graphics.procgen.cricket.internal)
   (#:mod #:%mfiano.graphics.procgen.cricket.modifiers)
   (#:u #:mfiano.misc.utils))
  (:use #:cl)
  (:shadow #:max))

(in-package #:%mfiano.graphics.procgen.cricket.modifiers.max)

(defstruct (mod:max
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:max (source1 source2)
  "Construct a sampler that, when sampled, outputs the maximum of the outputs of its two input
samplers.

`source1`: The first input sampler (required).

`source2`: The second input sampler (required)."
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'max :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'max :argument 'source2 :value source2))
  (make-max :rng (int::sampler-rng source1) :source1 source1 :source2 source2))

(defmethod int:sample ((sampler mod:max) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (cl:max (the u:f32 (int:sample (source1 sampler) x y z w))
          (the u:f32 (int:sample (source2 sampler) x y z w))))
