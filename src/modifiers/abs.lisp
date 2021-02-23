(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.abs
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:abs))

(in-package #:coherent-noise.modifiers.abs)

(defstruct (abs
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler))

(defun mod:abs (source)
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'abs
           :argument 'source
           :value source))
  (make-abs :rng (int::sampler-rng source) :source source))

(defmethod int:sample ((sampler abs) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (cl:abs (the u:f32 (int:sample (source sampler) x y z w))))
