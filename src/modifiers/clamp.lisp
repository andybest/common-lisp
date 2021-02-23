(in-package #:cl-user)

(defpackage #:%coherent-noise.modifiers.clamp
  (:local-nicknames
   (#:int #:%coherent-noise.internal)
   (#:mod #:%coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:min #:max))

(in-package #:%coherent-noise.modifiers.clamp)

(defstruct (clamp
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (min -1f0 :type u:f32)
  (max 1f0 :type u:f32))

(defun mod:clamp (source min max)
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'clamp
           :argument 'source
           :value source))
  (unless (realp min)
    (error 'int:invalid-real-argument
           :sampler-type 'clamp
           :argument 'min
           :value min))
  (unless (realp max)
    (error 'int:invalid-real-argument
           :sampler-type 'clamp
           :argument 'max
           :value max))
  (make-clamp :rng (int::sampler-rng source)
              :source source
              :min (float min 1f0)
              :max (float max 1f0)))

(defmethod int:sample ((sampler clamp) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (u:clamp (the u:f32 (int:sample (source sampler) x y z w))
           (min sampler)
           (max sampler)))
