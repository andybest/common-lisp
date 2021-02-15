(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.clamp
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:min #:max)
  (:export
   #:clamp))

(in-package #:coherent-noise.modifiers.clamp)

(defstruct (clamp
            (:include int::sampler)
            (:constructor %clamp)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (min -1d0 :type u:f64)
  (max 1d0 :type u:f64))

(defun clamp (source min max)
  (%clamp :rng (int::sampler-rng source)
          :source source
          :min min
          :max max))

(defmethod int::sample ((sampler clamp) x &optional (y 0d0) (z 0d0) (w 0d0))
  (u:clamp (int::sample (source sampler) x y z w)
           (min sampler)
           (max sampler)))
