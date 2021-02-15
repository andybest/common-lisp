(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.abs
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:abs)
  (:export
   #:abs))

(in-package #:coherent-noise.modifiers.abs)

(defstruct (abs
            (:include int::sampler)
            (:constructor %abs)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler))

(defun abs (source)
  (%abs :rng (int::sampler-rng source)
        :source source))

(defmethod int::sample ((sampler abs) x &optional (y 0d0) (z 0d0) (w 0d0))
  (cl:abs (int::sample (source sampler) x y z w)))
