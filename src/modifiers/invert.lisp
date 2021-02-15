(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.invert
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:invert))

(in-package #:coherent-noise.modifiers.invert)

(defstruct (invert
            (:include int::sampler)
            (:constructor %invert)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler))

(defun invert (source)
  (%invert :rng (int::sampler-rng source)
           :source source))

(defmethod int::sample ((sampler invert) x &optional (y 0d0) (z 0d0) (w 0d0))
  (- (int::sample (source sampler) x y z w)))
