(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.expt
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow #:expt))

(in-package #:coherent-noise.modifiers.expt)

(defstruct (expt
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (power 1.0 :type u:f32))

(defun mod:expt (source power)
  (make-expt :rng (int::sampler-rng source)
             :source source
             :power (float power 1f0)))

(defmethod int:sample ((sampler expt) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample (the u:f32 (int:sample (source sampler) x y z w))))
    (1- (* (cl:expt (abs (* (1+ sample) 0.5)) (power sampler)) 2))))
