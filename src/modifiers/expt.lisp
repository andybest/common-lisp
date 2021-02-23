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
  (power 1.0 :type u:f32)
  (symmetric-p nil :type boolean))

(defun mod:expt (source power &key symmetric-p)
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'expt
           :argument 'source
           :value source))
  (unless (realp power)
    (error 'int:invalid-real-argument
           :sampler-type 'expt
           :argument 'power
           :value power))
  (make-expt :rng (int::sampler-rng source)
             :source source
             :power (float power 1f0)
             :symmetric-p symmetric-p))

(defmethod int:sample ((sampler expt) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample (the u:f32 (int:sample (source sampler) x y z w))))
    (if (symmetric-p sampler)
        (* (signum sample) (cl:expt (abs sample) (power sampler)))
        (1- (* (cl:expt (abs (* (1+ sample) 0.5)) (power sampler)) 2)))))
