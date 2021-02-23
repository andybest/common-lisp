(in-package #:cl-user)

(defpackage #:%coherent-noise.generators.constant
  (:local-nicknames
   (#:gen #:%coherent-noise.generators)
   (#:int #:%coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%coherent-noise.generators.constant)

(defstruct (constant
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (value 0.5 :type u:f32))

(defun gen:constant (value &key seed)
  (unless (realp value)
    (error 'int:invalid-real-argument
           :sampler-type 'constant
           :argument 'value
           :value value))
  (make-constant :rng (int::make-rng seed)
                 :value (u:lerp (u:clamp (float value 1f0) 0.0 1.0) -1.0 1.0)))

(defmethod int:sample ((sampler constant) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore x y z w)
           (optimize speed)
           (int::f50 x y z w))
  (value sampler))
