(in-package #:cl-user)

(defpackage #:coherent-noise.generators.checker-2d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:checker-2d))

(in-package #:coherent-noise.generators.checker-2d)

(defstruct (checker-2d
            (:include int::sampler)
            (:constructor %checker-2d)
            (:conc-name "")
            (:predicate nil)
            (:copier nil)))

(defun checker-2d (&key seed)
  (%checker-2d :rng (int::make-rng seed)))

(defmethod int::sample ((sampler checker-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (if (zerop (logxor (logand (floor x) 1) (logand (floor y) 1) (logand (floor z) 1)))
      1.0
      -1.0))
