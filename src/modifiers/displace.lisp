(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.displace
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:displace))

(in-package #:coherent-noise.modifiers.displace)

(defstruct (displace
            (:include int::sampler)
            (:constructor %displace)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int::sampler)
  (x nil :type (or int::sampler null))
  (y nil :type (or int::sampler null))
  (z nil :type (or int::sampler null))
  (w nil :type (or int::sampler null)))

(defun displace (source &key x y z w)
  (%displace :rng (int::sampler-rng source)
             :source source
             :x x
             :y y
             :z z
             :w w))

(defmethod int::sample ((sampler displace) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let* ((dx (if (x sampler) (int::sample (x sampler) x y z w) 0.0))
         (dy (if (y sampler) (int::sample (y sampler) x y z w) 0.0))
         (dz (if (z sampler) (int::sample (z sampler) x y z w) 0.0))
         (dw (if (w sampler) (int::sample (w sampler) x y z w) 0.0)))
    (int::sample (source sampler) (+ x dx) (+ y dy) (+ z dz) (+ w dw))))
