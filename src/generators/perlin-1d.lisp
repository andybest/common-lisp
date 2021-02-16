(in-package #:cl-user)

(defpackage #:coherent-noise.generators.perlin-1d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.perlin-1d)

(defstruct (perlin-1d
            (:include int::sampler)
            (:constructor %perlin-1d)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table int::+perlin-permutation+ :type (simple-array u:ub8 (512))))

(defun gen:perlin-1d (&key seed)
  (let* ((rng (int::make-rng seed))
         (table (rng:shuffle rng int::+perlin-permutation+)))
    (%perlin-1d :rng rng :table table)))

(defmethod int:sample ((sampler perlin-1d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore y z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((noise (hash x)
           (let* ((h (logand hash 15))
                  (grad (1+ (logand h 7))))
             (if (zerop (logand h 8))
                 (* grad x)
                 (* (- grad) x)))))
    (declare (inline noise))
    (u:mvlet* ((table (table sampler))
               (xi xf (truncate x))
               (xi (logand xi 255)))
      (float (* (u:lerp (int::interpolate/quintic xf)
                        (noise (aref table xi) xf)
                        (noise (aref table (1+ xi)) (1- xf)))
                0.25)
             1f0))))
