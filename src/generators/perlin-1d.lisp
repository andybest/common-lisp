(in-package #:cl-user)

(defpackage #:%coherent-noise.generators.perlin-1d
  (:local-nicknames
   (#:gen #:%coherent-noise.generators)
   (#:int #:%coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%coherent-noise.generators.perlin-1d)

(defstruct (gen:perlin-1d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table int::+perlin-permutation+ :type (simple-array u:ub8 (512))))

(defun gen:perlin-1d (&key seed)
  (let* ((rng (int::make-rng seed))
         (table (rng:shuffle rng int::+perlin-permutation+)))
    (make-perlin-1d :rng rng :table table)))

(defmethod int:sample ((sampler gen:perlin-1d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore y z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x)
           (let* ((h (logand hash 15))
                  (grad (1+ (logand h 7))))
             (if (zerop (logand h 8))
                 (* grad x)
                 (* (- grad) x)))))
    (declare (inline grad))
    (u:mvlet* ((table (table sampler))
               (xi xf (floor x))
               (xi (logand xi 255))
               (r1 (grad (aref table xi) xf))
               (r2 (grad (aref table (1+ xi)) (1- xf))))
      (float (* (u:lerp (int::quintic-curve xf) r1 r2) 0.25) 1f0))))
