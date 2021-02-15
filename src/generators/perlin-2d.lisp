(in-package #:cl-user)

(defpackage #:coherent-noise.generators.perlin-2d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:perlin-2d))

(in-package #:coherent-noise.generators.perlin-2d)

(defstruct (perlin-2d
            (:include int::sampler)
            (:constructor %perlin-2d)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table int::+perlin-permutation+ :type (simple-array u:ub8 (512))))

(defun perlin-2d (&key seed)
  (let* ((rng (int::make-rng seed))
         (table (rng:shuffle rng int::+perlin-permutation+)))
    (%perlin-2d :rng rng :table table)))

(defmethod int::sample ((sampler perlin-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x y)
           (let* ((h (logand hash 7))
                  (u (if (< h 4) x y))
                  (v (if (< h 4) y x)))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (u:mvlet* ((table (table sampler))
               (xi xf (truncate x))
               (yi yf (truncate y))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (u (int::interpolate/quintic xf))
               (a (+ (aref table xi) yi))
               (b (+ (aref table (1+ xi)) yi)))
      (float
       (u:lerp (int::interpolate/quintic yf)
               (u:lerp u
                       (grad (int::lookup table (aref table a)) xf yf)
                       (grad (int::lookup table (aref table b)) (1- xf) yf))
               (u:lerp u
                       (grad (int::lookup table (aref table (1+ a))) xf (1- yf))
                       (grad (int::lookup table (aref table (1+ b))) (1- xf) (1- yf))))
       1f0))))
