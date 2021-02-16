(in-package #:cl-user)

(defpackage #:coherent-noise.generators.perlin-3d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.perlin-3d)

(defstruct (perlin-3d
            (:include int:sampler)
            (:constructor %perlin-3d)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table int::+perlin-permutation+ :type (simple-array u:ub8 (512))))

(defun gen:perlin-3d (&key seed)
  (let* ((rng (int::make-rng seed))
         (table (rng:shuffle rng int::+perlin-permutation+)))
    (%perlin-3d :rng rng :table table)))

(defmethod int:sample ((sampler perlin-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((noise (hash x y z)
           (let* ((h (logand hash 15))
                  (u (if (< h 8) x y))
                  (v (case h
                       ((0 1 2 3) y)
                       ((12 14) x)
                       (t z))))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (declare (inline noise))
    (u:mvlet* ((table (table sampler))
               (xi xf (truncate x))
               (yi yf (truncate y))
               (zi zf (truncate z))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (u (int::quintic-curve xf))
               (v (int::quintic-curve yf))
               (w (int::quintic-curve zf))
               (a (+ (aref table xi) yi))
               (b (+ (aref table (1+ xi)) yi)))
      (float
       (u:lerp
        w
        (u:lerp
         v
         (u:lerp u
                 (noise (int::lookup table zi a) xf yf zf)
                 (noise (int::lookup table zi b) (1- xf) yf zf))
         (u:lerp u
                 (noise (int::lookup table zi (1+ a)) xf (1- yf) zf)
                 (noise (int::lookup table zi (1+ b)) (1- xf) (1- yf) zf)))
        (u:lerp
         v
         (u:lerp u
                 (noise (int::lookup table (1+ zi) a) xf yf (1- zf))
                 (noise (int::lookup table (1+ zi) b) (1- xf) yf (1- zf)))
         (u:lerp u
                 (noise (int::lookup table (1+ zi) (1+ a)) xf (1- yf) (1- zf))
                 (noise (int::lookup table zi (1+ b)) (1- xf) (1- yf) (1- zf)))))
       1f0))))
