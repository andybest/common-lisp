(in-package #:cl-user)

(defpackage #:coherent-noise.generators.perlin-3d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:perlin-3d))

(in-package #:coherent-noise.generators.perlin-3d)

(u:fn-> %sample ((simple-array u:ub8 (512)) int::f50 int::f50 int::f50) u:f32)
(declaim (inline %sample))
(defun %sample (table x y z)
  (declare (optimize speed))
  (flet ((grad (hash x y z)
           (let* ((h (logand hash 15))
                  (u (if (< h 8) x y))
                  (v (case h
                       ((0 1 2 3) y)
                       ((12 14) x)
                       (t z))))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (zi zf (truncate z))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (u (int::interpolate/quintic xf))
               (v (int::interpolate/quintic yf))
               (w (int::interpolate/quintic zf))
               (a (+ (aref table xi) yi))
               (b (+ (aref table (1+ xi)) yi)))
      (float
       (u:lerp
        w
        (u:lerp
         v
         (u:lerp u
                 (grad (int::lookup table zi a) xf yf zf)
                 (grad (int::lookup table zi b) (1- xf) yf zf))
         (u:lerp u
                 (grad (int::lookup table zi (1+ a)) xf (1- yf) zf)
                 (grad (int::lookup table zi (1+ b)) (1- xf) (1- yf) zf)))
        (u:lerp
         v
         (u:lerp u
                 (grad (int::lookup table (1+ zi) a) xf yf (1- zf))
                 (grad (int::lookup table (1+ zi) b) (1- xf) yf (1- zf)))
         (u:lerp u
                 (grad (int::lookup table (1+ zi) (1+ a)) xf (1- yf) (1- zf))
                 (grad (int::lookup table zi (1+ b)) (1- xf) (1- yf) (1- zf)))))
       1f0))))

(defun perlin-3d (&key (seed "default"))
  (let* ((rng (int::make-rng seed))
         (table (rng:shuffle rng int::+perlin/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (%sample table x y z))))
