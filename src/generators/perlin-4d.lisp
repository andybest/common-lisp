(in-package #:cl-user)

(defpackage #:coherent-noise.generators.perlin-4d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:perlin-4d))

(in-package #:coherent-noise.generators.perlin-4d)

(u:fn-> %sample ((simple-array u:ub8 (512)) int::f50 int::f50 int::f50 int::f50) u:f32)
(declaim (inline %sample))
(defun %sample (table x y z w)
  (declare (optimize speed))
  (flet ((grad (hash x y z w)
           (let* ((h (logand hash 31))
                  (u (if (< h 24) x y))
                  (v (if (< h 16) y z))
                  (w (if (< h 8) z w)))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))
                (if (zerop (logand h 4)) w (- w))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (zi zf (truncate z))
               (wi wf (truncate w))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (wi (logand wi 255))
               (xi1 (logand (1+ xi) 255))
               (yi1 (logand (1+ yi) 255))
               (zi1 (logand (1+ zi) 255))
               (wi1 (logand (1+ wi) 255))
               (xf-1 (1- xf))
               (yf-1 (1- yf))
               (zf-1 (1- zf))
               (wf-1 (1- wf))
               (fs (int::interpolate/quintic xf))
               (ft (int::interpolate/quintic yf))
               (fr (int::interpolate/quintic zf))
               (fq (int::interpolate/quintic wf)))
      (float
       (u:lerp
        fs
        (u:lerp
         ft
         (u:lerp
          fr
          (u:lerp fq
                  (grad (int::lookup table xi yi zi wi) xf yf zf wf)
                  (grad (int::lookup table xi yi zi wi1) xf yf zf wf-1))
          (u:lerp fq
                  (grad (int::lookup table xi yi zi1 wi) xf yf zf-1 wf)
                  (grad (int::lookup table xi yi zi1 wi1) xf yf zf-1 wf-1)))
         (u:lerp
          fr
          (u:lerp fq
                  (grad (int::lookup table xi yi1 zi wi) xf yf-1 zf wf)
                  (grad (int::lookup table xi yi1 zi wi1) xf yf-1 zf wf-1))
          (u:lerp fq
                  (grad (int::lookup table xi yi1 zi1 wi) xf yf-1 zf-1 wf)
                  (grad (int::lookup table xi yi1 zi1 wi1) xf yf-1 zf-1 wf-1))))
        (u:lerp
         ft
         (u:lerp
          fr
          (u:lerp fq
                  (grad (int::lookup table xi1 yi zi wi) xf-1 yf zf wf)
                  (grad (int::lookup table xi1 yi zi wi1) xf-1 yf zf wf-1))
          (u:lerp fq
                  (grad (int::lookup table xi1 yi zi1 wi) xf-1 yf zf-1 wf)
                  (grad (int::lookup table xi1 yi zi1 wi1) xf-1 yf zf-1 wf-1)))
         (u:lerp
          fr
          (u:lerp fq
                  (grad (int::lookup table xi1 yi1 zi wi) xf-1 yf-1 zf wf)
                  (grad (int::lookup table xi1 yi1 zi wi1) xf-1 yf-1 zf wf-1))
          (u:lerp fq
                  (grad (int::lookup table xi1 yi1 zi1 wi) xf-1 yf-1 zf-1 wf)
                  (grad (int::lookup table xi1 yi1 zi1 wi1) xf-1 yf-1 zf-1 wf-1)))))
       1f0))))

(defun perlin-4d (&key (seed "default"))
  (let* ((rng (int::make-rng seed))
         (table (rng:shuffle rng int::+perlin/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (%sample table x y z w))))
