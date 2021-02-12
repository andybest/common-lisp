(in-package #:cl-user)

(defpackage #:coherent-noise.generators.value-3d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:value-3d))

(in-package #:coherent-noise.generators.value-3d)

(u:fn-> sample (u:ub32 int::f50 int::f50 int::f50) u:f32)
(declaim (inline sample))
(defun sample (seed x y z)
  (declare (optimize speed))
  (labels ((in-range (x)
             (logand x (1- (expt 2 32))))
           (coord (seed x y z)
             (let ((hash (expt (in-range (* (logxor seed x y z) 668265261)) 2)))
               (* (in-range (logxor hash (ash hash -19)))
                  (/ 2147483648.0)))))
    (declare (inline in-range coord))
    (u:mvlet* ((x0 xs (floor x))
               (xs (int::interpolate/cubic xs))
               (x0 (in-range (* x0 int::+prime-x+)))
               (x1 (+ x0 int::+prime-x+))
               (y0 ys (floor y))
               (ys (int::interpolate/cubic ys))
               (y0 (in-range (* y0 int::+prime-y+)))
               (y1 (+ y0 int::+prime-y+))
               (z0 zs (floor z))
               (zs (int::interpolate/cubic zs))
               (z0 (in-range (* z0 int::+prime-z+)))
               (z1 (+ z0 int::+prime-z+)))
      (float
       (1- (u:lerp zs
                   (u:lerp ys
                           (u:lerp xs (coord seed x0 y0 z0) (coord seed x1 y0 z0))
                           (u:lerp xs (coord seed x0 y1 z0) (coord seed x1 y1 z0)))
                   (u:lerp ys
                           (u:lerp xs (coord seed x0 y0 z1) (coord seed x1 y0 z1))
                           (u:lerp xs (coord seed x0 y1 z1) (coord seed x1 y1 z1)))))
       1f0))))

(defun value-3d (&key (seed "default"))
  (u:mvlet ((rng seed (int::make-rng seed)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (sample seed x y z))))
