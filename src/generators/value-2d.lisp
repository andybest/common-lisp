(in-package #:cl-user)

(defpackage #:coherent-noise.generators.value-2d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:value-2d))

(in-package #:coherent-noise.generators.value-2d)

(defstruct (value-2d
            (:include int::sampler)
            (:constructor %value-2d)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (seed 0 :type u:ub32))

(defun value-2d (&key seed)
  (u:mvlet ((rng seed (int::make-rng seed)))
    (%value-2d :rng rng
               :seed seed)))

(defmethod int::sample ((sampler value-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w)
           (optimize speed)
           (int::f50 x y z w))
  (labels ((in-range (x)
             (logand x #.(1- (expt 2 32))))
           (coord (seed x y)
             (let ((hash (expt (in-range (* (logxor seed x y) 668265261)) 2)))
               (* (in-range (logxor hash (ash hash -19)))
                  (/ 2147483648.0)))))
    (declare (inline in-range coord))
    (u:mvlet* ((seed (seed sampler))
               (x0 xs (floor x))
               (xs (int::interpolate/cubic xs))
               (x0 (in-range (* x0 int::+prime-x+)))
               (x1 (+ x0 int::+prime-x+))
               (y0 ys (floor y))
               (ys (int::interpolate/cubic ys))
               (y0 (in-range (* y0 int::+prime-y+)))
               (y1 (+ y0 int::+prime-y+)))
      (float (1- (u:lerp ys
                         (u:lerp xs (coord seed x0 y0) (coord seed x1 y0))
                         (u:lerp xs (coord seed x0 y1) (coord seed x1 y1))))
             1f0))))
