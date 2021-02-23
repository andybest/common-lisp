(in-package #:cl-user)

(defpackage #:coherent-noise.generators.spheres-3d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.spheres-3d)

(defstruct (spheres-3d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (frequency 1d0 :type u:f64))

(defun gen:spheres-3d (&key seed (frequency 1d0))
  (unless (realp frequency)
    (error 'int:invalid-real-argument
           :sampler-type 'spheres-3d
           :argument :frequency
           :value frequency))
  (make-spheres-3d :rng (int::make-rng seed)
                   :frequency (float frequency 1d0)))

(defmethod int:sample ((sampler spheres-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (let* ((frequency (frequency sampler))
         (x (* x frequency))
         (y (* y frequency))
         (z (* z frequency))
         (distance-center (the int::f50 (sqrt (+ (* x x) (* y y) (* z z)))))
         (distance-small (- distance-center (floor distance-center)))
         (distance-large (- 1 distance-small))
         (nearest (min distance-small distance-large)))
    (float (- 1.0 (* nearest 4.0)) 1f0)))
