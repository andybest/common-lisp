(in-package #:cl-user)

(defpackage #:coherent-noise.generators.misc
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:checkered
   #:constant
   #:cylinders
   #:spheres))

(in-package #:coherent-noise.generators.misc)

(u:fn-> %cylinders (u:f32 int::f50 int::f50) u:f32)
(defun %cylinders (frequency x z)
  (declare (optimize speed))
  (let* ((x (* x frequency))
         (z (* z frequency))
         (distance-center (the int::f50 (sqrt (+ (* x x) (* z z)))))
         (distance-small (- distance-center (floor distance-center)))
         (distance-large (- 1 distance-small))
         (nearest (min distance-small distance-large)))
    (float (- 1.0 (* nearest 4.0)) 1f0)))

(defun cylinders (&key (frequency 1.0))
  (lambda (x &optional y (z 0d0) w)
    (declare (ignore y w))
    (%cylinders frequency x z)))

(u:fn-> %spheres (u:f32 int::f50 int::f50 int::f50) u:f32)
(defun %spheres (frequency x y z)
  (declare (optimize speed))
  (let* ((x (* x frequency))
         (y (* y frequency))
         (z (* z frequency))
         (distance-center (the int::f50 (sqrt (+ (* x x) (* y y) (* z z)))))
         (distance-small (- distance-center (floor distance-center)))
         (distance-large (- 1 distance-small))
         (nearest (min distance-small distance-large)))
    (float (- 1.0 (* nearest 4.0)) 1f0)))

(defun spheres (&key (frequency 1.0))
  (lambda (x &optional (y 0d0) (z 0d0) w)
    (declare (ignore w))
    (%spheres frequency x y z)))

(u:fn-> %checkered (int::f50 int::f50 int::f50) u:f32)
(defun %checkered (x y z)
  (declare (optimize speed))
  (if (zerop (logxor (logand (floor x) 1) (logand (floor y) 1) (logand (floor z) 1)))
      1.0
      -1.0))

(defun checkered ()
  (lambda (x &optional (y 0d0) (z 0d0) w)
    (declare (ignore w))
    (%checkered x y z)))

(defun constant (value)
  (constantly (u:lerp (u:clamp value 0 1) -1.0 1.0)))
