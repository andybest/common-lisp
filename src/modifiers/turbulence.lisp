(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.turbulence
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.turbulence)

(defstruct (turbulence
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (displacement-source nil :type int:sampler)
  (power 1.0 :type u:f32)
  (x1 0d0 :type u:f64)
  (x2 0d0 :type u:f64)
  (x3 0d0 :type u:f64)
  (x4 0d0 :type u:f64)
  (y1 0d0 :type u:f64)
  (y2 0d0 :type u:f64)
  (y3 0d0 :type u:f64)
  (y4 0d0 :type u:f64)
  (z1 0d0 :type u:f64)
  (z2 0d0 :type u:f64)
  (z3 0d0 :type u:f64)
  (z4 0d0 :type u:f64)
  (w1 0d0 :type u:f64)
  (w2 0d0 :type u:f64)
  (w3 0d0 :type u:f64)
  (w4 0d0 :type u:f64))

(defun mod:turbulence (source displacement &key (frequency 1.0) (power 1.0) (roughness 3))
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'turbulence
           :argument 'source
           :value source))
  (unless (typep displacement 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'turbulence
           :argument 'displacement
           :value displacement))
  (unless (realp frequency)
    (error 'int:invalid-real-argument
           :sampler-type 'turbulence
           :argument :frequency
           :value frequency))
  (unless (realp power)
    (error 'int:invalid-real-argument
           :sampler-type 'turbulence
           :argument :power
           :value power))
  (unless (typep roughness '(integer 1 32))
    (error 'int:invalid-fractal-octave-count
           :sampler-type 'turbulence
           :value roughness))
  (let ((rng (int::sampler-rng source)))
    (make-turbulence :rng (int::sampler-rng source)
                     :source source
                     :displacement-source (mod:fractalize
                                           displacement
                                           :fbm
                                           :octaves roughness
                                           :frequency frequency)
                     :power (float power 1f0)
                     :x1 (float (rng:float rng 0.0 1.0) 1d0)
                     :x2 (float (rng:float rng 0.0 1.0) 1d0)
                     :x3 (float (rng:float rng 0.0 1.0) 1d0)
                     :x4 (float (rng:float rng 0.0 1.0) 1d0)
                     :y1 (float (rng:float rng 0.0 1.0) 1d0)
                     :y2 (float (rng:float rng 0.0 1.0) 1d0)
                     :y3 (float (rng:float rng 0.0 1.0) 1d0)
                     :y4 (float (rng:float rng 0.0 1.0) 1d0)
                     :z1 (float (rng:float rng 0.0 1.0) 1d0)
                     :z2 (float (rng:float rng 0.0 1.0) 1d0)
                     :z3 (float (rng:float rng 0.0 1.0) 1d0)
                     :z4 (float (rng:float rng 0.0 1.0) 1d0)
                     :w1 (float (rng:float rng 0.0 1.0) 1d0)
                     :w2 (float (rng:float rng 0.0 1.0) 1d0)
                     :w3 (float (rng:float rng 0.0 1.0) 1d0)
                     :w4 (float (rng:float rng 0.0 1.0) 1d0))))

(defmethod int:sample ((sampler turbulence) x &optional (y 0.0) (z 0.0) (w 0.0))
  (let ((displacement (displacement-source sampler))
        (power (power sampler))
        (x1 (+ x (x1 sampler)))
        (y1 (+ y (y1 sampler)))
        (z1 (+ z (z1 sampler)))
        (w1 (+ w (w1 sampler)))
        (x2 (+ x (x2 sampler)))
        (y2 (+ y (y2 sampler)))
        (z2 (+ z (z2 sampler)))
        (w2 (+ w (w2 sampler)))
        (x3 (+ x (x3 sampler)))
        (y3 (+ y (y3 sampler)))
        (z3 (+ z (z3 sampler)))
        (w3 (+ w (w3 sampler)))
        (x4 (+ x (x4 sampler)))
        (y4 (+ y (y4 sampler)))
        (z4 (+ z (z4 sampler)))
        (w4 (+ w (w4 sampler))))
    (int:sample (source sampler)
                (+ x (* (int:sample displacement x1 y1 z1 w1) power))
                (+ y (* (int:sample displacement x2 y2 z2 w2) power))
                (+ z (* (int:sample displacement x3 y3 z3 w3) power))
                (+ w (* (int:sample displacement x4 y4 z4 w4) power)))))
