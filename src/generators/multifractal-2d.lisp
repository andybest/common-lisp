(in-package #:cl-user)

(defpackage #:%coherent-noise.generators.multifractal-2d
  (:local-nicknames
   (#:gen #:%coherent-noise.generators)
   (#:int #:%coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%coherent-noise.generators.multifractal-2d)

(defstruct (gen:multifractal-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (sources (vector) :type simple-vector)
  (scale 1.0 :type u:f32)
  (octaves 4 :type (integer 1 32))
  (frequency 1.0 :type u:f32)
  (lacunarity 2.0 :type u:f32)
  (persistence 0.5 :type u:f32))

(defun get-scale-factor (octaves persistence)
  (loop :for i :below octaves
        :for result = 1
          :then (+ result (* result (expt persistence i)))
        :finally (return (float result 1f0))))

(defun gen:multifractal-2d (&key seed (generator #'gen:perlin-2d) (octaves 4) (frequency 1.0)
                              (lacunarity 2.0) (persistence 0.5))
  (unless (typep octaves '(integer 1 32))
    (error 'int:invalid-fractal-octave-count :sampler-type 'multifractal-2d :value octaves))
  (unless (realp frequency)
    (error 'int:invalid-real-argument
           :sampler-type 'multifractal-2d
           :argument :frequency
           :value frequency))
  (unless (realp lacunarity)
    (error 'int:invalid-real-argument
           :sampler-type 'multifractal-2d
           :argument :lacunarity
           :value lacunarity))
  (unless (realp persistence)
    (error 'int:invalid-real-argument
           :sampler-type 'multifractal-2d
           :argument :persistence
           :value persistence))
  (let ((rng (int::make-rng seed)))
    (make-multifractal-2d :rng rng
                          :sources (int::make-fractal-sources generator rng octaves)
                          :scale (get-scale-factor octaves persistence)
                          :octaves octaves
                          :frequency (float frequency 1f0)
                          :lacunarity (float lacunarity 1f0)
                          :persistence (float persistence 1f0))))

(defmethod int:sample ((sampler gen:multifractal-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w))
  (loop :with sources = (sources sampler)
        :with frequency = (frequency sampler)
        :with lacunarity = (lacunarity sampler)
        :with persistence = (persistence sampler)
        :for i :below (octaves sampler)
        :for fx = (* x frequency) :then (* fx lacunarity)
        :for fy = (* y frequency) :then (* fy lacunarity)
        :for amplitude = 1 :then (* amplitude persistence)
        :for sample = (int:sample (aref sources i) fx fy)
        :for result = sample :then (+ result (* result sample amplitude))
        :finally (return (/ result (scale sampler)))))
