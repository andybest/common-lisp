(in-package #:cl-user)

(defpackage #:coherent-noise.generators.billow-3d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.billow-3d)

(defstruct (billow-3d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (sources (vector) :type simple-vector)
  (scale 1.0 :type u:f32)
  (octaves 4 :type (integer 1 32))
  (frequency 1.0 :type u:f32)
  (lacunarity int::+default-lacunarity+ :type u:f32)
  (persistence 0.5 :type u:f32))

(defun gen:billow-3d (&key seed (generator #'gen:perlin-3d) (octaves 4) (frequency 1.0)
                        (lacunarity int::+default-lacunarity+) (persistence 0.5))
  (let ((rng (int::make-rng seed)))
    (make-billow-3d :rng rng
                    :sources (int::make-fractal-sources generator rng octaves)
                    :scale (loop :for i :below octaves :sum (expt persistence i))
                    :octaves octaves
                    :frequency (float frequency 1f0)
                    :lacunarity (float lacunarity 1f0)
                    :persistence (float persistence 1f0))))

(defmethod int:sample ((sampler billow-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w))
  (loop :with sources = (sources sampler)
        :with frequency = (frequency sampler)
        :with lacunarity = (lacunarity sampler)
        :with persistence = (persistence sampler)
        :for i :below (octaves sampler)
        :for amplitude = 1.0 :then (* amplitude persistence)
        :for fx = (* x frequency) :then (* fx lacunarity)
        :for fy = (* y frequency) :then (* fy lacunarity)
        :for fz = (* z frequency) :then (* fz lacunarity)
        :for sample = (1- (* (abs (int:sample (aref sources i) fx fy fz)) 2))
        :sum (* sample amplitude) :into result
        :finally (return (/ result (scale sampler)))))
