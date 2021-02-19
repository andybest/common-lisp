(in-package #:cl-user)

(defpackage #:coherent-noise.generators.hybrid-multifractal-2d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.hybrid-multifractal-2d)

(defstruct (hybrid-multifractal-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (sources (vector) :type simple-vector)
  (scale 1.0 :type u:f32)
  (octaves 4 :type (integer 1 32))
  (frequency 1.0 :type u:f32)
  (lacunarity int::+default-lacunarity+ :type u:f32)
  (persistence 0.25 :type u:f32))

(defun get-scale-factor (octaves persistence)
  (loop :with persistence = persistence
        :with result = persistence
        :for i :from 1 :below octaves
        :for amplitude = persistence :then (* amplitude persistence)
        :for weight = result :then (max weight 1)
        :for sample = amplitude
        :do (setf weight (* weight sample))
            (incf result weight)
        :finally (return result)))

(defun gen:hybrid-multifractal-2d (&key seed (generator #'gen:perlin-2d) (octaves 4) (frequency 1.0)
                                     (lacunarity int::+default-lacunarity+) (persistence 0.25))
  (let ((rng (int::make-rng seed)))
    (make-hybrid-multifractal-2d :rng rng
                                 :sources (int::make-fractal-sources generator rng octaves)
                                 :scale (get-scale-factor octaves persistence)
                                 :octaves octaves
                                 :frequency (float frequency 1f0)
                                 :lacunarity (float lacunarity 1f0)
                                 :persistence (float persistence 1f0))))

(defmethod int:sample ((sampler hybrid-multifractal-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w))
  (loop :with sources = (sources sampler)
        :with frequency = (frequency sampler)
        :with lacunarity = (lacunarity sampler)
        :with persistence = (persistence sampler)
        :with fx = (* x frequency)
        :with fy = (* y frequency)
        :with result = (* (int:sample (aref sources 0) fx fy) persistence)
        :for i :from 1 :below (octaves sampler)
        :for amplitude = persistence :then (* amplitude persistence)
        :for weight = result :then (max weight 1)
        :for lx = (* fx lacunarity)
        :for ly = (* fy lacunarity)
        :for sample = (* (int:sample (aref sources i) lx ly) amplitude)
        :do (setf weight (* weight sample))
            (incf result weight)
        :finally (return (/ result (scale sampler)))))
